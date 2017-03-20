;;; perspective.el --- switch between named "perspectives" of the editor

;; Copyright (C) 2008-2015 Natalie Weizenbaum <nex342@gmail.com>
;;
;; Licensed under the same terms as Emacs and under the MIT license.

;; Author original : Natalie Weizenbaum <nex342@gmail.com>
;; URL: http://github.com/nex3/perspective-el
;; Package-Version: 20160609.1444
;; Package-Requires: ((cl-lib "0.5"))
;; Version: 1.12
;; Created: 2008-03-05
;; By: Natalie Weizenbaum <nex342@gmail.com>
;; Keywords: workspace, convenience, frames

;;; Commentary:

;; A much simpler rewrite for perspective.

;; From external user perspective is still a set of buffers, a windows
;; configuration and a collection of local variable binding.

;; However the list of buffers is no longer a raw list that is expanded on
;; buffer switch or on user explicit demand.  Instead this list is dynamically
;; evaluated from a set of rules specific to the perspective.  For instance, its
;; possible to define a perspective whose buffers all share the same
;; directory root or with a pattern in their name.  These rules can be combined
;; and a scoring system allows to define the weighting of each rules.  It
;; thus become possible to define arbitrarily complex rules.  A set of standard
;; rule builder function is provided for convenience.
;; The scoring system is also used to define which perspective a buffer is the
;; more strongly attached to and allows to define perspective switching rules
;; that are dynamic, arbitrarily complex and yet fully predictible.
;;
;; This new implementation is much more robust that the previous one that relied
;; on buffer switch hook.  In addition, it requires no user input to work and
;; it does so predicatabilily.  It can be easily customized to fit any workflow.

(require 'dash)
(require 's)
(require 'ht)
;;; Code:

(defgroup vview-mode 'nil
  "Customization for vview mode"
  :group 'frames)

(defcustom vview-default-name "default"
  "Name used for the initial perspective when enabling `vview-mode'."
  :type 'string
  :group 'vview-mode)

(defvar vview-views (ht-create)
  "A hash containing all vview indexed by their name.")

(defvar vview-history (list)
  "A list containing the name of the registered vview in `vview-views'.
The list is ordered by activation history: first element is the current view")

(defvar vview-local-vars (list)
  "A list containing all symbols name that should be local to a view.")

(defun vview-rule-buffer-owned-default-p (buffer)
  "Default predicate for a vview rule BUFFER predicate.
Always evaluate to nil."
  nil)

;; Collection of internal helper function
(defun vview--make-rule-name (pattern)
  "Return a function checking if a buffer name match PATTERN."
  (if (stringp pattern)
      `(lambda (buffer)
         (s-matches-p ,pattern (buffer-name buffer)))
    #'vview-rule-buffer-owned-default-p))

(defun vview--make-rule-path (path)
  "Return a function checking if a buffer directory match PATH."
  (if (stringp path)
      `(lambda (buffer)
         (s-matches-p ,path
                      (buffer-local-value 'default-directory buffer)))
    #'vview-rule-buffer-owned-default-p))

(defun vview--make-rule-mode (mode)
  "Return a function checking if a buffer directory major mode is MODE."
  `(lambda (buffer)
     (equal ',mode (buffer-local-value 'major-mode buffer))))

(defun vview--validate-score (unchecked-score)
  "Check that UNCHECKED-SCORE is a score or return a nil score instead."
  (if (integerp unchecked-score)
      unchecked-score
    0))

;; Structure for a virtual view buffer rule.
;; All rules have the same structure and must define a rule-id, a score value
;; and a predicate taking a buffer in parameter.
;; Several predefined constructor are available for the most common rules.
(cl-defstruct
    (vview-rule
     (:constructor nil)
     (:constructor vview-rule-name-new
                   (pattern &optional
                            (unchecked-score 1)
                            &aux
                            (buffer-owned-p (vview--make-rule-name pattern))
                            (score (vview--validate-score unchecked-score))
                            (rule-id (format "Rule name [%s], score [%s]" pattern score))))
     (:constructor vview-rule-mode-new
                   (mode &optional
                         (unchecked-score 1)
                         &aux
                         (buffer-owned-p (vview--make-rule-mode mode))
                         (score (vview--validate-score unchecked-score))
                         (rule-id (format "Rule mode [%s], score [%s]" mode score))))
     (:constructor vview-rule-path-new
                   (path &optional
                         (unchecked-score 1)
                         &aux
                         (buffer-owned-p (vview--make-rule-path path))
                         (score (vview--validate-score unchecked-score))
                         (rule-id (format "Rule path [%s], score [%s]" path score)))))
  (rule-id "empty vview rule")
  (score 0)
  (buffer-owned-p #'vview-rule-buffer-owned-p))

;; Structure for a virtual view.
;; It's composed of a set of rules that together defines if a buffer is part of
;; the view.  It also holds persp-like data such as persp local var binding,
;; points location and windows configuration.
(cl-defstruct
    (vview-view
     (:constructor nil)
     (:constructor vview-new
                   (name
                    &key (score 0) (names (list)) (paths (list)) (modes (list))
                    &aux
                    (score (vview--validate-score score))

                    (rules (-concat
                            (when paths (--map (apply 'vview-rule-path-new (-flatten (list it))) paths))
                            (when names (--map (apply 'vview-rule-name-new (-flatten (list it))) names))
                            (when modes (--map (apply 'vview-rule-mode-new (-flatten (list it))) modes)))))))
  name
  (rules (list))
  (score 0)
  (local-vars (ht-create))
  window-configuration
  point-marker)

;; vview functions
(defun vview-score-buffer (a-vview a-buffer)
  "Return a score for A-VVIEW applied to A-BUFFER."
  (if (and (vview-view-p a-vview)
           (bufferp a-buffer))
      (-sum (--map
             (if (funcall (vview-rule-buffer-owned-p it) a-buffer)
                 (vview-rule-score it)
               0)
             (vview-view-rules a-vview)))
    0))

(defun vview-buffer-owned-p (a-vview a-buffer)
  "Return t if A-VVIEW owns A-BUFFER."
  (> (vview-score-buffer a-vview a-buffer) 0))

(defun vview-buffer-list (a-vview)
  "Return a list of buffer for A-VVIEW."
  (if (vview-view-p a-vview)
      (-filter (lambda (buffer) (vview-buffer-owned-p a-vview buffer)) (buffer-list))
    (list)))

(defun vview-< (vview1 vview2)
  "Return t if score of VVIEW1 is strictly inferior to VVIEW2."
  (if (and (vview-view-p vview1)
           (vview-view-p vview2))
      (< (vview-view-score vview1) (vview-view-score vview2))
    (error "%s or %s are not vviews" vview1 vview2)))

(defun vview-> (vview1 vview2)
  "Return t if score of VVIEW1 is strictly superior to VVIEW2."
  (if (and (vview-view-p vview1)
           (vview-view-p vview2))
      (> (vview-view-score vview1) (vview-view-score vview2))
    (error "%s or %s are not vviews" vview1 vview2)))

(defun vview-save-window-conf (a-vview)
  "Save in A-VVIEW the current window configuration"
  (setf (vview-view-window-configuration a-vview) (current-window-configuration))
  (setf (vview-view-point-marker a-vview) (point-marker)))

(defun vview-load-window-conf (a-vview)
  "Save in A-VVIEW the current window configuration"
  (set-window-configuration (vview-view-window-configuration a-vview))
  (goto-char (vview-view-point-marker a-vview)))

(defun vview-save-local-vars (a-vview)
  "Save in A-VVIEW all view local vars according to their current global values."
  (-each vview-local-vars
    (lambda (symbol)
      (ht-set! (vview-view-local-vars a-vview)
               symbol
               (symbol-value symbol)))))

(defun vview-load-local-vars (a-vview)
  "Load all view local vars from A-VVIEW."
  (ht-each (lambda (key value)
             (set key value))
           (vview-view-local-vars a-vview)))

;; vviews management function
(defun vview-make-var-local (symbol)
  "Make SYMBOL vview local."
  (add-to-list 'vview-local-vars symbol))

(defun vview-register-view (a-vview)
  "Register A-VVIEW into `vview-views'.
In addition, save in A-VVIEW the current local variable and create a virgin
window configuration."
  (when (vview-view-p a-vview)
    (let ((this-vview-name (vview-view-name a-vview)))
      (when (not (ht-contains-p vview-views this-vview-name)))
      (add-to-list 'vview-history this-vview-name t)
      (vview-save-local-vars a-vview)
      (save-window-excursion
        (switch-to-buffer "*scratch*")
        (funcall initial-major-mode)
        (delete-other-windows)
        (vview-save-window-conf a-vview))
      (ht-set! vview-views this-vview-name a-vview))))

(defun vview-unregister-view (vview-name)
  "Unregister a view with name VVIEW-NAME from`vview-views'."
  (when (and (stringp vview-name)
             (ht-contains-p vview-views vview-name))
    (setq vview-history (-remove-item vview-name vview-history))
    (ht-remove! vview-views vview-name)))

(defun vview-switch-view (vview-name)
  "Switch to a vview with name VVIEW-NAME."
  (if (ht-contains-p vview-views vview-name)
      (progn
        ;; current is the view before switch
        (vview-save-local-vars (vview-current-view))
        (vview-save-window-conf (vview-current-view))
        (push vview-name vview-history)
        (delete-dups vview-history)
        ;; current is the new view
        (vview-load-local-vars (vview-current-view))
        (vview-load-window-conf (vview-current-view)))
    (error "%s is not a known view" vview-name)))

(defun vview-get-history-rank (a-vview)
  "Return the place in `vview-history' for A-VVIEW."
  (if (vview-view-p a-vview)
      (-elem-index (vview-view-name a-vview) vview-history)
    most-positive-fixnum))

(defun vview-get-best-view (buffer)
  "Return the best view which owns BUFFER.
The best view is the one which owns BUFFER and have the best score.  If several
meet the same criteria, the last visited one takes precdence.
If no view owns BUFFER, current view is returned."
  (let* ((owning-views (-filter (lambda (a-vview)
                                  (vview-buffer-owned-p a-vview buffer))
                                (ht-values vview-views)))
         (best-score (vview-view-score (-max-by #'vview-> owning-views)))
         (best-owning-views (-filter (lambda (a-vview)
                                       (eq best-score (vview-view-score a-vview)))
                                     owning-views))
         (best-owning-view (-max-by (lambda (vview1 vview2)
                                      (< (vview-get-history-rank vview1) (vview-get-history-rank vview2)))
                                    best-owning-views)))
    (if best-owning-view
        best-owning-view
      (vview-current-view))))

;; functions for current virtual views
(defun vview-current-buffer-list ()
  "Return a list of buffer for current view."
  (if (> (length vview-history) 0)
      (vview-buffer-list (ht-get vview-views (first vview-history)))
    (buffer-list)))

(defun vview-current-buffer-name-list ()
  "Return a list of buffer for current view."
  (-map (lambda (buffer) (buffer-name buffer)) (vview-current-buffer-list)))

(defun vview-switch-buffer (buffer)
  "Switch to BUFFER by changing view if needed."
  (let* ((the-buffer (get-buffer buffer))
         (best-view (vview-get-best-view the-buffer)))
    (if (not (equal best-view (vview-current-view)))
        (vview-switch-view (vview-view-name best-view))))
  (switch-to-buffer buffer))

(defun vview-current-view ()
  "Return current vview if it exists."
  (when vview-history (ht-get vview-views (first vview-history))))

(defun vview-get-registered-view (vview-or-name)
  "Return the registered vview associated to VVIEW-OR-NAME.
VVIEW-OR-NAME can be a VVIEW or the name of a VVIEW.
If no such vview is registered, an error is raised."
  (let ((vview-name (cond ((vview-view-p vview-or-name) (vview-view-name vview-or-name))
                          (t vview-or-name))))
    (unless (not (ht-contains-p vview-views vview-name))
      (error "%s is not registered" vview-name))))

(defun ivyew-switch-buffer-current-view ()
  "Interactivly switch buffer to another in current view."
  (interactive)
  (ivy-read "Switch buffer in current view: " (vview-current-buffer-name-list)
            :preselect (nth 1 (vview-current-buffer-name-list))
            :action #'switch-to-buffer
            :keymap ivy-switch-buffer-map))

(defun ivyew-switch-buffer-all-view ()
  "Interactivly switch buffer to another in current view."
  (interactive)
  (let ((buffer-name-list (-remove (lambda (buffer) (string-match-p "\\` " buffer))
                                   (-map (lambda (buffer) (buffer-name buffer))
                                         (buffer-list)))))
    (ivy-read "Switch buffer in current view: "
              buffer-name-list
              :preselect (nth 1 buffer-name-list)
              :action #'vview-switch-buffer
              :keymap ivy-switch-buffer-map)))

(defun ivyew-switch-view ()
  "Interactvily switch view."
  (interactive)
  (ivy-read "Switch views:"
            vview-history
            :action #'vview-switch-view
            :preselect (nth 1 vview-history)))

(vview-score-buffer (vview-current-view) (current-buffer))
(setq vview-history (list))
(message "%s" vview-views)
(message "%s" vview-history)
(setq default-directory "/home")
(vview-get-best-view (get-buffer "zefezfezfez"))
(vview-switch-buffer (get-buffer "zefezfezfez"))
(vview-current-buffer-list)
(vview-switch-view "mugu")
(vview-switch-view "mugu2")
(vview-switch-view "mugu4")
(vview-register-view  (vview-new "mugu"
                                 :modes '(org-mode)))
(vview-register-view  (vview-new "mugu2"
                                 :paths '("/usr")
                                 :names '(".*el$")))
(vview-register-view  (vview-new "mugu3"
                                 :score 2
                                 :paths '("/usr")
                                 :names '(".*el$")))
(vview-register-view  (vview-new "mugu4"
                                 :score 2
                                 :paths '("/usr")
                                 :names '(".*el$")))
(vview-make-var-local 'default-directory)
(vview-unregister-view "mugu")
(vview-unregister-view "mugu2")
(vview-unregister-view "mugu3")
(vview-unregister-view "mugu4")

;; for mode line
(defun safe-persp-name (&optional unused)
  "Return the current perspective name.
UNUSED is there for compatibility with spaceline"
  (format "%s" (vview-view-name (vview-current-view))))
(defun get-frame-persp ()
  "Do nothing.  Defined for compatibility with spaceline."
  "mugu")
(defvar persp-nil-name "no persp")

;; for mode line
;;;###autoload
(define-minor-mode persp-mode
  "Toggle perspective mode.
When active, keeps track of multiple 'perspectives',
TODO
named collections of buffers and window configurations."
  :global t
  (if persp-mode
      (progn
        (setq vview-views ht-create)
        (setq vview-history (list))
        (setq vview-local-vars (list))
        (vview-register-view (vview-new "default-view"
                                        :names '(".*")))
        (vview-switch-view "default-view"))))

;; projectile perspectives or directory perspective:
;; filter out : none (or temp buffer)
;; filter in  : default directory child of root

;; perspectives by name:
;; filter out : none
;; filter in : name with matching pattern

;;


;; vview : special buffer
;; gather special buffer
;; dont want to switch automatically on it

;; vview : project
;; gather project based on either root directory or the fact that is owned by the project
;; explicit switch is mandatory unless shadowed

;; vview : special special buffer
;; message/debug or magit or anything else
;; explicit switch is ok but it is also ok to not have

;; vview : specific mode
;; typically org files, mail things etc

;; switch buffer in view
;; switch buffer all views


(provide 'vview)

;; Local Variables:

;; End:
;;; perspective.el ends here
