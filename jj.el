;;; jj.el --- A simple frontend for jj. -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Samuel Thomas

;; Author: Samuel Thomas <sgt@cs.utexas.edu>
;; Package-Requires: (dash magit-section s transient )

;;; External packages:
(require 'dash)
(require 'magit-section)
(require 's)
(require 'transient)

(defun jj-status ()
  (interactive)
  (let ((jj-buffer (get-buffer-create "*jj*"))
        (inhibit-read-only t))
    (with-current-buffer jj-buffer
      (jj-log-mode)
      (revert-buffer))
    (switch-to-buffer-other-window jj-buffer)))

(defvar jj--data-log
  (format "jj log --no-pager --color=never -T '%s' --no-graph"
          "change_id ++ \", \" ++ commit_id ++ \"\\n\""))

(defvar jj--user-log
  "jj log --no-pager --color=always")

(defvar jj--change-data
  '()
  "Alist mapping change ids to commit ids")

(defun jj--update-data ()
  "Get list of change ids and commit ids from `jj'"

  (let* ((data-string (shell-command-to-string jj--data-log))
         (lines (s-split "\n" data-string)))
    (setq jj--change-data
          (--map (s-split ", " it) lines))))

(defun jj--change-id-prefix? (string)
  (--first (s-starts-with? string (car it))
           jj--change-data))

(defun jj--change-id-at-point (point)
  "Gets the `change_id' of under the given `point'"

  (save-excursion
    (goto-char point)
    (when (get-text-property (point) 'jj-log)
      (let* ((line (buffer-substring-no-properties (line-beginning-position)
                                                   (line-end-position))))
        (--first (jj--change-id-prefix? it)
                 (s-split " " (s-collapse-whitespace line)))))))

(defun jj--goto-change-id (change-id)
  (goto-char (point-min))
  (while (not (and
               (get-text-property (point) 'jj-log)
               (equal (jj--change-id-at-point (point))
                      change-id)))
    (next-line)))

(defun jj--render ()
  ;; testing
  ;; (setq-local default-directory "~/Development/yardbird")
  (jj--update-data)

  (let ((current-point (point))
        (inhibit-read-only t))
    (erase-buffer)
    (magit-insert-section (magit-section)
      (magit-insert-heading "Status")
      (magit-insert-section (magit-section)
        (magit-insert-section-body
          (insert (ansi-color-apply
                   (shell-command-to-string "jj st --no-pager --color=always")))
          (insert "\n")))

      (magit-insert-section (magit-section)
        (magit-insert-heading "Log")
        (magit-insert-section-body
          (insert (propertize (ansi-color-apply (shell-command-to-string jj--user-log))
                              'jj-log t))
          (insert "\n"))))

    (goto-char current-point)))

;; Commands:
(defun jj-edit ()
  (interactive)
  (let ((change-id (jj--change-id-at-point (point))))
    (when change-id
      (shell-command-to-string (format "jj edit %s" change-id))
      (revert-buffer)
      (jj--goto-change-id change-id))))

(defun jj-desc ()
  (interactive)
  (let ((change-id (jj--change-id-at-point (point)))
        (server-window 'pop-to-buffer))
    (when change-id
      (with-environment-variables (("JJ_EDITOR" "emacsclient"))
        (make-process
         :name "jj-desc"
         :buffer "*jj-desc-debug*"
         :command `("jj" "desc" "-r" ,change-id)
         :sentinel (lambda (_process event)
                      (when (s-equals? event "finished\n")
                       (with-current-buffer (get-buffer "*jj*")
                         (revert-buffer)))))))))

(defun jj-new ()
  (interactive)
  (let ((change-id (jj--change-id-at-point (point))))
    (when change-id
      (shell-command-to-string (format "jj new %s" change-id))
      (revert-buffer)
      (jj--goto-change-id change-id))))

(defun jj-abandon ()
  (interactive)
  (let ((change-id (jj--change-id-at-point (point))))
    (when change-id
      (message "%s"
               (shell-command-to-string (format "jj abandon %s" change-id)))
      (revert-buffer))))

(defun jj-new-merge ()
  (interactive)
  (message "TODO: new merge"))

(defun jj-squash ()
  (interactive)
  (message "TODO: squash"))

(defun jj-rebase ()
  (interactive)
  (message "TODO: rebase"))

(transient-define-prefix jj--git-transient ()
  [["Actions"
    ("p" "Push" jj-git-push)
    (jj-git-fetch)]
   ["Exit"
    ("q" "Quit" transient-quit-one)]])

(defun jj-git-push ()
  (interactive)
  (message "TODO: git push"))

(transient-define-prefix jj-git-push ()
  "jj git push"
  ["Arguments"
   ("-R" "Remote (TODO)" "--remote=")

   ("-b" "Bookmark (TODO)" "--bookmark=")
   ("-a" "Push all bookmarks" "--all=")
   ("-t" "Push all tracked bookmarks" "--tracked")
   ("-d" "Push all deleted bookmarks" "--deleted")

   ("-N" "Allow pushing new bookmarks" "--allow-new")
   ("-e" "Allow pushing commits with empty descriptions" "--allow-empty-description")
   ("-p" "Allow pushing commits that are private" "--allow-private")
   ("-r" "Push bookmarks pointing to these commits" "--revisions=")
   ("-c" "Push this commit by creating a bookmark based on its change ID" "--change=")
   ("-D" "Only display what will change on the remote" "--dry-run")]
  ["Actions"
   ("p" "Push" jj--do-git-push)
   ]
  )

(defun jj--do-git-push (&optional args)
  (interactive
   (list (transient-args 'jj-git-push)))
  (message "%s"
           (shell-command-to-string (format "jj git push %s" (s-join " " args)))))

(transient-define-suffix jj-git-fetch ()
  :transient nil
  :key "F"
  :description "Fetch"
  (interactive)
  (message "%s" (shell-command-to-string "jj git fetch")))

(transient-define-prefix jj--bookmark-transient ()
  [["Actions"
    ("c" "Create" jj-bookmark-create)]
   ["Exit" ("q" "Quit" transient-quit-one)]
   ]
  )

(defun jj-bookmark-create (bookmark-name)
  (interactive "MBookmark: ")
  (let ((change-id (jj--change-id-at-point (point))))
    (when change-id
      (shell-command-to-string (format "jj bookmark create -r %s %s" change-id bookmark-name))
      (revert-buffer))))

(defun jj-bookmark-move ()
  (interactive)
  (message "TODO: bookmark move"))

(defun jj-bookmark-forget ()
  (interactive)
  (message "TODO: bookmark forget"))

(defun jj-test ()
  (interactive)
  (message "%s" (jj--change-id-at-point (point))))

(defvar-keymap jj-log-mode-map
  :parent special-mode-map
  "," #'jj-test
  "C-i" #'magit-section-toggle

  "e"  #'jj-edit
  "d"  #'jj-desc
  "n"  #'jj-new
  "A"  #'jj-abandon
  "N"  #'jj-new-merge
  "s"  #'jj-squash
  "r"  #'jj-rebase

  ;; git commands
  "g" #'jj--git-transient
  ;; "gp" #'jj-git-push
  ;; "gf" #'jj-git-fetch

  ;; bookmark commands
  "b" #'jj--bookmark-transient
  ;; "bm" #'jj-bookmark-move
  ;; "bf" #'jj-bookmark-forget

  )

(define-derived-mode jj-log-mode special-mode "jj status"
  "Major mode for the jj status buffer."

  (buffer-disable-undo)
  (setq-local truncate-lines t
              revert-buffer-function (lambda (&optional _ignore-auto _no-confirm)
                                       (jj--render)))
  (when (fboundp 'evil-make-overriding-map)
    (evil-make-overriding-map jj-log-mode-map 'normal)))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.jjdescription" . jj-describe-mode))

(defvar-keymap jj-describe-mode-map
  :parent prog-mode-map
  "C-c C-c" #'jj-describe-confirm
  "C-c C-k" #'jj-describe-abort)

(defun jj-describe-confirm ()
  (interactive)
  (save-buffer)
  (server-edit))

(defun jj-describe-abort ()
  (interactive)
  (not-modified)
  (kill-buffer)
  (server-edit-abort))

;;;###autoload
(define-derived-mode jj-describe-mode prog-mode "jj describe"
  "Major mode for editing jj commit messages"

  (setq-local font-lock-keywords t)
  (setq-local syntax-propertize-function
              (syntax-propertize-rules
               ((rx line-start (group "JJ:"))  (1 "<"))
               ((rx (group "\n"))  (1 ">"))))
  (setq-local font-lock-defaults
              `(((,(rx "JJ:"
                       (* space)
                       (group
                        (: "M" " " (+ not-newline))))
                  1 'font-lock-function-call-face t)
                 (,(rx "JJ:"
                       (* space)
                       (group
                        (: "A" " " (+ not-newline))))
                  1 'font-lock-string-face t)
                 (,(rx "JJ:"
                       (* space)
                       (group
                        (: "D" " " (+ not-newline))))
                  1 'font-lock-warning-face t)
                 ;; hack for substitue-command-keys not working the way I expect
                 (,(rx (group
                        (: "‘" (group (+ (not "’"))) "’")))
                  2 'help-key-binding t))))

  (goto-char (point-min))
  ;; TODO: figure out how to highlight the keys
  (insert (substitute-command-keys
           "JJ: Press `\\[jj-describe-confirm]' to confirm, `\\[jj-describe-abort]' to abort\n"))
  (goto-line 1))

;;; Code:
(provide 'jj)

;;; jj.el ends here
