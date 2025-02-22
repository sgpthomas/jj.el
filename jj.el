;;; jj.el --- A simple frontend for jj. -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Samuel Thomas

;; Author: Samuel Thomas <sgt@cs.utexas.edu>
;; Package-Requires: (dash magit-section s transient)

;;; External packages:
(require 'dash)
(require 'magit-section)
(require 's)
(require 'transient)
(require 'server)
(require 'thingatpt)

(defun jj-status ()
  (interactive)
  (let ((jj-buffer (get-buffer-create "*jj*"))
        (inhibit-read-only t))
    (with-current-buffer jj-buffer
      (jj-log-mode)
      (revert-buffer))
    (switch-to-buffer jj-buffer)))

;;;###autoload
(defvar jj--data-log
  (format "jj log --no-pager --color=never -T '%s' --no-graph"
          "change_id ++ \", \" ++ commit_id ++ \"\\n\""))

;;;###autoload
(defvar jj--user-log
  "jj log --no-pager --color=always")

;;;###autoload
(defvar jj--change-data
  '()
  "Alist mapping change ids to commit ids")

;;;###autoload
(defvar jj--marked-changes
  '()
  "A list of `marked' changed-ids")

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
  "Gets the `change_id' over the line `point' is in"

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
    (forward-line)))

(defun jj--goto-current-change ()
  (interactive)
  (let ((change-id (save-excursion
                     (goto-char (point-min))
                     (search-forward-regexp
                      (rx (: "Working copy" (0+ whitespace) ":" (0+ whitespace)
                             (group (= 8 alnum)))))
                     (match-string-no-properties 1))))
    (jj--goto-change-id change-id)))

(defun jj--render ()
  (jj--update-data)

  (let ((current-point (point))
        (inhibit-read-only t))
    (erase-buffer)
    (magit-insert-section (magit-section)
      (magit-insert-section (magit-section)
        (magit-insert-heading "Status")
        (magit-insert-section (magit-section)
          (magit-insert-section-body
            (insert (ansi-color-apply
                     (shell-command-to-string "jj st --no-pager --color=always")))
            (insert "\n"))))

      (magit-insert-section (magit-section)
        (magit-insert-heading "Log")
        (magit-insert-section-body
          (let ((before (point)))
            (insert (propertize (ansi-color-apply (shell-command-to-string jj--user-log))
                                'jj-log t))
            ;; go through and highlight all change-ids that are marked
            ;; TODO: not sure if this is where I want this to happen
            (goto-char before)
            (while (< (point) (point-max))
              (when (-contains? jj--marked-changes (word-at-point))
                (overlay-put (make-overlay (point) (- (point) (length (word-at-point))))
                             'face 'match))
              (forward-word)))
          (insert "\n"))))

    (goto-char current-point)))

(defun jj--debug-overlays ()
  (interactive)
  (let ((change-id (jj--change-id-at-point (point))))
    (goto-char (line-beginning-position))
    (setq-local jj--debug-word-at-point 'nil)
    (while (and (not jj--debug-word-at-point)
                (not (s-equals? jj--debug-word-at-point change-id))
                (< (point) (line-end-position)))
      (message "%s" jj--debug-word-at-point)
      (forward-to-word)
      (setq-local jj--debug-word-at-point (word-at-point))
      (let ((overlay (make-overlay (point) (+ (point) (length change-id)))))
        (overlay-put overlay 'face 'match)))))

(defun jj--debug-mark-all ()
  (interactive)
  (while (< (point) (point-max))
    (forward-word)
    (when (-contains? jj--marked-changes (word-at-point))
      (let ((overlay (make-overlay (point) (- (point) (length (word-at-point))))))
        (overlay-put overlay 'face 'match)))))

(defun jj--debug-remove-overlays (loc)
  (interactive "d")
  (--map (delete-overlay it) (overlays-at loc))
  )

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
  (unless server-mode
    (error "You need to start the emacs server with `server-start'"))
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
                         (revert-buffer)
                         (jj--goto-current-change)))))))))

;; (defun jj-new ()
;;   (interactive)
;;   (if jj--marked-changes
;;       (progn
;;         (shell-command-to-string (format "jj new %s" (s-join " " jj--marked-changes)))
;;         (setq jj--marked-changes 'nil)
;;         (revert-buffer))
;;     ;; else
;;     (let ((change-id (jj--change-id-at-point (point))))
;;       (when change-id
;;         (shell-command-to-string (format "jj new %s" change-id))
;;         (revert-buffer)
;;         (jj--goto-change-id change-id)))))

(transient-define-prefix jj-new-with-options ()
  ["Options"
   ("-m" "The change description to use" "--message=")
   ("-n" "Do not edit the newly created change" "--no-edit")]
  ["Actions"
   ("n" "New" jj--do-new)
   ("A" "After" jj-new-after)
   ("B" "Before" jj-new-before)])

(defun jj--do-new (&optional args)
  (interactive
   (list (transient-args 'jj-new-with-options)))

  (if jj--marked-changes
      (progn
        (shell-command-to-string (format "jj new %s %s"
                                         (s-join " " jj--marked-changes)
                                         (s-join " " args)))
        (setq jj--marked-changes 'nil)
        (revert-buffer)
        (jj--goto-current-change))
    ;; else
    (let ((change-id (jj--change-id-at-point (point))))
      (when change-id
        (shell-command-to-string (format "jj new %s %s"
                                         change-id
                                         (s-join " " args)))
        (revert-buffer)
        (jj--goto-current-change)))))

(defun jj-new-after (&optional args)
  (interactive
   (list (transient-args 'jj-new-with-options)))
  (let ((change-id (jj--change-id-at-point (point))))
    (when change-id
      (shell-command-to-string (format "jj new -A %s %s"
                                       change-id
                                       (s-join " " args)))
      (revert-buffer)
      (jj--goto-current-change))))

(defun jj-new-before (&optional args)
  (interactive
   (list (transient-args 'jj-new-with-options)))
  (let ((change-id (jj--change-id-at-point (point))))
    (when change-id
      (shell-command-to-string (format "jj new -B %s %s"
                                       change-id
                                       (s-join " " args)))
      (revert-buffer)
      (jj--goto-current-change))))

(transient-define-prefix jj-abandon ()
  ["Actions"
   ("RET" "Abandon change at point" jj--do-abandon)])

(defun jj--do-abandon ()
  (interactive)
  (let ((change-id (jj--change-id-at-point (point))))
    (when change-id
      (shell-command-to-string (format "jj abandon %s" change-id))
      (revert-buffer)
      (jj--goto-current-change))))

(defun jj-squash ()
  (interactive)
  (message "TODO: squash"))

(transient-define-prefix jj-rebase ()
  ["--from"
   ("r" "Revision" jj--rebase-revision)
   ("s" "Source" jj--rebase-source)])

(defun jj--rebase-revision ()
  (interactive)

  (let ((change-id (jj--change-id-at-point (point))))
    (when (and change-id jj--marked-changes)
      (message "%s" (shell-command-to-string
       (format "jj rebase -r %s %s"
               change-id
               (s-join " " (--map (format "-d %s" it)
                                  jj--marked-changes)))))
      (setq jj--marked-changes 'nil)
      (revert-buffer)
      (jj--goto-current-change))))

(defun jj--rebase-source ()
  (interactive)

  (let ((change-id (jj--change-id-at-point (point))))
    (when (and change-id jj--marked-changes)
      (shell-command-to-string
       (format "jj rebase -s %s %s"
               change-id
               (s-join " " (--map (format "-d %s" it)
                                  jj--marked-changes))))
      (setq jj--marked-changes 'nil)
      (revert-buffer)
      (jj--goto-current-change))))

(defun jj-diff ()
  (interactive)
  (let ((change-id (jj--change-id-at-point (point))))
    (when change-id
      (let ((diff-buffer (get-buffer-create (format "*jj-diff-%s*" change-id))))
        (switch-to-buffer diff-buffer)

        (jj-diff-mode)
        (let* ((inhibit-read-only t)
               (jj-diff-out (ansi-color-apply
                             (shell-command-to-string
                              (format "jj diff -r %s --color=always" change-id))))
               (proc-diff (jj--process-diff jj-diff-out)))
          (erase-buffer)
          (jj--insert-diff proc-diff))
        
        (goto-char (point-min))))))

(defun jj--process-diff (diff)
  (let* ((file-rx (rx (: (| "Modified" "Added" "Removed") (1+ any) ":" "\n")))
         (file-chunks (--map (s-split-up-to "\n" it 1) (s-slice-at file-rx diff)))
         (chunks-rx (rx (: bol (1+ any) "..." "\n"))))
    (--map (cons (car it)
                 (s-split chunks-rx (cadr it) t))
           file-chunks)))

(defun jj--insert-diff (processed-diff)
  (magit-insert-section (magit-section)
    (--map (magit-insert-section (magit-section)
             (magit-insert-heading (substring-no-properties (car it)))
             (magit-insert-section-body
               (if (length> (cdr it) 1)
                   (--map (magit-insert-section (magit-section)
                            (magit-insert-heading (jj--chunk-name it))
                            (magit-insert-section-body (insert it)))
                          (cdr it))
                 (insert (cadr it)))))
           processed-diff)))

(defun jj--chunk-name (chunk)
  (let* ((linenos (->> (s-lines chunk)
                       (--map
                        (if (s-index-of ":" it)
                            (substring-no-properties it 0 (1+ (s-index-of ":" it)))
                          ""))
                       (--map (s-split (rx (1+ whitespace)) it t))
                       (-flatten)
                       (--separate (not (s-ends-with? ":" it)))))
         (prev (-map #'string-to-number (car linenos)))
         (curr (--map (string-to-number (s-chop-suffix ":" it)) (cadr linenos))))
    (propertize (format "@@ removed %s-%s, added %s-%s @@"
                        (-min prev) (-max prev)
                        (-min curr) (-max curr))
                'face 'bold-italic)))


(defun jj--mark ()
  (interactive)

  (let ((change-id (jj--change-id-at-point (point))))
    (when change-id
      (add-to-list 'jj--marked-changes change-id))
    
    (revert-buffer)))

(defun jj--unmark ()
  (interactive)

  (let ((change-id (jj--change-id-at-point (point))))
    (setq-local jj--marked-changes
                (remove change-id jj--marked-changes))
    (revert-buffer)))

(transient-define-prefix jj-git-push ()
  [["Options"
    ("-R" "Remote (TODO)" "--remote=")
    ("-D" "Only display what will change on the remote" "--dry-run")
    ("-c" "(TODO) Push this commit by creating a bookmark based on its change ID" "--change=")]
   ["Bookmarks"
    ("-b" "Bookmark (TODO)" "--bookmark=")
    ("-a" "Push all bookmarks" "--all=")
    ("-t" "Push all tracked bookmarks" "--tracked")
    ("-d" "Push all deleted bookmarks" "--deleted")
    ("-N" "Allow pushing new bookmarks" "--allow-new")]
   ["Commits"
    ("-e" "Allow pushing commits with empty descriptions" "--allow-empty-description")
    ("-p" "Allow pushing commits that are private" "--allow-private")
    ("-r" "(TODO) Push bookmarks pointing to these commits" "--revisions=")]]
  ["Actions"
   ("p" "Push" jj--do-git-push)])

(defun jj--do-git-push (&optional args)
  (interactive
   (list (transient-args 'jj-git-push)))
  ;; TODO, display reuslt of command in a nicer way
  (message "%s"
           (shell-command-to-string (format "jj git push %s" (s-join " " args))))
  (revert-buffer)
  (jj--goto-current-change))

(transient-define-prefix jj-git-fetch ()
  ["Options"
   ;; TODO: list known branches
   ("-b" "(TODO) Fetch only some of the branches" "--branch=")
   ;; TODO: list known remotes
   ("-R" "(TODO) The remote to fetch from" "--remote=")
   ("-A" "Fetch from all remotes" "--all-remotes")]
  ["Actions"
   ("f" "Fetch" jj--do-git-fetch)])

(defun jj--do-git-fetch (&optional args)
  (interactive
   (list (transient-args 'jj-git-fetch)))
  ;; TODO, display reuslt of command in a nicer way
  (message "%s"
           (shell-command-to-string (format "jj git fetch %s" (s-join " " args))))
  (revert-buffer)
  (jj--goto-current-change))

(transient-define-prefix jj--bookmark-transient ()
  ["jj bookmark"
   ["Create"
    ("c" "Create" jj-bookmark-create)]
   ["Move"
    ("-B" "Allow moving bookmarks backwards or sideways" "--allow-backwards")
    ("m" "Move" jj-bookmark-move)]])

(defun jj-bookmark-create (bookmark-name)
  (interactive "MBookmark: ")
  (let ((change-id (jj--change-id-at-point (point))))
    (when change-id
      (shell-command-to-string (format "jj bookmark create -r %s %s" change-id bookmark-name))
      (revert-buffer)
      (jj--goto-current-change))))

(defun jj-bookmark-move (&optional args)
  (interactive
   (list (transient-args 'jj--bookmark-transient)))
  (let ((change-id (jj--change-id-at-point (point))))
    (if (equal (length jj--marked-changes) 1)
        (progn
          (shell-command-to-string
           (format "jj bookmark move --from %s --to %s %s"
                   (car jj--marked-changes)
                   change-id
                   (s-join " " args)))
          (setq jj--marked-changes 'nil)
          (revert-buffer)
          (jj--goto-current-change))
      ;; else
      (when change-id
        (let ((bookmark-name (read-string "Bookmark: ")))
          (shell-command-to-string
           (format "jj bookmark move --from %s --to %s %s"
                   bookmark-name
                   change-id
                   (s-join " " args)))
          (revert-buffer)
          (jj--goto-current-change))))))

(defun jj-bookmark-forget ()
  (interactive)
  (message "TODO: bookmark forget"))

(defun jj-test ()
  (interactive)
  (message "%s" (jj--change-id-at-point (point))))

(transient-define-prefix jj-help ()
  [["Editing Commands"
    ("e" "Edit" jj-edit)
    ("d" "Describe" jj-desc)
    ("n" "New" jj-new-with-options)
    ("x" "Abandon" jj-abandon)
    ("s" "Squash" jj-squash)
    ("r" "Rebase" jj-rebase)
    ("D" "Diff" jj-diff)]
   ["Git Commands"
    ("P" "Push" jj-git-push)
    ("F" "Fetch" jj-git-fetch)
    ("B" "Bookmarks" jj--bookmark-transient)]])

(defvar-keymap jj-log-mode-map
  :parent special-mode-map
  "," #'jj-test
  "C-i" #'magit-section-toggle

  "?" #'jj-help

  "e"  #'jj-edit
  "d"  #'jj-desc
  "n"  #'jj-new-with-options

  "x"  #'jj-abandon
  "s"  #'jj-squash
  "r"  #'jj-rebase
  "D"  #'jj-diff

  "m"  #'jj--mark
  "u"  #'jj--unmark

  "@" #'jj--goto-current-change

  ;; git commands
  "P" #'jj-git-push
  "F" #'jj-git-fetch
  "B" #'jj--bookmark-transient)

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
  (set-buffer-modified-p nil)
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
  (forward-line 1))

(defvar-keymap jj-diff-mode-map
  :parent special-mode-map
  "C-i" #'magit-section-cycle
  "<backtab>" #'magit-section-cycle-global
  "^" #'magit-section-up
  "p" #'magit-section-backward
  "n" #'magit-section-forward
  "M-p" #'magit-section-backward-sibling
  "M-n" #'magit-section-forward-sibling
  "1" #'magit-section-show-level-1
  "2" #'magit-section-show-level-2
  "3" #'magit-section-show-level-3
  "M-1" #'magit-section-show-level-1-all
  "M-2" #'magit-section-show-level-2-all
  "M-3" #'magit-section-show-level-3-all)

(define-derived-mode jj-diff-mode special-mode "jj diff"
  "Major mode for viewing jj diffs")

;;;###autoload
(defun jj-diff-editor (left right output)
  (message "%s %s %s" left right output)
  (ediff-directories3 left right output nil)
  (error "nyi"))

(define-minor-mode jj-diff-hl-mode
  "Toggles the highlight jj diffs in buffer."
  :init-value nil
  :global nil
  :group 'jj

  (if jj-diff-hl-mode
      (progn
        (jj--find-conflicts)
        (jj--highlight-conflicts)
        (jj--highlight-changes))
    (progn
      ;; TODO: use jj--conflicts to make this more efficient
      (save-excursion
        (goto-char (point-min))
        (while (not (eobp))
          (--map (when (overlay-get it 'jj-diff-hl)
                   (delete-overlay it))
                 (overlays-at (point)))
          (forward-line 1))))))

(defvar-local jj--conflicts '())

(defun jj--find-conflicts ()
  (let ((start-conflict-rx
         (rx (: "<<<<<<<" (1+ any) (1+ digit)
                space "of" space (1+ digit))))
        (end-conflict-rx
         (rx (: ">>>>>>" (1+ any) (1+ digit)
                space "of" space (1+ digit) space "ends"))))

    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward start-conflict-rx (point-max) t)
        (beginning-of-line)
        (let ((start (point)))
          (re-search-forward end-conflict-rx)
          (end-of-line)
          (let ((end (point)))
            (add-to-list 'jj--conflicts `(,start . ,(1+ end)))))))))

(defun jj--highlight-conflicts ()
  (--map (jj--make-overlay (car it) (cdr it) 'diff-index)
         jj--conflicts))

(defun jj--highlight-changes ()
  (--map (save-excursion
           (goto-char (car it))
           (when (re-search-forward (rx (: "%%%%%%%" (1+ any) "Changes"))
                                    (cdr it)
                                    t)
             (end-of-line)
             (let ((start (1+ (point))))
               (forward-line 1)
               (re-search-forward (rx (| "+++++++" ">>>>>>>" "%%%%%%%"))  (cdr it))
               (beginning-of-line)
               (let ((end (point)))
                 (goto-char start)
                 (while (< (point) end)
                   (beginning-of-line)
                   (when (s-starts-with? "+" (thing-at-point 'line))
                     (jj--make-overlay (line-beginning-position)
                                       (line-end-position)
                                       'diff-added))
                   (when (s-starts-with? "-" (thing-at-point 'line))
                     (jj--make-overlay (line-beginning-position)
                                       (line-end-position)
                                       'diff-removed))
                   (forward-line 1))
                 (jj--make-overlay start end 'diff-context)))))
         jj--conflicts))

(defun jj--highlight-contents ()
  )

(defun jj--make-overlay (start end face)
  (let ((overlay (make-overlay start end)))
    (overlay-put overlay 'jj-diff-hl t)
    (overlay-put overlay 'face face)))

;;; Code:
(provide 'jj)

;;; jj.el ends here
