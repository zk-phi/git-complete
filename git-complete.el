;; git-complete.el (C) 2016 zk_phi / GPLv3-

;; Load this script
;;
;;   (require 'git-completion)
;;
;; and type something in a file under a git repo
;;
;;   ::SHA
;;
;; then `M-x git-completion` completes rest of the line, if suitable
;; one is found in your git repo.
;;
;;   use Digest::SHA qw/sha1_base64/;
;;
;; You may also bind some keys to the command.
;;
;;   (global-set-key (kbd "C-c C-c") 'git-complete)

(require 'popup)

(defvar git-complete-enable-dwim-newline nil
  "When non-nil, git-complete tries to guess if you want to a
newline or not after completion. Otherwise TAB will not insert a
newline but RET does.")

;; * utilities

(defun git-complete--trim-spaces (str)
  "Remove leading/trailing whitespaces from STR. This is a
non-destructive function."
  (if (string-match "^[\s\t]*\\(.*[^\s\t]\\)[\s\t]*" str)
      (match-string 1 str)
    ""))

(defvar git-complete--popup-menu-keymap
  (let ((kmap (copy-keymap popup-menu-keymap)))
    (define-key kmap (kbd "TAB") 'popup-select)
    kmap)
  "Keymap for git-complete popup menu.")

(defvar-local git-complete--root-dir nil)
(defun git-complete--root-dir ()
  (or git-complete--root-dir
      (setq git-complete--root-dir
            (cond ((null buffer-file-name) default-directory)
                  ((locate-dominating-file buffer-file-name ".git"))
                  (t (error "Not under a git repository."))))))

;; * get candidates via git grep

(defun git-complete--get-candidates (query &optional next-line)
  (let* ((default-directory (git-complete--root-dir))
         (command (format "git grep -F -h %s %s"
                          (if next-line "-A1" "")
                          (shell-quote-argument query)))
         (lines (split-string (shell-command-to-string command) "\n"))
         (hash (make-hash-table :test 'equal)))
    (while (and lines (cdr lines))
      (when next-line (pop lines))      ; pop the first line
      (let ((str (git-complete--trim-spaces (pop lines))))
        (puthash str (1+ (gethash str hash 0)) hash))
      (when next-line (pop lines)))     ; pop "--"
    (let ((result nil))
      (maphash (lambda (k v) (push (cons k v) result)) hash)
      (mapcar 'car (sort result (lambda (a b) (> (cdr a) (cdr b))))))))

;; * do completion

(defun git-complete--insert-newline-p ()
  "Determine whether to insert newline here, after completion.

1. not EOL -> insert newline

   use strict;            use strict;
   warnings|use utf8; ->  use warnings;
                         |use utf8;

2. EOF -> insert newline

   use strict;       use strict;
   use warnings; ->  use warnings;
   utf8|[EOF]        use utf8;
                    |[EOF]

3. next line is EOF -> insert newline

   use strict;       use strict;
   use warnings; ->  use warnings;
   utf8|             use utf8;
   [EOF]            |
                     [EOF]

4. next line is empty -> DO NOT insert newline

   use strict;       use strict
   use warnings; ->  use warnings;
   utf8|             use utf8;
                    |
   sub foo {         sub foo {

5. next line is not empty
   a. current line hasn't been empty -> DO NOT insert newline

      use strict;     use strict;     use strict;
      use utf8;   ->  warnings|   ->  use warnings;
                      use utf8;      |use utf8;

   b. the line had been empty -> insert newline

      use strict;      use strict;       use strict;
      use warnings; -> use warnings; ->  use warnings;
                       utf8|             use utf8;
      sub foo {        sub foo {        |
                                         sub foo{

   * since I have no good idea to distinguish these two cases,
     git-complete never inserts a newline."
  (save-excursion
    (or (not (eolp))                    ; not EOL
        (not (zerop (forward-line -1))) ; EOL but also EOF
        (eobp))))                       ; next line is EOF

(defun git-complete ()
  (interactive)
  (let* ((next-line-p (looking-back "^[\s\t]*"))
         (query (save-excursion
                  (when next-line-p (forward-line -1) (end-of-line))
                  (git-complete--trim-spaces (buffer-substring (point-at-bol) (point)))))
         (candidates (delete "" (and (not (string= query ""))
                                     (git-complete--get-candidates query next-line-p)))))
    (if (null candidates)
        (message "No completions found.")
      (let ((completion (popup-menu* candidates :scroll-bar t :isearch t
                                     :keymap git-complete--popup-menu-keymap)))
        (delete-region (point) (point-at-bol))
        (insert completion)
        (save-excursion (funcall indent-line-function))
        (cond ((or (not (eolp))
                   (if git-complete-enable-dwim-newline
                       (git-complete--insert-newline-p)
                     (= last-input-event 13))) ; 13 = RET
               (insert "\n"))
              (t
               (forward-line 1)))
        (back-to-indentation)
        (save-excursion (funcall indent-line-function))))))

;; * provide

(provide 'git-complete)
