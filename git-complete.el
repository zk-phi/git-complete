;; git-complete.el (C) 2016 zk_phi / GPLv3-

;; Load this script
;;
;;   (require 'git-completion)
;;
;; and type something in a file under a git repo
;;
;;   ::SHA
;;
;; and `M-x git-completion` completes rest of the line, using `git
;; grep` as a completion source.
;;
;;   use Digest::SHA qw/sha1_base64/;

;; (global-set-key (kbd "C-c C-c") 'git-complete)

(require 'popup)

(defun git-complete--trim-spaces (str)
  (if (string-match "^[\s\t]*\\(.*[^\s\t]\\)[\s\t]*" str)
      (match-string 1 str)
    ""))

;; [newline insertion criteria]

;; 1. writing program from the top
;;    a. no newlines -> newline required
;;
;;       use strict;       use strict;
;;       use warnings; ->  use warnings;
;;       utf8|[EOF]        use utf8;
;;                        |[EOF]
;;
;;    b. has newline -> newline prefered
;;
;;       use strict;       use strict;
;;       use warnings; ->  use warnings;
;;       utf8|             use utf8;
;;       [EOF]            |
;;                         [EOF]

;; 2. inserting a newline into the middle of existing lines
;;    a. no newlines previously
;;       x. no newlines now -> newline required
;;
;;          use strict;            use strict;
;;          warnings|use utf8; ->  use warnings;
;;                                |use utf8;
;;
;;       y. still has newline -> no newlines prefered
;;
;;          use strict;     use strict;
;;          warnings|   ->  use warnings;
;;          use utf8;      |use utf8;

;;    b. has newline previously
;;       x. no newlines now -> newline required (but undistinguishable from 2ay case)
;;
;;          use strict;       use strict;
;;          use warnings; ->  use warnings;
;;          utf8|             use utf8;
;;          sub foo {
;;                           |sub foo {
;;
;;       y. still has newline -> no newlines prefered
;;
;;          use strict;       use strict
;;          use warnings; ->  use warnings;
;;          utf8|             use utf8;
;;                           |
;;          sub foo {         sub foo {

(defun git-complete--insert-newline-p ()
  (save-excursion
    (or (not (eolp))                    ; not EOL
        (not (zerop (forward-line -1))) ; EOL but also EOF
        (eobp))))                       ; next line is EOF

(defvar-local git-complete--root-dir nil)
(defun git-complete--root-dir ()
  (or git-complete--root-dir
      (setq git-complete--root-dir
            (cond ((null buffer-file-name) default-directory)
                  ((locate-dominating-file buffer-file-name ".git"))
                  (t (error "Not under a git repository."))))))

(defun git-complete--get-candidates (query)
  (let* ((default-directory (git-complete--root-dir))
         (command (format "git grep -F -h %s" (shell-quote-argument query)))
         (lines (split-string (shell-command-to-string command) "\n"))
         (hash (make-hash-table :test 'equal)))
    (while (and lines (cdr lines))
      (unless (string-equal "--" (car lines))
        (let ((str (git-complete--trim-spaces (pop lines))))
          (puthash str (1+ (gethash str hash 0)) hash))))
    (let ((result nil))
      (maphash (lambda (k v) (push (cons k v) result)) hash)
      (mapcar 'car (sort result (lambda (a b) (> (cdr a) (cdr b))))))))

(defun git-complete ()
  (interactive)
  (let* ((query (git-complete--trim-spaces (buffer-substring (point-at-bol) (point))))
         (candidates (and (not (string= query "")) (git-complete--get-candidates query))))
    (if (null candidates)
        (message "No completions found.")
      (let ((completion (popup-menu* candidates :scroll-bar t :isearch t)))
        (delete-region (point) (point-at-bol))
        (insert completion)
        (save-excursion (funcall indent-line-function))
        (cond ((git-complete--insert-newline-p)
               (insert "\n")
               (indent-for-tab-command))
              (t
               (forward-line 1)
               (skip-chars-forward "\s\t")))))))

(provide 'git-complete)
