;;; git-complete.el -- Linewise completion engine powered by "git grep"

;; Copyright (C) 2017- zk_phi

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301 USA

;; Author: zk_phi
;; URL: http://hins11.yu-yake.com/
;; Version: 0.0.0
;; Package-Requires: ((popup "0.4"))

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

;;; Change Log:

;; 0.0.0 text release

;;; Code:

(require 'popup)

(defgroup git-complete nil
  "Complete lines via git-grep results."
  :group 'git-complete)

(defcustom git-complete-enable-dwim-newline t
  "When non-nil, git-complete tries to guess if you want to a
newline or not after completion. Otherwise TAB will not insert a
newline but RET does."
  :type 'boolean
  :group 'git-complete)

(defcustom git-complete-enable-autopair t
  "When non-nil, close parens are automatically inserted when the
completed line has unclosed parens."
  :type 'boolean
  :group 'git-complete)

(defcustom git-complete-lispy-modes
  '(lisp-mode emacs-lisp-mode scheme-mode
              lisp-interaction-mode gauche-mode scheme-mode
              clojure-mode racket-mode egison-mode)
  "List of lisp-like language modes. Newline is not inserted
after the point by when `git-complete-enable-autopair', in the
modes."
  :type '(list symbol)
  :group 'git-complete)

(defcustom git-complete-multiline-complete-threshold 0.4
  "Threshold to determine whether to start multiline completion
or not. When 0.4, which is the default value, starts multiline
completion if the second line will be correctly completed with
30% or greater probablity, for example."
  :type 'number
  :group 'git-complete)

;; * utilities

(defun git-complete--trim-spaces (str)
  "Remove leading/trailing whitespaces from STR."
  (replace-regexp-in-string "^[\s\t]*\\|[\s\t]*$" "" str))

(defvar git-complete--popup-menu-keymap
  (let ((kmap (copy-keymap popup-menu-keymap)))
    (define-key kmap (kbd "TAB") 'popup-select)
    kmap)
  "Keymap for git-complete popup menu.")

(defvar-local git-complete--root-dir nil)
(defun git-complete--root-dir ()
  "Find the root directory of this git repo. If current directory
is not under a git repo, raises an error."
  (or git-complete--root-dir
      (setq git-complete--root-dir
            (cond ((null buffer-file-name) default-directory)
                  ((locate-dominating-file buffer-file-name ".git"))
                  (t (error "Not under a git repository."))))))

(defun git-complete--get-unclosed-parens (str)
  "Parse parens in STR and return a string of unclosed parens."
  (let ((expected-closes nil) syntax char class partner)
    (with-temp-buffer
      (save-excursion (insert str))
      (while (progn (skip-syntax-forward "^\\\\()") (not (eobp)))
        (setq char    (char-after)
              syntax  (aref (syntax-table) (char-after))
              class   (car syntax)
              partner (cdr syntax))
        (cond ((= class 4)              ; open paren
               (push partner expected-closes))
              ((= class 5)              ; close paren
               (when (and expected-closes (= (car expected-closes) char))
                 (pop expected-closes)))
              ((= class 9)              ; escape
               (forward-char 1)))
        (forward-char 1)))
    (apply 'string (nreverse expected-closes))))

;; * get candidates via git grep

(defun git-complete--get-candidates (query threshold &optional next-line)
  "Get completion candidates with `git grep'."
  (let* ((default-directory (git-complete--root-dir))
         (command (format "git grep -F -h %s %s"
                          (if next-line "-A1" "")
                          (shell-quote-argument query)))
         (lines (split-string (shell-command-to-string command) "\n"))
         (hash (make-hash-table :test 'equal))
         (total-count 0))
    (while (and lines (cdr lines))
      (when next-line (pop lines))      ; pop the first line
      (let ((str (git-complete--trim-spaces (pop lines))))
        (unless (string= "" str)
          (setq total-count (1+ total-count))
          (puthash str (1+ (gethash str hash 0)) hash)))
      (when next-line (pop lines)))     ; pop "--"
    (let* ((result nil)
           (threshold (* (or threshold 0) total-count)))
      (maphash (lambda (k v) (push (cons k v) result)) hash)
      (delq nil
            (mapcar (lambda (x) (and (>= (cdr x) threshold) (car x)))
                    (sort result (lambda (a b) (> (cdr a) (cdr b)))))))))

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

(defun git-complete (&optional threshold)
  "Complete the line at point with `git grep'."
  (interactive)
  (let* ((next-line-p (looking-back "^[\s\t]*"))
         (query (save-excursion
                  (when next-line-p (forward-line -1) (end-of-line))
                  (git-complete--trim-spaces (buffer-substring (point-at-bol) (point)))))
         (candidates (and (not (string= query ""))
                          (git-complete--get-candidates query (or threshold 0) next-line-p))))
    (if (null candidates)
        (message "No completions found.")
      (let ((completion (popup-menu* candidates :scroll-bar t :isearch t
                                     :keymap git-complete--popup-menu-keymap))
            close beg end)
        (delete-region (point) (point-at-bol))
        (setq beg (point))
        (insert completion)
        (save-excursion
          (when git-complete-enable-autopair
            (when (not (string= "" (setq close (git-complete--get-unclosed-parens completion))))
              (insert "\n" (if (memq major-mode git-complete-lispy-modes) "" "\n") close)))
          (when (if git-complete-enable-dwim-newline
                    (git-complete--insert-newline-p)
                  (eql last-input-event 13)) ; 13 = RET
            (insert "\n"))
          (setq end (point)))
        (indent-region beg end)
        (forward-line 1)
        (funcall indent-line-function)
        (back-to-indentation)
        (git-complete git-complete-multiline-complete-threshold)))))

;; * provide

(provide 'git-complete)

;;; git-complete.el ends here
