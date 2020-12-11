;;; -*- lexical-binding: t -*-
;;; git-complete.el --- Yet another completion engine powered by "git grep"

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
;; URL: http://zk-phi.github.com/
;; Version: 0.0.11
;; Package-Requires: ((popup "0.4"))

;; Load this script
;;
;;   (require 'git-complete)
;;
;; and type something in a file under a git repo
;;
;;   ::SHA
;;
;; then `M-x git-complete` suggests rest of the line by git-grepping
;; your repo.
;;
;;   use Digest::SHA qw/sha1_base64/;
;;
;; You may also bind some keys to the command.
;;
;;   (global-set-key (kbd "C-c C-c") 'git-complete)

;;; Change Log:

;; 0.0.0 test release
;; 0.0.1 add option `git-complete-repeat-completion'
;; 0.0.2 add option `git-complete-threshold'
;; 0.0.3 add option `git-complete-omni-completion-type'
;; 0.0.4 remove obsolete variables
;; 0.0.5 add option `git-complete-grep-function' and support ripgrep
;; 0.0.6 add option `git-complete-candidate-limit'
;; 0.0.7 implement better newline handling
;; 0.0.8 performance improvements
;; 0.0.9 add option `git-complete-normalize-spaces'
;; 0.0.10 add options `git-complete-eol/bol-indicator'
;; 0.0.11 rewrite autopair mechanism to be more simple-minded

;;; Code:

(require 'popup)
(require 'cl-lib)

(defgroup git-complete nil
  "Yet another completion engine powered by \"git grep\"."
  :group 'git-complete)

(defcustom git-complete-enable-autopair t
  "When non-nil, `git-complete' tries to keep the parens balanced
during completion (i.e. automatically insert close parens
together with open parens, avoid inserting extra close parens,
and do not invoke whole-line completion when the current line has
more open/close parens than close/open parens)."
  :type 'boolean
  :group 'git-complete)

(defcustom git-complete-lispy-modes
  '(lisp-mode emacs-lisp-mode scheme-mode
              lisp-interaction-mode gauche-mode scheme-mode
              clojure-mode racket-mode egison-mode)
  "List of lisp-like language modes. Newline is not inserted
after the point by when `git-complete-enable-autopair', in the
modes."
  :type '(repeat symbol)
  :group 'git-complete)

(defcustom git-complete-threshold 0.05
  "Threshold to filter the results from `git grep'. When 0.05 for
example, which is the defualt value, completion candidates which
occupy less than 5% amount the grep results are dropped."
  :type 'number
  :group 'git-complete)

(defcustom git-complete-whole-line-completion-threshold 0.1
  "Like `git-complete-threshold', but used to determine whether
use whole-line completion or not. Set this variable greater than
1.0 to disable whole-line completion."
  :type 'number
  :group 'git-complete)

(defcustom git-complete-next-line-completion-threshold 0.3
  "Like `git-complete-threshold' but used while next-line
completion. Set this variable greater than 1.0 to disable
next-line completion"
  :type 'number
  :group 'git-complete)

(defcustom git-complete-normalize-spaces t
  "When non-nil, git-complete do not distinguish two or more
  consecutive spaces and a single space, when extracting
  candidates."
  :type 'boolean
  :group 'git-complete)

(defcustom git-complete-candidate-limit 100000
  "Maximum number of grep result. If more lines are found by
  grep, stop completion."
  :type 'number
  :group 'git-complete)

(defcustom git-complete-omni-completion-type 'subword
  "Specifies how to shorten query to perform omni-completion. Can
be either 'symbol, 'word, 'subword, or nil to disable
omni-completion."
  :type 'symbol
  :group 'git-complete)

(defcustom git-complete-repeat-completion 'newline
  "When nil, do not repeat completion after successful
completions. When `newline', repeat completion only after a
newline. Otherwise always repeat completion."
  :type 'symbol
  :group 'git-complete)

(defcustom git-complete-ignore-case 'dwim
  "When t, git-complete call git grep with `--ignore-case'
option. When 'dwim, enable `--ignore-case' only when the query
has an upcase character. When nil, git-complete does not use
`--ignore-case'."
  :type 'symbol
  :group 'git-complete)

(defcustom git-complete-limit-extension nil
  "When non-nil, candidates are limited to files with the same
extension as the current buffer. See also:
`git-complete-major-mode-extensions-alist'."
  :type 'boolean
  :group 'git-complete)

(defcustom git-complete-fallback-function nil
  "When a function is set, the function is called if completion
fails."
  :type 'function
  :group 'git-complete)

(defcustom git-complete-major-mode-extensions-alist
  '((c-mode "c" "h")
    (cperl-mode "pl" "pm" "t"))
  "Alist of major-mode vs list of extensions. If
`git-complete-limit-extension' is non-nil and the current
major-mode has an entry in this alist, limit candidates to files
with matching extensions *listed in the alist*, instead of the
current file's extension."
  :type 'sexp
  :group 'git-complete)

(defcustom git-complete-enable-isearch t
  "When non-nil, enable isearch by default on selecting completion
candidate."
  :type 'boolean
  :group 'git-complete)

(defcustom git-complete-bol-indicator ""
  "String to indicate beginning-of-lines in the tooltip."
  :type 'string
  :group 'git-complete)

(defcustom git-complete-eol-indicator ""
  "String to indicate end-of-lines in the tooltip."
  :type 'string
  :group 'git-complete)

(defcustom git-complete-grep-function 'git-complete-git-grep
  "Function used to grep over the git repo. You may assume that
the function is called with `default-directory' set to the
project root. The function must return result as a list of
strings."
  :type 'function
  :group 'git-complete)

;; * utilities

(defun git-complete--maphash (fn hash)
  "Like `maphash' but returns a list of returned value as the
result."
  (let (lst)
    (maphash (lambda (k v) (push (funcall fn k v) lst)) hash)
    lst))

(defun git-complete--up-list-unsafe ()
  "Like `up-list' but regardless of `forward-sexp-function'."
  (goto-char (or (scan-lists (point) 1 1) (buffer-end 1))))

(defun git-complete--normalize-query (str)
  "Remove all leading spaces (indentation) from STR. If STR has
  more than two trailing spaces, then delete them except for
  one."
  (with-temp-buffer
    (save-excursion (insert str))
    (skip-chars-forward "\s\t")
    (delete-region (point-min) (point))
    (goto-char (point-max))
    (when (<= (skip-chars-backward "\s\t") -2)
      (delete-region (1+ (point)) (point-max)))
    (buffer-string)))

(defun git-complete--trim-candidate (str &optional query lim-paren)
  "If QUERY is specified, search QUERY inside STR, and remove
   characters before the query and the query itself (if no
   matches are found, return an empty string). If LIM-PAREN is
   non-nil and STR has more close parens than open parens, then
   remove all characters after the last matching close paren."
  (with-temp-buffer
    (save-excursion (insert str))
    (when query
      (or (search-forward query nil t)
          (goto-char (point-max)))
      (delete-region (point-min) (point)))
    (when lim-paren
      (ignore-errors
        (git-complete--up-list-unsafe)
        (forward-char -1)
        (skip-chars-backward "\s\t")
        (delete-region (point) (point-max))))
    (buffer-string)))

(defun git-complete--normalize-candidate (str &optional no-leading-whitespaces)
  "Remove all trailing spaces from STR. If STR ends with a
  newline character, then delete all spaces at the eol. If
  NO-LEADING-WHITESPACES is non-nil, leading spaces are removed
  too. Then replace all consecutive spaces with single space
  character. "
  (with-temp-buffer
    (save-excursion (insert (replace-regexp-in-string "[\s\t]+" " " str)))
    (when (and no-leading-whitespaces (> (skip-chars-forward "\s\t") 0))
      (delete-region (point) (point-min)))
    (end-of-line)
    (skip-chars-backward "\s\t")
    (delete-region (point) (point-at-eol))
    (buffer-string)))

(defvar-local git-complete--root-dir nil) ; cache
(defun git-complete--root-dir ()
  "Find the root directory of this git repo. If the current
directory is not under a git repo, raises an error. This function
caches the result per buffer."
  (or git-complete--root-dir
      (setq git-complete--root-dir
            (and buffer-file-name (locate-dominating-file buffer-file-name ".git")))))

(defvar-local git-complete--extensions nil) ; cache
(defun git-complete--extensions ()
  "Returns a list of extensions to which candidates should be
limited."
  (and git-complete-limit-extension
       (or git-complete--extensions
           (setq git-complete--extensions
                 (or (assoc-default major-mode git-complete-major-mode-extensions-alist)
                     (list (file-name-extension buffer-file-name)))))))

(defun git-complete--shorten-query (query)
  "Shorten QUERY by one word (according to
git-complete-omni-completion-type). If it cannot be shorter,
return an empty string."
  (let* ((case-fold-search nil)
         (shortened (replace-regexp-in-string
                     (cl-case git-complete-omni-completion-type
                       ((symbol)  "^\\(.+?\\)\\_<.")
                       ((word)    "^\\(.+?\\)\\<.")
                       ((subword) "^\\(.+?\\)\\(\\<.\\|[A-Z][a-z]\\)")
                       (t "^$"))
                     "" query nil nil 1)))
    (if (string= query shortened) "" shortened)))

;; * smart string substitution

(defun git-complete--parse-parens (str)
  "Parse parens in STR and returns either t, nil or a list. If
this function returns nil, parens in STR are balanced. If this
function returns a list, STR has some unmatched open parens and
it is a list of close parens required to restore the balance. If
this function returns t, STR has some unmatched close parens."
  (with-temp-buffer
    (save-excursion (insert str))
    (catch 'return
      (let (stack)
        (while (progn (skip-syntax-forward "^\\\\()") (not (eobp)))
          (let* ((char (char-after))
                 (syntax (aref (syntax-table) char)))
            (cl-case (car syntax)
              ((4)                      ; open paren
               (push (cdr syntax) stack))
              ((5)                      ; close paren
               (if (and stack (= (car stack) char))
                   (pop stack)
                 (throw 'return t)))
              ((9)                      ; escape
               (forward-char 1))))
          (forward-char 1))
        stack))))

;; [how "git-complete--extract-candidates" works]
;;
;;  SORTED CANDIDATES           STACK
;;                              <empty>
;;                              (".Component" 0) (" {" 0) stack
;; .Component {                 (".Component" 0) (" {" 1) +inc
;; .Component {                 (".Component" 0) (" {" 2) +inc
;; .Component {                 (".Component" 0) (" {" 3) +inc
;; .Component {                 (".Component" 0) (" {" 4) +inc
;; .Component {                 (".Component" 0) (" {" 5) +inc
;;                              (".Component" 0) (" {" 5) -> completion
;;                              (".Component" 0) <- reduce
;;                              <empty>
;;                              ("DOM" 0) (" from" 0) (" 'react-dom" 0) ("';" 0) stack
;; DOM from 'react-dom';        ("DOM" 0) (" from" 0) (" 'react-dom" 0) ("';" 1) +inc
;; DOM from 'react-dom';        ("DOM" 0) (" from" 0) (" 'react-dom" 0) ("';" 2) +inc
;;                              ("DOM" 0) (" from" 0) (" 'react-dom" 0) ("';" 2) -> completion
;;                              ("DOM" 0) (" from" 0) (" 'react-dom" 0) <- reduce
;;                              ("DOM" 0) (" from" 0) <- reduce
;;                              ("DOM" 0)
;;                              ("DOM" 0) (".render" 0) ("(<Component" 1) (" />);" 0) stack
;; DOM.render(<Component />);   ("DOM" 0) (".render" 0) ("(<Component" 0) (" />);" 1) +inc
;;                              ("DOM" 0) (".render" 0) ("(<Component" 0) (" />);" 1) <- reduce
;;                              ("DOM" 0) (".render" 0) ("(<Component" 1) <- reduce
;;                              ("DOM" 0) (".render" 1) <- reduce
;;                              ("DOM" 0) (".render" 1) ("(<MyComponent" 1) (" />);" 0) stack
;; DOM.render(<MyComponent />); ("DOM" 0) (".render" 1) ("(<MyComponent" 0) (" />);" 1) +inc
;;                              ("DOM" 0) (".render" 1) ("(<MyComponent" 0) (" />);" 1) <- reduce
;;                              ("DOM" 0) (".render" 1) ("(<MyComponent" 1) <- reduce
;;                              ("DOM" 0) (".render" 2) -> completion
;;                              ("DOM" 0) <- reduce
;;                              <empty>
;;                              (" from" 0) ("'react" 0) ("';" 0) stack
;;  from 'react';               (" from" 0) ("'react" 0) ("';" 1) +inc
;;  from 'react';               (" from" 0) ("'react" 0) ("';" 2) +inc
;;  from 'react';               (" from" 0) ("'react" 0) ("';" 3) +inc
;;  from 'react';               (" from" 0) ("'react" 0) ("';" 4) +inc
;;                              (" from" 0) ("'react" 0) ("';" 4) -> completion
;;                              (" from" 0) ("'react" 0) <- reduce
;;                              (" from" 0) <- reduce
;;                              <empty>

(defun git-complete--extract-candidates (sorted-list exact-only threshold)
  (let* ((threshold (* threshold (length sorted-list)))
         (stack-root (list ["" "" 0]))     ; sentinel
         extracted
         (reduce-stack-fn (lambda (stack)
                            (cl-reduce
                             (lambda (l r)
                               (let ((count (+ (aref l 2) r)))
                                 (if (< count threshold) count
                                   (push (cons (aref l 1) count) extracted)
                                   0)))
                             stack :from-end t :initial-value 0))))
    (dolist (item sorted-list)
      (let ((words (if exact-only (list item) (split-string item "$\\|\\_>" t)))
            (stack stack-root))
        (while words
          (when (and (cdr stack)
                     (not (string= (car words) (aref (cadr stack) 0))))
            ;; Stack: (("" . 0) #=("React" . 0) ("DOM" . 1)) / Split: (" from" " 'react" "';")
            ;; -> Stack: (("" . 0) #=("React" . 1)) / Split: (" from" " 'react" "';")
            (cl-incf (aref (car stack) 2) (funcall reduce-stack-fn (cdr stack)))
            (setcdr stack nil))
          (when (null (cdr stack))
            ;; Stack: (("" . 0) #=("React" . 1)) / Split: (" from" " 'react" "';")
            ;; -> Stack: (("" . 0) #=("React" . 1) (" from" . 1)) / Split: (" from" " 'react" "';")
            (let ((prefix (aref (car stack) 1)))
              (setcdr stack (list (vector (car words) (concat prefix (car words)) 0)))))
          ;; Stack: (("" . 0) #=("React" . 1) (" from" . 1)) / Split: (" from" " 'react" "';")
          ;; -> Stack: (("" . 0) ("React" . 1) #=(" from" . 1)) / Split: (" 'react" "';")
          (setq stack (cdr stack) words (cdr words)))
        (cl-incf (aref (car stack) 2))))
    (funcall reduce-stack-fn (cdr stack-root))
    (mapcar 'car (sort extracted (lambda (a b) (> (cdr a) (cdr b)))))))

;; * grep

(defun git-complete--get-grep-result (query nextline-p)
  "Get completion candidates. This function calls `git grep'
command to get lines matching QUERY and returns as a list of
string."
  (when (git-complete--root-dir)
    (let* ((default-directory (git-complete--root-dir)))
      (funcall git-complete-grep-function
               (shell-quote-argument query)
               (git-complete--extensions)
               nextline-p
               (if (eq git-complete-ignore-case 'dwim)
                   (not (string-match "[A-Z]" query))
                 git-complete-ignore-case)))))

(defun git-complete-git-grep (query extensions nextline-p ignore-case-p)
  (let* ((command (format "git grep -F -h %s %s %s -- %s"
                          (if nextline-p "-A1" "")
                          (if ignore-case-p "-i" "")
                          query
                          (if extensions
                              (mapconcat (lambda (ext) (concat "\"*." ext "\"")) extensions " ")
                            "*")))
         (lines (split-string (shell-command-to-string command) "^" t))
         lst)
    (while (and lines (cdr lines))
      (when nextline-p (pop lines))     ; pop the first line
      (push (pop lines) lst)
      (when nextline-p (pop lines)))    ; pop "--"
    lst))

(defun git-complete-ripgrep (query extensions nextline-p ignore-case-p)
  (let* ((command (format "rg --no-heading --no-filename --no-line-number -F %s %s %s -- %s"
                          (if nextline-p "-A1" "")
                          (if ignore-case-p "-i" "")
                          (if extensions
                              (mapconcat (lambda (ext) (concat "-g '*." ext "'")) extensions " ")
                            " ")
                          query))
         (lines (split-string (shell-command-to-string command) "^" t))
         lst)
    (while (and lines (cdr lines))
      (when nextline-p (pop lines))     ; pop the first line
      (push (pop lines) lst)
      (when nextline-p (pop lines)))    ; pop "--"
    lst))

;; * interface

(defvar git-complete--popup-menu-keymap
  (let ((kmap (copy-keymap popup-menu-keymap)))
    (define-key kmap (kbd "TAB") 'popup-select)
    kmap)
  "Keymap for git-complete popup menu.")

(defun git-complete--collect-whole-line-candidates (query)
  "Collect whole-line completion candidates and return as a list of string."
  (when (and (string-match "\\_>" query) ; at least one symbol required
             (or (not git-complete-enable-autopair)
                 (eq (git-complete--parse-parens query) nil)))
    (let ((candidates (git-complete--get-grep-result query nil)))
      (unless (> (length candidates) git-complete-candidate-limit)
        (let* ((trimmed
                (if (not git-complete-enable-autopair) candidates
                  (mapcar (lambda (s) (git-complete--trim-candidate s nil t)) candidates)))
               (normalized
                (if (not git-complete-normalize-spaces) trimmed
                  (mapcar (lambda (s) (git-complete--normalize-candidate s t)) candidates)))
               (filtered
                (cl-remove-if
                 ;; if QUERY is substring of a candidate, the candidate
                 ;; is an omni candidate
                 (lambda (s) (or (string= s "") (string-prefix-p query s))) normalized)))
          (git-complete--extract-candidates
           (sort filtered 'string<) t git-complete-whole-line-completion-threshold))))))

(defun git-complete--collect-omni-candidates (query next-line-p no-leading-whitespaces)
  "Collect omni completion candidates and return as a list of string."
  (when (string-match "\\_>" query)     ; at least one symbol required
    (let ((candidates (git-complete--get-grep-result query next-line-p)))
      (unless (> (length candidates) git-complete-candidate-limit)
        (let* ((trimmed
                (mapcar (lambda (s)
                          (git-complete--trim-candidate
                           s (and (not next-line-p) query) git-complete-enable-autopair))
                        candidates))
               (normalized
                (if (not git-complete-normalize-spaces) trimmed
                  (mapcar (lambda (s) (git-complete--normalize-candidate s no-leading-whitespaces))
                          trimmed)))
               (filtered
                (cl-remove-if (lambda (s) (or (string= s "") (string= s "\n"))) normalized))
               (extracted
                (git-complete--extract-candidates
                 (sort filtered 'string<) next-line-p
                 (if next-line-p
                     git-complete-next-line-completion-threshold
                   git-complete-threshold))))
          (or extracted
              (git-complete--collect-omni-candidates
               (git-complete--shorten-query query) next-line-p no-leading-whitespaces)))))))

(defun git-complete--make-popup-item (str whole-line-p)
  "If STR ends with a newline character, then drop the newline
  character and return (STR . t). Otherwise just return (STR
  . nil)."
  (let* ((bol (if whole-line-p git-complete-bol-indicator ""))
         (newline (string-match "\n" str))
         (str (if newline (substring str 0 (match-beginning 0)) str))
         (dispstr (concat bol str (if newline git-complete-eol-indicator ""))))
    (popup-make-item dispstr :value (cons whole-line-p (cons str newline)))))

(defun git-complete--insert-close-paren-if-needed (str newline)
  (let ((close-parens (git-complete--parse-parens str))
        (beg (point)))
    (when (consp close-parens)
      (insert (if (and newline (not (memq major-mode git-complete-lispy-modes))) "\n" "")
              (apply 'string close-parens))
      (indent-region beg (point)))))

(defun git-complete ()
  "Complete the line at point with `git grep'."
  (interactive)
  (let* ((bol (point-at-bol))
         (next-line-p (looking-back "^[\s\t]*" bol))
         (no-leading-whitespaces (looking-back "[\s\t]" bol))
         (query (save-excursion
                  (when next-line-p (forward-line -1) (end-of-line))
                  (buffer-substring (point-at-bol) (point))))
         (query (if (not git-complete-normalize-spaces) query
                  (git-complete--normalize-query query)))
         (whole-line (and (not next-line-p) (git-complete--collect-whole-line-candidates query)))
         (omni (git-complete--collect-omni-candidates query next-line-p no-leading-whitespaces))
         (items (nconc
                 (mapcar (lambda (e) (git-complete--make-popup-item e t)) whole-line)
                 (mapcar (lambda (e) (git-complete--make-popup-item e nil)) omni))))
    (cond (items
           (cl-destructuring-bind (whole-line-p str . newline)
               (popup-menu*
                items
                :scroll-bar t
                :isearch git-complete-enable-isearch
                :keymap git-complete--popup-menu-keymap)
             (when whole-line-p
               (delete-region (point-at-bol) (point)))
             (when git-complete-normalize-spaces
               (funcall indent-line-function))
             (insert str)
             (when newline
               (insert "\n")
               (funcall indent-line-function))
             (when git-complete-enable-autopair
               (save-excursion
                 (git-complete--insert-close-paren-if-needed str newline)))
             (when (if (eq git-complete-repeat-completion 'newline)
                       (looking-back "^[\s\t]*" (point-at-bol))
                     git-complete-repeat-completion)
               (let ((git-complete-fallback-function nil))
                 (git-complete)))))
          (git-complete-fallback-function
           (funcall git-complete-fallback-function))
          (t
           (message "No completions found.")))))

;; * provide

(provide 'git-complete)

;;; git-complete.el ends here
