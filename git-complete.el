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
;; Version: 0.0.5
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

;;; Code:

(require 'popup)
(require 'cl-lib)

(defgroup git-complete nil
  "Yet another completion engine powered by \"git grep\"."
  :group 'git-complete)

(defcustom git-complete-enable-autopair t
  "When non-nil, `git-complete' assumes that the parens are
always balanced, and keep the balance on
completion (i.e. automatically insert close parens together with
open parens, and avoid inserting extra close parens)."
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

(defcustom git-complete-candidate-limit 1000
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

(defun git-complete--trim-candidate (str query)
  "Search OMNI-QUERY inside STR, and remove characters before the
   query and the query itself (if no matches are found, return an
   empty string). If STR has more close parens than open parens,
   then remove all characters after the last matching close
   paren."
  (with-temp-buffer
    (save-excursion (insert str))
    (or (search-forward query nil t)
        (goto-char (point-max)))
    (delete-region (point-min) (point))
    (ignore-errors
      (git-complete--up-list-unsafe)
      (forward-char -1)
      (skip-chars-backward "\s\t")
      (delete-region (point) (point-max)))
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

(defun git-complete--beginning-of-next-word (current-start)
  "Returns the beginning position of next word (according to
git-complete-omni-completion-type) in the line, or nil if not
found."
  (save-excursion
    (let ((lim (point))
          (case-fold-search nil))
      (goto-char (or current-start (point-at-bol)))
      (cl-case git-complete-omni-completion-type
        ((symbol)  (and (search-forward-regexp ".\\_<" lim t) (point)))
        ((word)    (and (search-forward-regexp ".\\<" lim t) (point)))
        ((subword) (and (search-forward-regexp ".\\<\\|[a-zA-Z]\\([A-Z]\\)[a-z]" lim t)
                        (or (match-beginning 1) (point))))
        (t nil)))))

;; * smart string substitution

(defun git-complete--parse-parens (str)
  "Internal function for `git-complete--replace-substring'. Parse
str and returns unbalanced parens in the form (((EXTRA_OPEN
. EXEPECTED_CLOSE) ...) . ((EXTRA_CLOSE . EXPECTED_OPEN) ...)).

Example:
- ()    => (nil . nil) since parens are balanced
- f(o)o => (nil . nil) non-paren characters does not affects the result
- [     => (((?\[ . ?\])) . nil) since we have an extra \"[\"
- [}    => (((?\[ . ?\])) . ((?\} . ?\{))) since we have another extra \"}\""
  (let (opens closes syntax char)
    (with-temp-buffer
      (save-excursion (insert str))
      (while (progn (skip-syntax-forward "^\\\\()") (not (eobp)))
        (setq char   (char-after)
              syntax (aref (syntax-table) char)) ; (CLASS . PARTNER)
        (cl-case (car syntax)
          ((4)                          ; (string-to-syntax "(")
           (push (cons char (cdr syntax)) opens))
          ((5)                          ; (string-to-syntax ")")
           (if (and opens (= (cdar opens) char))
               (pop opens)
             (push (cons char (cdr syntax)) closes)))
          ((9)                          ; (string-to-syntax "\\")
           (forward-char 1)))
        (forward-char 1)))
    (cons opens closes)))

(defun git-complete--diff-parens (lst1 lst2)
  "Internal function for
`git-complete--replace-substring'. Compute difference of two
results of `git-complete--parse-parens'.

Example:
- (git-complete--diff-parens
   (git-complete--parse-parens \"(\")
   (git-complete--parse-parens \"}\")) => (nil . ((?\} . ?\{) (?\) . ?\()))
When replacing \"(\" with \"}\", we need an extra \"{\" and a
\"(\", to keep the balance."
  (let ((existing-opens (car lst1))
        (added-opens (car lst2))
        (existing-closes (cdr lst1))
        (added-closes (cdr lst2))
        deleted-opens deleted-closes)
    ;; open parens
    (while (and existing-opens added-opens)
      (if (= (caar existing-opens) (caar added-opens))
          (progn (pop existing-opens) (pop added-opens))
        (push (pop existing-opens) deleted-opens)))
    (when existing-opens
      (setq deleted-opens (nconc (nreverse existing-opens) deleted-opens)))
    ;; close parens
    (while (and existing-closes added-closes)
      (if (= (caar existing-closes) (caar added-closes))
          (progn (pop existing-closes) (pop added-closes))
        (push (pop existing-closes) deleted-closes)))
    (when existing-closes
      (setq deleted-closes (nconc (nreverse existing-closes) deleted-closes)))
    ;; result
    (cons (nconc (mapcar (lambda (a) (cons (cdr a) (car a))) deleted-closes) added-opens)
          (nreverse (nconc (mapcar (lambda (a) (cons (cdr a) (car a))) deleted-opens) added-closes)))))

(defun git-complete--replace-substring (from to replacement &optional no-newline)
  "Replace region between FROM TO with REPLACEMENT and move the
point just after the inserted text. Unlike `replace-string', this
function tries to keep parenthesis balanced and indent the
inserted text (the behavior may disabled via customize
options). When NO-NEWLINE is specified, extra newlines are not
inserted."
  (let ((deleted (buffer-substring from to)) end)
    (delete-region from to)
    (setq from (goto-char from))
    (insert replacement)
    (save-excursion
      (let (skip-newline)
        (when git-complete-enable-autopair
          (let* ((res (git-complete--diff-parens
                       (git-complete--parse-parens deleted)
                       (git-complete--parse-parens replacement)))
                 (expected (car res))
                 (extra (cdr res)))
            (when expected
              (insert (if no-newline "" "\n")
                      (if (or no-newline (memq major-mode git-complete-lispy-modes)) "" "\n")
                      (apply 'string (mapcar 'cdr expected)))
              (setq skip-newline t))
            (while extra
              (if (looking-at (concat "[\s\t\n]*" (char-to-string (caar extra))))
                  (replace-match "")
                (save-excursion (goto-char from) (insert (char-to-string (cdar extra)))))
              (pop extra))))
        (unless (or no-newline skip-newline) (insert "\n")))
      (setq end (point)))
    (indent-region from end)
    (unless no-newline
      (forward-line 1)
      (funcall indent-line-function)
      (back-to-indentation))))

;; * trim and filter candidates

(defun git-complete--make-hist-trie (lst-of-lst)
  "Internal function for `git-complete--filter-candidates'. Takes
a List[List[String]], and makes a trie-like tree, whose nodes
are (CHILDREN . COUNT) where CHILDREN is a hash map of String ->
Node. Last element in each List[String] is expected to be an
empty string."
  (let ((trie (cons (make-hash-table :test 'equal) 0)) current-node child-node)
    (dolist (lst lst-of-lst)
      (setq current-node trie)
      (cl-incf (cdr current-node))
      (dolist (elem lst)
        (setq child-node (gethash elem (car current-node)))
        (cond (child-node
               (setq current-node child-node)
               (cl-incf (cdr current-node)))
              (t
               (setq child-node (cons (make-hash-table :test 'equal) 1))
               (puthash elem child-node (car current-node))
               (setq current-node child-node)))))
    trie))

(defun git-complete--dump-trie (trie)
  "FOR DEBUG USE."
  (let ((res nil))
    (maphash (lambda (k v) (push (cons k (git-complete--dump-trie v)) res)) (car trie))
    (cons (cdr trie) res)))

(defun git-complete--filter-candidates-internal (trie threshold exact-only &optional node-key)
  "Internal recursive function for
`git-complete--filter-candidates'. Traverse a trie returned by
`git-complete--make-hist-trie' and finds list of \"suitable\"
completion candidates according to THRESHOLD and EXACT-ONLY,
returned as a list of the form ((STRING . COUNT) ...). Optional
arg NODE-KEY is used internally."
  (when (and trie (>= (cdr trie) threshold))
    (let ((children
           (apply 'nconc
                  (git-complete--maphash
                   (lambda (k v)
                     (funcall 'git-complete--filter-candidates-internal v threshold exact-only k))
                   (car trie)))))
      (cond (children
             (mapcar (lambda (x) (cons (if node-key (concat node-key (car x)) (car x)) (cdr x)))
                     children))
            ((null node-key)
             nil)
            ((or (string= node-key "") (not exact-only))
             (list (cons node-key (cdr trie))))))))

(defun git-complete--filter-candidates (lst &optional exact-only threshold)
  "Extract a sorted (by occurrences) list of \"suitable\"
completion candidates from a string list LST. Unlessj EXACT-ONLY
is non-nil, strings can be shortened to meet THRESHOLD."
  (let* ((trie (git-complete--make-hist-trie (mapcar (lambda (s) (split-string s "$\\|\\_>")) lst)))
         (threshold (* threshold (cdr trie)))
         (filtered (git-complete--filter-candidates-internal trie threshold exact-only)))
    (mapcar 'car (sort filtered (lambda (a b) (> (cdr a) (cdr b)))))))

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
         (lines (split-string (shell-command-to-string command) "\n"))
         (count 0)
         lst)
    (while (and lines (cdr lines))
      (when nextline-p (pop lines))     ; pop the first line
      (push (pop lines) lst)
      (when nextline-p (pop lines))     ; pop "--"
      (when (> (setq count (1+ count)) git-complete-candidate-limit)
        (setq lines nil lst nil)))
    lst))

(defun git-complete-ripgrep (query extensions nextline-p ignore-case-p)
  (let* ((command (format "rg --no-heading --no-filename --no-line-number -F %s %s %s -- %s"
                          (if nextline-p "-A1" "")
                          (if ignore-case-p "-i" "")
                          (if extensions
                              (mapconcat (lambda (ext) (concat "-g '*." ext "'")) extensions " ")
                            " ")
                          query))
         (lines (split-string (shell-command-to-string command) "\n"))
         (count 0)
         lst)
    (while (and lines (cdr lines))
      (when nextline-p (pop lines))     ; pop the first line
      (push (pop lines) lst)
      (when nextline-p (pop lines))     ; pop "--"
      (when (> (setq count (1+ count)) git-complete-candidate-limit)
        (setq lines nil lst nil)))
    lst))

;; * interface

(defvar git-complete--popup-menu-keymap
  (let ((kmap (copy-keymap popup-menu-keymap)))
    (define-key kmap (kbd "TAB") 'popup-select)
    kmap)
  "Keymap for git-complete popup menu.")

(defun git-complete--collect-candidates (next-line-p no-leading-whitespaces &optional omni-from)
  "Collect candidates and returns as a pair of 1. a list of
whole-line candidates and 2. a list of omni candidates."
  (let* ((query (save-excursion
                  (when next-line-p (forward-line -1) (end-of-line))
                  (git-complete--normalize-query
                   (buffer-substring (or omni-from (point-at-bol)) (point)))))
         (candidates (when (string-match "\\_>" query) ; at least one symbol required
                       (git-complete--get-grep-result query next-line-p)))
         (whole-line (when (or next-line-p (null omni-from))
                       (git-complete--filter-candidates
                        (cl-remove-if
                         (lambda (s) (string= s ""))
                         (mapcar (lambda (s) (git-complete--normalize-candidate s t)) candidates))
                        t
                        (if next-line-p
                            git-complete-next-line-completion-threshold
                          git-complete-whole-line-completion-threshold))))
         (omni (unless next-line-p
                 (git-complete--filter-candidates
                  (cl-remove-if
                   (lambda (s) (string= s ""))
                   (mapcar (lambda (s)
                             (git-complete--normalize-candidate
                              (git-complete--trim-candidate s query) no-leading-whitespaces))
                           candidates))
                  nil
                  git-complete-threshold))))
    (or (and (or whole-line omni)
             (cons whole-line omni))
        (and git-complete-omni-completion-type
             (let ((next-from
                    (save-excursion
                      (cond (omni-from (git-complete--beginning-of-next-word omni-from))
                            (next-line-p (forward-line -1) (back-to-indentation) (point))
                            (t (back-to-indentation) (point))))))
               (when next-from
                 (git-complete--collect-candidates
                  next-line-p no-leading-whitespaces next-from)))))))

;; newline を入れるべきかどうかは、 whole-line かどうかの問題ではない
;; よなあ

;; git-complete--filter-candidates あたりのおしゃれ挙動 (クエリの有無
;; によって…とか) を git-complete 側に寄せていけば、 company-backend
;; 化できる気がする

(defun git-complete ()
  "Complete the line at point with `git grep'."
  (interactive)
  (let* ((bol (point-at-bol))
         (next-line-p (looking-back "^[\s\t]*" bol))
         (no-leading-whitespaces (looking-back "[\s\t]" bol))
         (candidates (git-complete--collect-candidates next-line-p no-leading-whitespaces))
         (items (nconc
                 (mapcar (lambda (e) (popup-make-item e :value (cons t e))) (car candidates))
                 (mapcar (lambda (e) (popup-make-item e :value (cons nil e))) (cdr candidates)))))
    (cond (items
           (cl-destructuring-bind (whole-line-p . str)
               (popup-menu*
                items
                :scroll-bar t
                :isearch git-complete-enable-isearch
                :keymap git-complete--popup-menu-keymap)
             (git-complete--replace-substring
              (if whole-line-p (point-at-bol) (point)) (point) str (not whole-line-p))
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
