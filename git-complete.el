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
;; URL: http://hins11.yu-yake.com/
;; Version: 0.0.0
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

;; 0.0.0 text release

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

(defcustom git-complete-line-completion-threshold 0.02
  "Threshold to filter the results from `git grep'. When 0.02 for
example, which is the default value, completion cnadidates which
occupy less than 2% among the grep results are dropped. Set this
variable greater than 1.0 to disable line completion."
  :type 'number
  :group 'git-complete)

(defcustom git-complete-omni-completion-threshold 0.005
  "Like `git-complete-line-completion-threshold' but used while
omni completion. Set this variable greater than 1.0 to disable
omni completion."
  :type 'number
  :group 'git-complete)

(defcustom git-complete-next-line-completion-threshold 0.3
  "Like `git-complete-omni-completion-threshold' but used while
next-line completion. Set this variable greater than 1.0 to
disable next-line completion"
  :type 'number
  :group 'git-complete)

(defcustom git-complete-repeat-line-completion t
  "When non-nil, do next-line completion again after
successful (next-)line completions."
  :type 'boolean
  :group 'git-complete)

(defcustom git-complete-repeat-omni-completion nil
  "When non-nil, do omni completion again after successful omni
completions."
  :type 'boolean
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

(defcustom git-complete-omni-completion-granularity 'subword
  "Specifies how to shorten query while omni-completion. Can be
either 'symbol, 'word or 'subword."
  :type 'symbol
  :group 'git-complete)

(defcustom git-complete-enable-isearch t
  "When non-nil, enable isearch by default on selecting completion
candidate."
  :type 'boolean
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

(defun git-complete--trim-spaces (str left right)
  "Remove leading/trailing whitespaces from STR."
  (replace-regexp-in-string
   (concat "^" (if left "[\s\t]*" "") "\\|" (if right "[\s\t]*" "") "$") "" str))

(defun git-complete--trim-candidate (str trim-query delimited)
  "Format candidate (= result from git-complete) by removing some
leading/trailing characters.

1. When TRIM-QUERY is non-nil, try to match TRIM-QUERY with STR,
and remove characters before the match-end (if no matches are
found, return an empty string). Otherwise remove leading
whitespaces.

2. When DELIMITED is non-nil and STR has more close parens than
open parens, remove all characters outside the unbalanced close
parens (close parens which do not have matching open
parens). Otherwise remove trailing whitespaces."
  (with-temp-buffer
    (save-excursion (insert str))
    (if trim-query
        (unless (search-forward trim-query nil t)
          (goto-char (point-max)))
      (skip-chars-forward "\s\t"))
    (delete-region (point-min) (point))
    (if delimited
        (ignore-errors
          (git-complete--up-list-unsafe)
          (delete-region (1- (point)) (point-max)))
      (goto-char (point-max))
      (skip-chars-backward "\s\t")
      (delete-region (point) (point-max)))
    (buffer-string)))

(defvar-local git-complete--root-dir nil)
(defun git-complete--root-dir ()
  "Find the root directory of this git repo. If the current
directory is not under a git repo, raises an error. This function
caches the result per buffer."
  (or git-complete--root-dir
      (setq git-complete--root-dir
            (and buffer-file-name (locate-dominating-file buffer-file-name ".git")))))

(defvar-local git-complete--extensions nil)
(defun git-complete--extensions ()
  "Returns a list of extensions to which candidates should be
limited."
  (and git-complete-limit-extension
       (or git-complete--extensions
           (setq git-complete--extensions
                 (or (assoc-default major-mode git-complete-major-mode-extensions-alist)
                     (list (file-name-extension buffer-file-name)))))))

;; * smart string substitution

(defun git-complete--parse-parens (str)
  "Internal function for `git-complete--replace-substring'. Parse
str and returns unbalanced parens in the form (((EXTRA_OPEN
. EXEPECTED_CLOSE) ...) . ((EXTRA_CLOSE . EXPECTED_OPEN) ...))."
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
results of `git-complete--parse-parens'."
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

(defun git-complete--replace-substring (from to replacement &optional oneline)
  "Replace region between FROM TO with REPLACEMENT and move the
point just after the inserted text. Unlike `replace-string', this
function tries to keep parenthesis balanced and indent the
inserted text (the behavior may disabled via customize
options). When ONELINE is specified, extra newlines are not
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
              (insert (if oneline "" "\n")
                      (if (or oneline (memq major-mode git-complete-lispy-modes)) "" "\n")
                      (apply 'string (mapcar 'cdr expected)))
              (setq skip-newline t))
            (while extra
              (if (looking-at (concat "[\s\t\n]*" (char-to-string (caar extra))))
                  (replace-match "")
                (save-excursion (goto-char from) (insert (char-to-string (cdar extra)))))
              (pop extra))))
        (unless (or oneline skip-newline) (insert "\n")))
      (setq end (point)))
    (indent-region from end)
    (unless oneline
      (forward-line 1)
      (funcall indent-line-function)
      (back-to-indentation))))

;; * get candidates via git grep

(defun git-complete--make-hist-trie (lst-of-lst)
  "Internal function for `git-complete--filter-candidates'. Make
a trie-like tree from a List[List[String]], whose nodes
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

(defun git-complete--filter-candidates-internal (trie threshold exact-match &optional node-key)
  "Internal function for
`git-complete--filter-candidates'. Traverse a trie returned by
`git-complete--make-hist-trie' and finds list of \"suitable\"
completion candidates. Optional arg NODE-KEY is used internally."
  (when (and trie (>= (cdr trie) threshold))
    (let ((children
           (apply 'nconc
                  (git-complete--maphash
                   (lambda (k v)
                     (funcall 'git-complete--filter-candidates-internal v threshold exact-match k))
                   (car trie)))))
      (cond (children
             (mapcar (lambda (x) (cons (if node-key (concat node-key (car x)) (car x)) (cdr x)))
                     children))
            ((and node-key (or (not exact-match) (string= node-key "")))
             (list (cons node-key (cdr trie))))))))

(defun git-complete--filter-candidates (lst threshold exact-match)
  "Internal function for `git-complete--get-candidates'. Extract
a list of \"suitable\" completion candidates of the form (STRING
. COUNT) from a string list LST, according to THRESHOLD. Unless
EXACT-MATCH is non-nil, substrings may also can be cnadidates."
  (let* ((trie (git-complete--make-hist-trie (mapcar (lambda (s) (split-string s "$\\|\\_>")) lst)))
         (threshold (* (or threshold 0) (cdr trie)))
         res)
    (git-complete--filter-candidates-internal trie threshold exact-match)))

(defun git-complete--get-candidates (query threshold whole-line-p nextline-p)
  "Get completion candidates with `git grep'."
  (when (and (git-complete--root-dir) (<= threshold 1.0))
    (let* ((default-directory (git-complete--root-dir))
           (ignore-case (if (eq git-complete-ignore-case 'dwim)
                            (not (string-match "[A-Z]" query))
                          git-complete-ignore-case))
           (extensions (git-complete--extensions))
           (command (format "git grep -F -h %s %s %s -- %s"
                            (if nextline-p "-A1" "")
                            (if ignore-case "-i" "")
                            (shell-quote-argument query)
                            (if extensions
                                (mapconcat (lambda (ext) (concat "\"*." ext "\"")) extensions " ")
                              "*")))
           (lines (split-string (shell-command-to-string command) "\n"))
           lst)
      (while (and lines (cdr lines))
        (when nextline-p (pop lines))   ; pop the first line
        (let ((str (git-complete--trim-candidate
                    (pop lines) (unless whole-line-p query) (not whole-line-p))))
          (unless (string= "" str) (push str lst)))
        (when nextline-p (pop lines)))  ; pop "--"
      (let ((filtered (git-complete--filter-candidates lst threshold whole-line-p)))
        (mapcar (lambda (x) (car x)) (sort filtered (lambda (a b) (> (cdr a) (cdr b)))))))))

;; * interface

(defvar git-complete--popup-menu-keymap
  (let ((kmap (copy-keymap popup-menu-keymap)))
    (define-key kmap (kbd "TAB") 'popup-select)
    kmap)
  "Keymap for git-complete popup menu.")

(defun git-complete--find-next-start (&optional current-start)
  "Returns next query start or nil."
  (save-excursion
    (let ((lim (point))
          (case-fold-search nil))
      (goto-char (or current-start (point-at-bol)))
      (cl-case git-complete-omni-completion-granularity
        ((symbol)  (and (search-forward-regexp ".\\_<" lim t) (point)))
        ((word)    (and (search-forward-regexp ".\\<" lim t) (point)))
        ((subword) (and (search-forward-regexp ".\\<\\|[a-zA-Z]\\([A-Z]\\)[a-z]" lim t)
                        (or (match-beginning 1) (point))))
        (t (error "invalid `git-complete-omni-completion-granularity'."))))))

(defun git-complete--internal (&optional omni-from)
  "Internal recursive function for git-complete."
  (let* ((next-line-p (looking-back "^[\s\t]*"))
         (threshold (cond (omni-from   git-complete-omni-completion-threshold)
                          (next-line-p git-complete-next-line-completion-threshold)
                          (t           git-complete-line-completion-threshold)))
         (query (save-excursion
                  (when next-line-p (forward-line -1) (end-of-line))
                  (git-complete--trim-spaces
                   (buffer-substring (or omni-from (point-at-bol)) (point)) t (null omni-from))))
         (candidates (when (string-match "\\_>" query)
                       (git-complete--get-candidates query threshold (null omni-from) next-line-p))))
    (cond (candidates
           (let ((completion (popup-menu* candidates :scroll-bar t
                                          :isearch git-complete-enable-isearch
                                          :keymap git-complete--popup-menu-keymap)))
             (git-complete--replace-substring
              (if omni-from (point) (point-at-bol)) (point) completion omni-from)
             (when (if omni-from
                       git-complete-repeat-omni-completion
                     git-complete-repeat-line-completion)
               (let ((git-complete-fallback-function nil))
                 (git-complete--internal)))))
          ((not next-line-p)
           (let ((next-from
                  (save-excursion
                    (cond (omni-from (git-complete--find-next-start omni-from))
                          (t (back-to-indentation) (point))))))
             (cond (next-from
                    (git-complete--internal next-from))
                   (git-complete-fallback-function
                    (funcall git-complete-fallback-function))
                   (t
                    (message "No completions found.")))))
          (git-complete-fallback-function
           (funcall git-complete-fallback-function))
          (t
           (message "No completions found.")))))

(defun git-complete ()
  "Complete the line at point with `git grep'."
  (interactive)
  (git-complete--internal))

;; * provide

(provide 'git-complete)

;;; git-complete.el ends here
