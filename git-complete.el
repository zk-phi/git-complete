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
;; Version: 0.0.3
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
;; 0.0.1 add option git-complete-repeat-completion
;; 0.0.2 add option git-complete-threshold
;; 0.0.3 add option git-complete-omni-completion-type

;;; Code:

(require 'popup)
(require 'cl-lib)

(defgroup git-complete nil
  "Yet another completion engine powered by \"git grep\"."
  :group 'git-complete)

;; autopair / newline

(defcustom git-complete-enable-autopair t
  "When non-nil, `git-complete' assumes that the parens are
always balanced, and keep the balance on
completion (i.e. automatically insert close parens together with
open parens, and avoid inserting extra close parens)."
  :type 'boolean
  :group 'git-complete)

(defcustom git-complete-prefer-slurp-close-parens t
  "When non-nil and the autopair feature is enabled, git-complete
tries to delete extra close parens, instead of inserting extra
open parens."
  :type 'boolean
  :group 'git-complete)

(defcustom git-complete-empty-line-before-close-parens t
  "When non-nil and the autopair feature is enabled, git-complete
inserts an extra newline, before inserting extra close parens."
  :type 'boolean
  :group 'git-complete)

(defcustom git-complete-lispy-modes
  '(lisp-mode emacs-lisp-mode scheme-mode
              lisp-interaction-mode gauche-mode scheme-mode
              clojure-mode racket-mode egison-mode)
  "List of major-mode in which
`git-complete-empty-line-before-close-parens' should be disabled
automatically."
  :type '(repeat symbol)
  :group 'git-complete)

(defcustom git-complete-whole-line-completion-with-newline t
  "When non-nil, a newline is inserted after inserting a
whole-line completion. If the autopair feature is enabled, two or
more newlines may also be inserted before/after parens."
  :type 'boolean
  :group 'git-complete)

(defcustom git-complete-omni-completion-with-newline 'end-of-line
  "When non-nil, a newline is inserted after inserting an
\"exact\" omni completion. When `end-of-line', insert only if the
point is at the end of the line. If the autopair feature is
enabled, two or more newlines may also be inserted before/after
parens."
  :type 'boolean
  :group 'git-complete)

;; completion

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

(defcustom git-complete-enable-fuzzy-whole-line-completion t
  "When non-nil, enable fuzzy whole-line completion during
omni-completion."
  :type 'boolean
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

;; interface

(defcustom git-complete-enable-isearch t
  "When non-nil, enable isearch by default on selecting completion
candidate."
  :type 'boolean
  :group 'git-complete)

;; deprecated

(defvar git-complete-repeat-line-completion nil)
(defvar git-complete-repeat-omni-completion nil)
(defvar git-complete-omni-completion-threshold nil)
(defvar git-complete-line-completion-threshold nil)
(defvar git-complete-omni-completion-granularity nil)
(make-obsolete-variable 'git-complete-repeat-line-completion 'git-complete-repeat-completion "0.0.1")
(make-obsolete-variable 'git-complete-repeat-omni-completion 'git-complete-repeat-completion "0.0.1")
(make-obsolete-variable 'git-complete-omni-completion-threshold 'git-complete-threshold "0.0.2")
(make-obsolete-variable 'git-complete-line-completion-threshold 'git-complete-whole-line-completion-threshold "0.0.2")
(make-obsolete-variable 'git-complete-omni-completion-granularity 'git-complete-omni-completion-type "0.0.3")

;; * utilities

(defun git-complete--maphash (fn hash)
  "Like `maphash' but returns a list of returned values."
  (let (lst)
    (maphash (lambda (k v) (push (funcall fn k v) lst)) hash)
    lst))

(defun git-complete--up-list-unsafe ()
  "Like `up-list', but not aware of `forward-sexp-function'."
  (goto-char (or (scan-lists (point) 1 1) (buffer-end 1))))

(defun git-complete--trim-spaces (str left right)
  "Remove leading/trailing whitespaces from STR."
  (replace-regexp-in-string
   (concat "^" (if left "[\s\t]*" "") "\\|" (if right "[\s\t]*" "") "$") "" str))

(defun git-complete--trim-candidate (str omni-query)
  "Remove some leading/trailing characters from each lines of
`git grep' result, as follows:

1. If OMNI-QUERY is nil, just remove leading and trailing
   whitespaces.

2. If OMNI-QUERY is non-nil:

   i. Search OMNI-QUERY inside STR, and remove characters before
      the query and the query itself (if no matches are found,
      return an empty string) and delete all leading whitespaces
      except for one.

   ii. When STR has more close parens than open parens, remove
       all characters outside the innermost unbalanced close
       paren (close paren which do not have matching open
       paren). Then delete all trailing whitespaces."
  (with-temp-buffer
    (save-excursion (insert str))
    (if omni-query
        (if (search-forward omni-query nil t)
            (and (looking-at "\\([\s\t]\\)+[\s\t]")
                 (goto-char (match-end 1)))
          (goto-char (point-max)))
      (skip-chars-forward "\s\t"))
    (delete-region (point-min) (point))
    (when omni-query
      (ignore-errors
        (git-complete--up-list-unsafe)
        (delete-region (1- (point)) (point-max))))
    (goto-char (point-max))
    (skip-chars-backward "\s\t")
    (delete-region (point) (point-max))
    (buffer-string)))

(defvar-local git-complete--root-dir nil) ; cache
(defun git-complete--root-dir ()
  "Find the root directory of this git repo. If the current
directory is not under a git repo, raises an error. This function
is cached, thus

1. Open a file NOT under a git repo
2. `git init' the directory
3. invoke `git-complete'

fails (but it may be a very rare case, and `git-complete' does
not work well, even if it does not raise an error)."
  (or git-complete--root-dir
      (setq git-complete--root-dir
            (and buffer-file-name (locate-dominating-file buffer-file-name ".git")))))

(defvar-local git-complete--extensions nil) ; cache
(defun git-complete--extensions ()
  "Returns a list of extensions to which candidates should be
limited. This function is cached, thus changing extensions after
opening files may lead an unexpected result."
  (and git-complete-limit-extension
       (or git-complete--extensions
           (setq git-complete--extensions
                 (or (assoc-default major-mode git-complete-major-mode-extensions-alist)
                     (list (file-name-extension buffer-file-name)))))))

(defun git-complete--beginning-of-next-word (current-start)
  "Returns the beginning position of the next \"word\" (according
to `git-complete-omni-completion-type') in the line, or nil if
not found."
  (save-excursion
    (let ((lim (point))
          (case-fold-search nil))
      (goto-char (or current-start (point-at-bol)))
      (cl-case (or git-complete-omni-completion-granularity ; backward compatiblity
                   git-complete-omni-completion-type)
        ((symbol)  (and (search-forward-regexp ".\\_<" lim t) (point)))
        ((word)    (and (search-forward-regexp ".\\<" lim t) (point)))
        ((subword) (and (search-forward-regexp ".\\<\\|[a-zA-Z]\\([A-Z]\\)[a-z]" lim t)
                        (or (match-beginning 1) (point))))
        (t nil)))))

;; * smart string substitution

(defun git-complete--find-unmatched-parens (str)
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

(defun git-complete--diff-unmatched-parens (lst1 lst2)
  "Internal function for
`git-complete--replace-substring'. Compute difference of two
results from `git-complete--find-unmatched-parens', in the
form (((EXTRA_OPEN . EXPECTED_CLOSE) ...) . ((EXTRA_CLOSE
. EXPECTED_OPEN) ...)).

Example:
- result 1: (git-complete--find-unmatched-parens \"([}\")
  => (((?\[ . ?\]) (?\( . ?\))) . ((?\} . ?\{)))
- result 2: (git-complete--find-unmatched-parens \"((]\")
  => (((?\( . ?\)) (?\( . ?\))) . ((?\] . ?\[)))
- difference: (git-complete--diff-unmatched-parens <result 1> <result 2>)
  => (((?\{ . ?\}) (?\( . ?\))) . ((?\] . ?\[) (?\] . ?\[)))

Which means, when replacing \"([}\" with \"((]\", we need an
extra \"})\" and a \"[[\" to keep the balance. Since:
- } is deleted (thus there will be an extra {, which expects a } to be inserted)
- ( is added (thus there will be an extra (, which expects a ) to be inserted)
- [ is deleted
- ] is added"
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

(defun git-complete--replace-substring (from to replacement &optional newline)
  "Replace region between FROM TO with REPLACEMENT and move the
point just after the inserted text. Unlike `replace-string', this
function tries to keep the parenthesis balanced by inserting or
deleting some parens (along with some newlines), and indent the
inserted text (the behavior may disabled via customize
options). When NEWLINE is specified, extra newlines may be
inserted."
  (let ((deleted (buffer-substring from to)) end)
    (delete-region from to)
    (insert replacement)
    ;; (point is kept at the end of the replacement text)
    (save-excursion
      (let ((newline (if newline "\n" ""))
            close-parens-are-inserted)
        (when git-complete-enable-autopair
          (let* ((res (git-complete--diff-unmatched-parens
                       (git-complete--find-unmatched-parens deleted)
                       (git-complete--find-unmatched-parens replacement)))
                 (closes (car res))
                 (opens (cdr res)))
            (when closes
              (insert newline)
              (when (and git-complete-empty-line-before-close-parens
                         (not (memq major-mode git-complete-lispy-modes)))
                (insert newline))
              (insert (mapconcat 'char-to-string (mapcar 'cdr closes) newline))
              (setq close-parens-are-inserted t))
            (while opens
              (if (and git-complete-prefer-slurp-close-parens
                       (looking-at (concat "[\s\t\n]*" (char-to-string (caar opens)) "\n?")))
                  (replace-match "")
                (save-excursion (goto-char from) (insert (char-to-string (cdar opens)))))
              (pop opens))))
        (unless close-parens-are-inserted (insert newline)))
      (setq end (point)))
    (indent-region from end)
    (when newline
      (forward-line 1)
      (funcall indent-line-function)
      (back-to-indentation))))

;; * get candidates via git grep

(defun git-complete--make-hist-trie (lst-of-lst)
  "Internal function for `git-complete--filter-candidates'. Take
a list of completions and make a trie-like tree. Each completions
must be a list of words, with an empty string supplied at the end
of the list. Returned trie is a cons cell of the form (CHILDREN
. COUNT), where CHILDREN is a hash map of String -> Trie, and
COUNT is the number of leaf nodes under the node."
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
  "FOR DEBUG USE. Turn a trie into a human-readable list of
string."
  (let ((res nil))
    (maphash (lambda (k v) (push (cons k (git-complete--dump-trie v)) res)) (car trie))
    (cons (cdr trie) res)))

;; * trim and filter candidates

(defun git-complete--filter-candidates-internal (trie threshold exact-p &optional node-key)
  "Internal recursive function for
`git-complete--filter-candidates'. Traverse a trie returned by
`git-complete--make-hist-trie' and finds a list of \"suitable\"
completion candidates due to THRESHOLD and EXACT-P, returned as a
list of the form ((STRING EXACT-P . COUNT) ...). EXACT-P is
non-nil if the completion ends at the end of the line. Optional
arg NODE-KEY is used internally, during the traversal."
  (when (and trie (>= (cdr trie) threshold))
    (let ((children
           (apply 'nconc
                  (git-complete--maphash
                   (lambda (k v)
                     (funcall 'git-complete--filter-candidates-internal v threshold exact-p k))
                   (car trie)))))
      (cond (children
             (mapcar (lambda (x) (cons (if node-key (concat node-key (car x)) (car x)) (cdr x)))
                     children))
            ((null node-key)
             nil)
            ((string= node-key "")
             (list (cons node-key (cons t (cdr trie)))))
            ((not exact-p)
             (list (cons node-key (cons nil (cdr trie)))))))))

(defun git-complete--filter-candidates (lst &optional omni-query threshold filter-regex)
  "Extract a sorted list of \"suitable\" completion candidates of
the form (STRING WHOLE-LINE-P EXACT-P . COUNT) from a string list
LST. WHOLE-LINE-P is non-nil, if OMNI-QUERY is
unspecified. Otherwise, if OMNI-QUERY is specified, candidates
are trimmed by `git-complete--trim-candidate' before
filtering. If WHOLE-LINE-P is unspecified, candidates are not
trimmed and results are limited to \"exact\" matches. When
FILTER-REGEX is specified, candidates not mathing the regex are
dropped before filtering."
  (setq lst
        (cl-remove-if
         (lambda (s) (string= s ""))
         (mapcar (lambda (s)
                   (if (or (null filter-regex) (string-match filter-regex s))
                       (git-complete--trim-candidate s omni-query)
                     ""))
                 lst)))
  (let* ((trie (git-complete--make-hist-trie (mapcar (lambda (s) (split-string s "$\\|\\_>")) lst)))
         (threshold (* threshold (cdr trie)))
         (filtered (git-complete--filter-candidates-internal trie threshold (null omni-query))))
    (mapcar (lambda (e) `(,(car e) ,(null omni-query) . ,(cdr e)))
            (sort filtered (lambda (a b) (> (cddr a) (cddr b)))))))

(defun git-complete--get-query-candidates (query nextline-p)
  "A wrapper function of `git grep'. Call `git grep' with QUERY,
and returns the result as a list of strings."
  (when (git-complete--root-dir)
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
        (push (pop lines) lst)
        (when nextline-p (pop lines)))  ; pop "--"
      lst)))

;; * interface

(defvar git-complete--popup-menu-keymap
  (let ((kmap (copy-keymap popup-menu-keymap)))
    (define-key kmap (kbd "TAB") 'popup-select)
    kmap)
  "Keymap for git-complete popup menu.")

(defun git-complete--internal (&optional omni-from)
  "Internal recursive function for git-complete. Recursion is
used during omni-completion, with OMNI-FROM argument specified."
  (let* ((next-line-p
          (looking-back "^[\s\t]*"))
         (query
          (save-excursion
            (when next-line-p (forward-line -1) (end-of-line))
            (git-complete--trim-spaces
             (buffer-substring (or omni-from (point-at-bol)) (point)) t nil)))
         (candidates
          (when (string-match "\\_>" query)
            (git-complete--get-query-candidates query next-line-p)))
         (filtered
          (nconc
           ;; whole-line matches
           (when (or (null omni-from)
                     next-line-p
                     git-complete-enable-fuzzy-whole-line-completion)
             (git-complete--filter-candidates
              candidates nil
              (or git-complete-line-completion-threshold ; backward compatiblity
                  (if next-line-p
                      git-complete-next-line-completion-threshold
                    git-complete-whole-line-completion-threshold))
              (when (and omni-from (not next-line-p))
                ;; fuzzy whole-line-completion
                (mapconcat 'identity
                           (mapcar 'regexp-quote
                                   (split-string
                                    (buffer-substring (point-at-bol) omni-from))) ".*"))))
           ;; omni matches
           (unless next-line-p
             (git-complete--filter-candidates
              candidates query
              (or git-complete-omni-completion-threshold ; backward compatiblity
                  git-complete-threshold))))))
    (cond (filtered
           (cl-destructuring-bind (str whole-line-p exact-p . count)
               (popup-menu*
                (mapcar (lambda (e) (popup-make-item (car e) :value e)) filtered)
                :scroll-bar t
                :isearch git-complete-enable-isearch
                :keymap git-complete--popup-menu-keymap)
             (git-complete--replace-substring
              (if whole-line-p (point-at-bol) (point)) (point) str
              (or (and git-complete-whole-line-completion-with-newline
                       whole-line-p)
                  (and git-complete-omni-completion-with-newline
                       exact-p
                       (or (not (eq git-complete-omni-completion-with-newline 'end-of-line))
                           (eolp)))))
             (when (or (if whole-line-p
                           git-complete-repeat-line-completion
                         git-complete-repeat-omni-completion) ; backward compatiblity
                       (if (eq git-complete-repeat-completion 'newline)
                           (looking-back "^[\s\t]*")
                         git-complete-repeat-completion))
               (let ((git-complete-fallback-function nil))
                 (git-complete--internal)))))
          ((and git-complete-omni-completion-type)
           (let ((next-from
                  (save-excursion
                    (cond (omni-from
                           (git-complete--beginning-of-next-word omni-from))
                          (t
                           (when next-line-p (forward-line -1))
                           (back-to-indentation)
                           (point))))))
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
