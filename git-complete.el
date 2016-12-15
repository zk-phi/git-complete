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

(defun git-complete--get-candidates (query)
  (let* ((command (format "git grep -F -h %s" (shell-quote-argument query)))
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
  (let ((query (git-complete--trim-spaces (buffer-substring (point-at-bol) (point-at-eol)))))
    (unless (string= query "")
      (let* ((candidates (git-complete--get-candidates query))
             (completion (popup-menu* candidates :scroll-bar t :isearch t)))
        (goto-char (point-at-bol))
        (kill-line)
        (insert completion)
        (save-excursion (funcall indent-line-function))
        (insert "\n")))))

(provide 'git-complete)
