

(defcustom SML.indent-program
  "/usr/local/share/emacs/site-lisp/sml-indent/indent"
  "Program to use to indent your code"
  :group 'SML)

(defcustom SML.test-dir
  "/usr/local/share/emacs/site-lisp/sml-indent/tests"
  "Tests"
  :group 'SML)

(defun SML.indentation ()
  (save-excursion
    (beginning-of-line)
    (let ((beg (point)))
      (forward-to-indentation 0)
      (- (point) beg))))

(defun SML.in-indentation ()
  "t iff cursor is before the first non-whitespace char on a line"
  (let* ((beg (save-excursion (beginning-of-line) (point)))
         (s (buffer-substring beg (point))))
    (if (string-match "^\\s-*$" s) t nil)))

;; ;; If cursor is before any characters, tab sends you to the new indentation
;; ;; Otherwise, go to (oldCol - oldIndent + newIndent)
;; ;; oldCol - oldIndent is nonnegative
(defun SML.indent-buffer ()
  (interactive)
  (let* ((coding-system-for-write 'utf-8)
         (coding-system-for-read 'utf-8)
         (windows (get-buffer-window-list (current-buffer)))
         (window-info
          (mapcar (lambda (w) (list w (window-point w) (window-start w))) windows))
         (pt (point))
         (mk (mark))
         (old-indent (SML.indentation))
         (in-indentation (SML.in-indentation))
         (old-col (current-column))
         (ws (window-start))
         (ln (line-number-at-pos pt))
         (ln-str (int-to-string (line-number-at-pos pt))))
    (call-process-region (point-min) (point-max) SML.indent-program t t nil "-line" ln-str)
    (goto-char (point-min))
    (forward-line (1- ln))
    (if in-indentation (forward-to-indentation 0)
      (let ((new-indent (SML.indentation))
            (col (- old-col old-indent)))
        (assert (>= col 0))
        (forward-char (+ col new-indent))))
    (set-mark mk)
    (mapc (lambda (info)
            (let ((w (nth 0 info))
                  (pt (nth 1 info))
                  (st (nth 2 info)))
              ;;(set-window-point w pt)
              (set-window-start w st))) window-info)))

(defun SML.make-test ()
  (interactive)
  (if (not (file-exists-p SML.test-dir))
      (message "The test directory (SML.test-dir) does not exist.")
    (save-window-excursion
      (let* ((buf (current-buffer))
             (files (shell-command-to-string
                     (format "ls -1 %s/*.sml | sort | tail -1" SML.test-dir)))
             (_ (string-match "\\([[:digit:]]+\\).sml" files))
             (m (match-string 1 files))
             (int (string-to-number m))
             (int (1+ int))
             (file (format "%s/%03d.sml" SML.test-dir int)))
        (find-file file)
        (insert-buffer buf)
        (save-buffer)))))

(provide 'sml-indent)

;; (defun SML.indent-buffer ()
;;   (interactive)
;;   (let* ((pt (point))
;;          (old-indent (SML.indentation))
;;          (in-indentation (SML.in-indentation))
;;          (old-col (current-column))
;;          (ws (window-start))
;;          (ln (line-number-at-pos pt))
;;          (ln-str (int-to-string (line-number-at-pos pt))))
;;     (call-process-region (point-min) (point-max) t t nil "-line" ln-str)))
