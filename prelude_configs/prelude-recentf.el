(require 'recentf)

(recentf-mode 1)

;; (defun recentf-open-files-compl ()
;;   "open recent files. In ido style if applicable --lgfang"
;;   (interactive)
;;   (let* ((path-table (mapcar (lambda (x) (cons (file-name-nondirectory x) x)) recentf-list))
;;          (file-list (mapcar (lambda (x) (file-name-nondirectory x)) recentf-list))
;;          (complete-fun (if (require 'ido nil t) 'ido-completing-read 'completing-read))
;;          (fname (funcall complete-fun "File Name: " file-list)))
;;     (find-file (cdr (assoc fname path-table)))))

(defun recentf-open-files-compl ()
  (interactive)
  (let* ((all-files recentf-list)
         (tocpl (mapcar (function (lambda (x) (cons (file-name-nondirectory x) x))) all-files))
         (prompt (append '("File name: ") tocpl))
         (fname (completing-read (car prompt) (cdr prompt) nil nil)))
    (find-file (cdr (assoc  fname tocpl)))))

(global-set-key [(control x)(control r)] 'recentf-open-files-compl)
