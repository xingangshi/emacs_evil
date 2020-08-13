;; Enable evil
(add-to-list 'load-path "~/.emacs.d/extra/evil")

(setq evil-toggle-key "")       ; remove default evil-toggle-key C-z, manually setup later
(setq evil-want-C-i-jump nil)   ; don't bind [tab] to evil-jump-forward

(require 'evil)
(evil-mode 1)

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/"))
(package-initialize)

(add-to-list 'load-path "~/.emacs.d/extra/emacs-vim-modeline")
(require 'vim-modeline)
(add-to-list 'find-file-hook 'vim-modeline/do)

(global-linum-mode t)

;; (load-theme 'molokai t)
(load-theme 'wombat t)

(add-to-list 'load-path "~/.emacs.d/extra/extra")

(require 'sr-speedbar)
(setq speedbar-show-unknown-files t)
(setq speedbar-use-images nil)
(setq sr-speedbar-width 35)
(setq sr-speedbar-auto-refresh nil)
(setq sr-speedbar-right-side nil)
(setq speedbar-tag-hierarchy-method nil)
(make-face 'speedbar-face)
(setq speedbar-mode-hook '(lambda () (buffer-face-set 'speedbar-face)))
(global-set-key (kbd "C-x s") 'sr-speedbar-toggle)

;; remove all keybindings from insert-state keymap, use emacs-state when editing
(setcdr evil-insert-state-map nil)

;; ESC to switch back normal-state
(define-key evil-insert-state-map [escape] 'evil-normal-state)

;; TAB to indent in normal-state
;;(define-key evil-normal-state-map (kbd "TAB") 'indent-for-tab-command)

;; Use j/k to move one visual line insted of gj/gk
(define-key evil-normal-state-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
(define-key evil-normal-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)
(define-key evil-motion-state-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
(define-key evil-motion-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)


;; Automatically set screen title
;; ref http://vim.wikia.com/wiki/Automatically_set_screen_title
;; FIXME: emacsclient in xterm will have problem if emacs daemon start in screen
(defun update-title ()
  (interactive)
  (if (getenv "STY")    ; check whether in GNU screen
    (send-string-to-terminal (concat "\033k\033\134\033k" "Emacs("(buffer-name)")" "\033\134"))
    (send-string-to-terminal (concat "\033]2; " "Emacs("(buffer-name)")" "\007"))))
(add-hook 'post-command-hook 'update-title)


;; Use xsel to access the X clipboard
;; From https://hugoheden.wordpress.com/2009/03/08/copypaste-with-emacs-in-terminal/
(unless window-system
  (when (getenv "DISPLAY")
    ;; Callback for when user cuts
    (defun xsel-cut-function (text &optional push)
      ;; Insert text to temp-buffer, and "send" content to xsel stdin
      (with-temp-buffer
        (insert text)
        ;; Use primary the primary selection
        ;; mouse-select/middle-button-click
        (call-process-region (point-min) (point-max) "xsel" nil 0 nil "--primary" "--input")))
    ;; Call back for when user pastes
    (defun xsel-paste-function()
      ;; Find out what is current selection by xsel. If it is different
      ;; from the top of the kill-ring (car kill-ring), then return
      ;; it. Else, nil is returned, so whatever is in the top of the
      ;; kill-ring will be used.
      (let ((xsel-output (shell-command-to-string "xsel --primary --output")))
        (unless (string= (car kill-ring) xsel-output)
          xsel-output)))
    ;; Attach callbacks to hooks
    (setq interprogram-cut-function 'xsel-cut-function)
    (setq interprogram-paste-function 'xsel-paste-function)))


;; Mimic Vim's set paste
;; From http://stackoverflow.com/questions/18691973/is-there-a-set-paste-option-in-emacs-to-paste-paste-from-external-clipboard
(defvar ttypaste-mode nil)
(add-to-list 'minor-mode-alist '(ttypaste-mode " Paste"))
(defun ttypaste-mode ()
  (interactive)
  (let ((buf (current-buffer))
        (ttypaste-mode t))
    (with-temp-buffer
      (let ((stay t)
            (text (current-buffer)))
        (redisplay)
        (while stay
               (let ((char (let ((inhibit-redisplay t)) (read-event nil t 0.1))))
                 (unless char
                   (with-current-buffer buf (insert-buffer-substring text))
                   (erase-buffer)
                   (redisplay)
                   (setq char (read-event nil t)))
                 (cond
                   ((not (characterp char)) (setq stay nil))
                   ((eq char ?\r) (insert ?\n))
                   ((eq char ?\e)
                    (if (sit-for 0.1 'nodisp) (setq stay nil) (insert ?\e)))
                   (t (insert char)))))
        (insert-buffer-substring text)))))

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
  '(custom-safe-themes
     (quote
       ("c3c0a3702e1d6c0373a0f6a557788dfd49ec9e66e753fb24493579859c8e95ab" "d8dc153c58354d612b2576fea87fe676a3a5d43bcc71170c62ddde4a1ad9e1fb" default)))
  '(org-agenda-files (quote ("~/self/org/orgmode.org")))
  '(package-selected-packages
     (quote
       (molokai-theme powerline paradox spinner lv parent-mode projectile pkg-info epl flx f highlight smartparens iedit anzu evil goto-chg undo-tree dash s bind-map bind-key packed helm avy helm-core popup async ## abyss-theme ws-butler winum which-key volatile-highlights vi-tilde-fringe uuidgen use-package toc-org spaceline restart-emacs request rainbow-delimiters popwin persp-mode pcre2el org-bullets open-junk-file neotree move-text macrostep lorem-ipsum linum-relative link-hint indent-guide hydra hungry-delete hl-todo highlight-parentheses highlight-numbers highlight-indentation helm-themes helm-swoop helm-projectile helm-mode-manager helm-make helm-flx helm-descbinds helm-ag google-translate golden-ratio flx-ido fill-column-indicator fancy-battery eyebrowse expand-region exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-search-highlight-persist evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-lisp-state evil-indent-plus evil-iedit-state evil-exchange evil-escape evil-ediff evil-args evil-anzu eval-sexp-fu elisp-slime-nav dumb-jump diminish define-word column-enforce-mode clean-aindent-mode auto-highlight-symbol auto-compile aggressive-indent adaptive-wrap ace-window ace-link ace-jump-helm-line))))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )

;; 打开 org-indent mode
(setq org-startup-indented t)

;; 设置 todo keywords
(setq org-todo-keywords '((sequence "NEXT(n)" "TODO(t)" "STARTED(s)" "WAITING(w@/!)" "|" "DONE(d!)" "CANCELLED(c@)")))

;; 调试好久的颜色，效果超赞！todo keywords 增加背景色
(setf org-todo-keyword-faces '(("TODO" . (:foreground "white" :background "#95A5A6" :weight bold))
                               ("HAND" . (:foreground "white" :background "#2E8B57" :weight bold))
                               ("DONE" . (:foreground "white" :background "#3498DB" :weight bold))))

;; agenda 里面时间块彩色显示
;; From: https://emacs-china.org/t/org-agenda/8679/3
(defun ljg/org-agenda-time-grid-spacing ()
  "Set different line spacing w.r.t. time duration."
  (save-excursion
    (let* ((background (alist-get 'background-mode (frame-parameters)))
           (background-dark-p (string= background "dark"))
           (colors (list "#1ABC9C" "#2ECC71" "#3498DB" "#9966ff"))
           pos
           duration)
      (nconc colors colors)
      (goto-char (point-min))
      (while (setq pos (next-single-property-change (point) 'duration))
             (goto-char pos)
             (when (and (not (equal pos (point-at-eol)))
                        (setq duration (org-get-at-bol 'duration)))
               (let ((line-height (if (< duration 30) 1.0 (+ 0.5 (/ duration 60))))
                     (ov (make-overlay (point-at-bol) (1+ (point-at-eol)))))
                 (overlay-put ov 'face `(:background ,(car colors)
                                                     :foreground
                                                     ,(if background-dark-p "black" "white")))
                 (setq colors (cdr colors))
                 (overlay-put ov 'line-height line-height)
                 (overlay-put ov 'line-spacing (1- line-height))))))))

(add-hook 'org-agenda-finalize-hook #'ljg/org-agenda-time-grid-spacing)

(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c a a") 'org-agenda)
(setq org-default-notes-file
      '("~/self/org/orgmode.org"))
(use-package org-bullets
  :hook (org-mode . org-bullets-mode)
  :config
  ;;(setq org-bullets-bullet-list '("■" "◆" "▲" "▶")))
  (setq org-bullets-bullet-list '("◉" "❖" "✮" "✱" "✸" "■" "◆" "▲" "▶")))

(setq org-tag-alist '(
                     (:startgroup . nil)
                        ("工作" . ?w)
                        ("公众号" . ?g)
                     (:endgroup . nil)
                     ("疫苗" . ?y)
                     ("磐石" . ?p)
                     ("技术" . ?j)
                     ("生日" . ?s)
                     ("读书" . ?d)
                     ("提升" . ?t)
                    ))

;; 记录时间
(setq org-log-done 'time)
;; 记录提示信息
(setq org-log-done 'note)

(setq org-capture-templates nil)

(add-to-list 'org-capture-templates
             '("r" "Book Reading Task" entry
               (file+olp "~/self/org/task.org" "Reading" "Book")
               "* TODO %^{书名}\n%u\n%a\n" :clock-in t :clock-resume t))

(add-to-list 'org-capture-templates
             '("w" "Work Task" entry
               (file+headline "~/slef/org/task.org" "Work")
               "* TODO %^{任务名}\n%u\n%a\n" :clock-in t :clock-resume t))

(add-to-list 'org-capture-templates
             '("w" "Web Collections" entry
               (file+headline "~/self/org/inbox.org" "Web")
               "* %U %:annotation\n\n%:initial\n\n%?"))

(add-to-list 'org-capture-templates '("t" "Tasks"))
(add-to-list 'org-capture-templates
             '("tr" "Book Reading Task" entry
               (file+olp "~/self/org/task.org" "Reading" "Book")
               "* TODO %^{书名}\n%u\n%a\n" :clock-in t :clock-resume t))
(add-to-list 'org-capture-templates
             '("tw" "Work Task" entry
               (file+headline "~/self/org/task.org" "Work")
               "* TODO %^{任务名}\n%u\n%a\n" :clock-in t :clock-resume t))

(add-to-list 'org-capture-templates
             '("j" "Journal" entry (file "~/self/org/journal.org")
               "* %U - %^{heading}\n  %?"))

(add-to-list 'org-capture-templates
             '("b" "Billing" plain
               (file+function "~/self/org/billing.org" find-month-tree)
               "\t\t\t%u * %^{消费描述} \n\t消费: %^{消费类型}\t\t\t%^{ } 元\n\t收入: %^{收入类型}\t\t\t%^{ } 元\n\t账户: %^{账户类型}\n\n" :clock-in t))


(defun get-year-and-month ()
  (list (format-time-string "%Y年") (format-time-string "%m月")))


(defun find-month-tree ()
  (let* ((path (get-year-and-month))
         (level 1)
         end)
    (unless (derived-mode-p 'org-mode)
      (error "Target buffer \"%s\" should be in Org mode" (current-buffer)))
    (goto-char (point-min))             ;移动到 buffer 的开始位置
    ;; 先定位表示年份的 headline，再定位表示月份的 headline
    (dolist (heading path)
      (let ((re (format org-complex-heading-regexp-format
                        (regexp-quote heading)))
            (cnt 0))
        (if (re-search-forward re end t)
          (goto-char (point-at-bol))  ;如果找到了 headline 就移动到对应的位置
          (progn                      ;否则就新建一个 headline
            (or (bolp) (insert "\n"))
            (if (/= (point) (point-min)) (org-end-of-subtree t t))
            (insert (make-string level ?*) " " heading "\n"))))
      (setq level (1+ level))
      (setq end (save-excursion (org-end-of-subtree t t))))
    (org-end-of-subtree)))


(add-to-list 'org-capture-templates
             '("c" "Contacts" entry (file "~/self/org/contacts.org")
               "* %^{姓名} %^{手机号}p %^{邮箱}p %^{住址}p\n\n  %?" :empty-lines 1))

(org-babel-do-load-languages
   'org-babel-load-languages
    '((python . t)
      (ledger . t)
      (C . t)))
