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

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(global-linum-mode t)

;; (load-theme 'molokai t)
(load-theme 'wombat t)

(add-to-list 'load-path "~/.emacs.d/extra")

;; Bootstrap 'use-package'
(eval-after-load 'gnutls
  '(add-to-list 'gnutls-trustfiles "/etc/ssl/cert.pem"))
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))
(require 'bind-key)
(setq use-package-always-ensure t)

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
(global-set-key (kbd "<escape> <escape>")  'keyboard-escape-quit)

;; TAB to indent in normal-state
(define-key evil-normal-state-map (kbd "C-x t") 'indent-for-tab-command)

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
;;(unless window-system
;;  (when (getenv "DISPLAY")
;;    ;; Callback for when user cuts
;;    (defun xsel-cut-function (text &optional push)
;;      ;; Insert text to temp-buffer, and "send" content to xsel stdin
;;      (with-temp-buffer
;;        (insert text)
;;        ;; Use primary the primary selection
;;        ;; mouse-select/middle-button-click
;;        (call-process-region (point-min) (point-max) "xsel" nil 0 nil "--primary" "--input")))
;;    ;; Call back for when user pastes
;;    (defun xsel-paste-function()
;;      ;; Find out what is current selection by xsel. If it is different
;;      ;; from the top of the kill-ring (car kill-ring), then return
;;      ;; it. Else, nil is returned, so whatever is in the top of the
;;      ;; kill-ring will be used.
;;      (let ((xsel-output (shell-command-to-string "xsel --primary --output")))
;;        (unless (string= (car kill-ring) xsel-output)
;;          xsel-output)))
;;    ;; Attach callbacks to hooks
;;    (setq interprogram-cut-function 'xsel-cut-function)
;;    (setq interprogram-paste-function 'xsel-paste-function)))


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
 '(org-agenda-files
   (quote
    ("~/self/org/1_note.org"
     "~/self/org/2_ledger.org"
     "~/self/org/3_task.org"
     "~/self/org/4_booklist.org"
     "~/self/org/6_trans_parents_ledger.org"
     "~/self/org/7_idcard_infos.org"
    )))
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

(setq-default org-display-custom-times t)
(setq org-time-stamp-custom-formats '("<%Y/%m/%d %a>" . "<%Y/%m/%d %a %H:%M>"))

;; 设置 todo keywords
(setq org-todo-keywords '((sequence "NEXT(n)" "TODO(t)" "STARTED(s)" "WAITING(w@/!)" "|" "DONE(d!)" "CANCELLED(c@)")))

;; 调试好久的颜色，效果超赞！todo keywords 增加背景色
(setf org-todo-keyword-faces '(("TODO" .    (:foreground "white" :background "#95A5A6" :weight bold))
                               ("STARTED" . (:foreground "white" :background "#2E8B57" :weight bold))
                               ("DONE" .    (:foreground "green" :background "#3497DB" :weight bold))))

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

(defun org-agenda-inactive ()
  (interactive)
  (let ((org-agenda-include-inactive-timestamps t))
    (org-agenda))
)

(global-set-key (kbd "C-c a a") 'org-agenda)
(global-set-key (kbd "C-c b") 'org-agenda-inactive)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c l") 'org-store-link)

(setq org-default-notes-file
      '("~/self/org/0_summary.org"))
(use-package org-bullets
  :hook (org-mode . org-bullets-mode)
  :config

(defun my:org-agenda-time-grid-spacing ()
  "Set different line spacing w.r.t. time duration."
  (save-excursion
    (let* ((background (alist-get 'background-mode (frame-parameters)))
           (background-dark-p (string= background "dark"))
           (colors (if background-dark-p
                     (list "#aa557f" "DarkGreen" "DarkSlateGray" "DarkSlateBlue")
                     (list "#F6B1C3" "#FFFF9D" "#BEEB9F" "#ADD5F7")))
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
                 (setj colors (cdr colors))
                 (overlay-put ov 'line-height line-height)
                 (overlay-put ov 'line-spacing (1- line-height))))))))

(add-hook 'org-agenda-finalize-hook #'my:org-agenda-time-grid-spacing)
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
             '("s" "记事" entry
               (file+headline "~/self/org/1_note.org" "2020-日常记事")
               "*** %^u %^{事情描述}\t\t\t:%^{关联人脉或者事件}:\n"))

(add-to-list 'org-capture-templates '("t" "新增纪念日"))
(add-to-list 'org-capture-templates
             '("ta" "纪念日（阳历）" entry
               (file "~/self/org/5_diary.org")
               "\n\n* %^{相关人物或事件}\n%%%^{直接回车}(diary-anniversary %^{月份} %^{日期} %^{年份}) %^{事件描述}，%d 周岁/周年（阳历）。\n"))

(add-to-list 'org-capture-templates
             '("tc" "纪念日（农历）" entry
               (file "~/self/org/5_diary.org")
               "\n\n* %^{相关人物或事件}\n%%%^{直接回车}(m-diary-chinese-anniversary %^{月份} %^{日期} %^{年份}) %^{事件描述}，%d 周岁/周年（农历）。\n"))

(add-to-list 'org-capture-templates
             '("d" "生活计划" entry
               (file+olp "~/self/org/3_task.org" "我的计划列表" "生活相关")
               "\n\n* TODO %u [#%^{优先级 A - D}] %^{计划描述}\t\t:%^{关联人脉或者事件}:\nDEADLINE: %^t SCHEDULED: %^t\n\n" :empty-lines 1))

(add-to-list 'org-capture-templates
             '("r" "读书计划" entry
               (file+olp "~/self/org/3_task.org" "我的计划列表" "读书计划")
                "\n\n* TODO %u #B %^{书名}\t\t%^{关联图书渠道}\nDEADLINE: %^t SCHEDULED: %^t\n\n" :empty-lines 1))

(add-to-list 'org-capture-templates
             '("x" "学习计划" entry
               (file+olp "~/self/org/3_task.org" "我的计划列表" "学习计划")
               "\n\n* TODO %u [#%^{优先级 A - D}] %^{学习计划描述}\t\t:%^{关联内容描述}:\nDEADLINE: %^t SCHEDULED: %^t\n\n" :empty-lines 1))

(add-to-list 'org-capture-templates
             '("w" "工作计划" entry
               (file+olp "~/self/org/3_task.org" "我的计划列表" "工作相关")
               "\n\n* TODO %u [#%^{优先级 A - D}] %^{计划描述}\t\t:%^{关联人脉或者事件}:\nDEADLINE: %^t SCHEDULED: %^t\n\n" :empty-lines 1))

(add-to-list 'org-capture-templates
             '("b" "借书记录" entry
              (file+olp "~/self/org/4_booklist.org" "读书买书借书列表" "借书列表")
              "\n\n* %^u %^{图书馆名称}，借书总计 %^{借书总数} 本 [/]\t\t\t:借书:\n最后还书日 —— DEADLINE: %^t\n- [ ] 《%^{书单列表}》\n\n" :empty-lines 1))

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

(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

(with-eval-after-load 'org
  (org-babel-do-load-languages
     'org-babel-load-languages
      '((python . t)
        (ledger . t)
        (C . t)))
  (setq org-confirm-babel-evalute nil))
(put 'scroll-left 'disabled nil)

(setq org-agenda-custom-commands
    '(("f" "查看TODO条目（按创建时间排序）" todo "TODO"
     ((org-agenda-sorting-strategy '(priority-down tag-down))))))

;; 优先级范围和默认任务的优先级
(setq org-highest-priority ?A)
(setq org-lowest-priority  ?D)
(setq org-default-priority ?C)

;; 优先级醒目外观
;;set colours for priorities
(setq org-priority-faces '((?A . (:foreground "#F0DFAF" :weight bold))
                           (?B . (:foreground "LightSteelBlue"))
                           (?D . (:foreground "OliveDrab"))))

;; 设置日历
(setq org-agenda-include-diary t
    diary-file (locate-user-emacs-file "~/self/org/5_diary.org")
    )

(setq my-holidays
    '(;;公历节日
     (holiday-fixed 2 14 "情人节")
     (holiday-fixed 9 10 "教师节")
     (holiday-float 5 0 2 "母亲节")   ;5月的第二个星期天
     (holiday-float 6 0 3 "父亲节")
     (holiday-float 11 4 4 "感恩节")   ;11 月的第 4 个星期四
     ;; 农历节日
     (holiday-chinese 1 1  "春节")
     (holiday-chinese 1 15 "元宵节")
     (holiday-solar-term   "清明" "清明节")
     (holiday-chinese 5 5  "端午节")
     (holiday-chinese 7 7  "七夕情人节")
     (holiday-chinese 8 15 "中秋节")
     ;;纪念日
     (holiday-chinese 1 1 "自定义农历生日 1 月 1 日")
     ))

(setq calendar-holidays my-holidays)  ;只显示我定制的节假日

;(defun m-diary-chinese-anniversary (lunar-month lunar-day &optional year mark)
;  (if year
;    (let* ((d-date (diary-make-date lunar-month lunar-day year))
;           (a-date (calendar-absolute-from-gregorian d-date))
;           (c-date (calendar-chinese-from-absolute a-date))
;           (cycle (car c-date))
;           (yy (cadr c-date))
;           (y (+ (* 100 cycle) yy)))
;      (diary-chinese-anniversary lunar-month lunar-day y mark))
;    (diary-chinese-anniversary lunar-month lunar-day year mark)))

(defun m-diary-chinese-anniversary (lunar-month lunar-day &optional year mark)
  (let* ((ddate (diary-make-date lunar-month lunar-day year))
      (dd (calendar-extract-day ddate))
      (mm (calendar-extract-month ddate))
      (yy (calendar-extract-year ddate))
      (a-date (calendar-absolute-from-gregorian date))
      (c-date (calendar-chinese-from-absolute a-date))
      (mm2 (nth 2 c-date))
      (dd2 (nth 3 c-date))
      (y (calendar-extract-year date))
      (diff (if year (- y year) 100)))
    (and (> diff 0) (= mm mm2) (= dd dd2)
        (cons mark (format entry diff (diary-ordinal-suffix diff))))))
