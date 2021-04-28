(require 'ivy)
(require 'diminish)

(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)
(global-set-key (kbd "C-c C-r") 'ivy-resume)
(diminish 'ivy-mode)
(global-set-key (kbd "C-s") 'swiper)
(global-set-key (kbd "C-c /") 'counsel-ag)

(provide 'prelude-ivy)
