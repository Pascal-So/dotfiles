(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(compile-command "g++ -Wall -Wextra -D_GLIBCXX_DEBUG -std=c++14 ")
 '(custom-enabled-themes (quote (wombat)))
 '(global-linum-mode t)
 '(inhibit-startup-screen t)
 '(tool-bar-mode nil)
 '(visible-bell t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(global-set-key [S-left] 'windmove-left)          ; move to left window
(global-set-key [S-right] 'windmove-right)        ; move to right window
(global-set-key [S-up] 'windmove-up)              ; move to upper window
(global-set-key [S-down] 'windmove-down)          ; move to lower window

(setq c-basic-offset 4)
(setq-default indent-tabs-mode nil)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

(require 'ps-ccrypt "ps-ccrypt.el")


;;;;;;;;;;;;; ORG MODE ;;;;;;;;;;;;;;;;;;

(defun setup-org-keybindings ()
  ;; fix window move
  (define-key org-mode-map [S-left] nil)
  (define-key org-mode-map [S-right] nil)
  (define-key org-mode-map [S-up] nil)
  (define-key org-mode-map [S-down] nil)

  ;; tree navigation
  (define-key org-mode-map [C-c C-b] nil)
  (define-key org-mode-map [C-c C-f] nil)
  (define-key org-mode-map [C-c C-u] nil)
  (define-key org-mode-map [C-c C-d] nil)
  (define-key org-mode-map [C-S-up] 'outline-backward-same-level)
  (define-key org-mode-map [C-S-down] 'outline-forward-same-level)
  (define-key org-mode-map [C-S-left] 'outline-up-heading)
  (define-key org-mode-map [C-S-right] nil))

(eval-after-load "org" '(setup-org-keybindings))
