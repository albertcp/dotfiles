;;; %%%%%%%%%%%%%%%%%%%%%% INIT %%%%%%%%%%%%%%%%%%%%%%%%
(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list 'package-archives '("elpa" . "http://tromey.com/elpa/"))
  (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
  (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
  (package-initialize))
;;; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

;;; %%%%%% TEMES %%%%%
(load-theme 'badwolf t)
;;; %%%%%%%%%%%%%%%%%%

;;; %%%%%%%%%%%%%%%%%%%%%% ADDONS CONFIG %%%%%%%%%%%%%%%%%%%%%%

;; windmode : change quickly between buffers
(global-set-key (kbd "C-c <left>")  'windmove-left)
(global-set-key (kbd "C-c <right>") 'windmove-right)
(global-set-key (kbd "C-c <up>")    'windmove-up)
(global-set-key (kbd "C-c <down>")  'windmove-down)

;; speedbar : view files inside emacs
(require 'sr-speedbar)
(global-set-key (kbd "C-c a") 'sr-speedbar-toggle)
; sr-speedbar no refresh
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes (quote ("c4a784404a2a732ef86ee969ab94ec8b8033aee674cd20240b8addeba93e1612" default)))
 '(inhibit-startup-screen t)
 '(sr-speedbar-auto-refresh nil))

;; Saves. Not in .emacs.d!!
(make-directory "~/.saves/" t)
(setq backup-directory-alist `(("." . "~/.saves/")))

;; mouse integration
(require 'mouse)
(xterm-mouse-mode t)
(defun track-mouse (e))
(setq mouse-sel-mode t)

;; enable wheel
(defun up-slightly () (interactive) (scroll-up 5))
(defun down-slightly () (interactive) (scroll-down 5))
(global-set-key (kbd "<mouse-4>") 'down-slightly)
(global-set-key (kbd "<mouse-5>") 'up-slightly)

;; enable wheel-click pasting
(global-set-key (kbd "<mouse-2>") 'x-clipboard-yank)

;; move lines
(defun move-text-internal (arg)
  (cond
   ((and mark-active transient-mark-mode)
    (if (> (point) (mark))
        (exchange-point-and-mark))
    (let ((column (current-column))
          (text (delete-and-extract-region (point) (mark))))
      (forward-line arg)
      (move-to-column column t)
      (set-mark (point))
      (insert text)
      (exchange-point-and-mark)
      (setq deactivate-mark nil)))
   (t
    (let ((column (current-column)))
      (beginning-of-line)
      (when (or (> arg 0) (not (bobp)))
        (forward-line)
        (when (or (< arg 0) (not (eobp)))
          (transpose-lines arg)
          (when (and (eval-when-compile
                       '(and (>= emacs-major-version 24)
                             (>= emacs-minor-version 3)))
                     (< arg 0))
            (forward-line -1)))
        (forward-line -1))
      (move-to-column column t)))))

(defun move-text-down (arg)
  "Move region (transient-mark-mode active) or current line
  arg lines down."
  (interactive "*p")
  (move-text-internal arg))

(defun move-text-up (arg)
  "Move region (transient-mark-mode active) or current line
  arg lines up."
  (interactive "*p")
  (move-text-internal (- arg)))

;; move lines easily
(global-set-key (kbd "M-s <up>") 'move-text-up)
(global-set-key (kbd "M-s <down>") 'move-text-down)

;; helm-gtags
(setq
 helm-gtags-ignore-case t
 helm-gtags-auto-update t
 helm-gtags-use-input-at-cursor t
 helm-gtags-pulse-at-cursor t
 helm-gtags-prefix-key "\C-cg"
 helm-gtags-suggested-key-mapping t
 )
(require 'helm-gtags)
; Enable helm-gtags-mode
(add-hook 'dired-mode-hook 'helm-gtags-mode)
(add-hook 'eshell-mode-hook 'helm-gtags-mode)
;(add-hook 'c-mode-hook 'helm-gtags-mode)
;(add-hook 'c++-mode-hook 'helm-gtags-mode)
(add-hook 'asm-mode-hook 'helm-gtags-mode)

(define-key helm-gtags-mode-map (kbd "C-c g a") 'helm-gtags-tags-in-this-function)
(define-key helm-gtags-mode-map (kbd "C-j") 'helm-gtags-select)
(define-key helm-gtags-mode-map (kbd "M-.") 'helm-gtags-dwim)
(define-key helm-gtags-mode-map (kbd "M-,") 'helm-gtags-pop-stack)
(define-key helm-gtags-mode-map (kbd "C-c <") 'helm-gtags-previous-history)
(define-key helm-gtags-mode-map (kbd "C-c >") 'helm-gtags-next-history)

;; company-mode
(require 'company)
(add-hook 'after-init-hook 'global-company-mode)
; company-mode for Clang
;(setq company-backends (delete 'company-semantic company-backends))
;(define-key c-mode  [(tab)] 'company-complete)
;(define-key c++-mode-map  [(tab)] 'company-complete)

;; irony autocompletion
(add-hook 'c++-mode-hook 'irony-mode)
(add-hook 'c-mode-hook 'irony-mode)
(add-hook 'objc-mode-hook 'irony-mode)
; replace the completion-at-point' and complete-symbol' bindings in
; irony-mode's buffers by irony-mode's function
(defun my-irony-mode-hook ()
  (define-key irony-mode-map [remap completion-at-point]
    'irony-completion-at-point-async)
  (define-key irony-mode-map [remap complete-symbol]
    'irony-completion-at-point-async))
(add-hook 'irony-mode-hook 'my-irony-mode-hook)
(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)

;; flycheck
(global-flycheck-mode)

;; Remove toolbar
(tool-bar-mode -1)

;;; %%%%%%%%%%%%%%%%%%%%%% FILES %%%%%%%%%%%%%%%%%%%%%
;; associate .pl as Ciao
(add-to-list 'auto-mode-alist '("\\.pl$" . ciao-mode))


;;; %%%%%%%%%%%%%%%%%%%%%% MODES %%%%%%%%%%%%%%%%%%%%%
;; ciao-mode
(add-to-list 'load-path "~/.emacs.d/ciao/")
(load "ciao") ;; best not to include the ending “.el” or “.elc”
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
