;;; init.el -- My emacs Configuration

;;; Commentary:

;;; Code:

;;; %%%%%%%%%%%%%%%%%%%%%%%%%% INIT %%%%%%%%%%%%%%%%%%%%%%%%%%%

(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list 'package-archives '("elpa" . "http://tromey.com/elpa/"))
  (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
  (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
  (package-initialize))

;;; %%%%%%%%%%%%%%%%%%%%%%%% UTILITIES %%%%%%%%%%%%%%%%%%%%%%%%

;; - Style
; Theme
(load-theme 'badwolf t)
; Remove GUI toolbar
(tool-bar-mode -1)

;; - Bindkeys
; show up emacs menus
(global-set-key (kbd "C-<f1O>") 'menu-bar-open)

;; - Funcionality
; windmode : change quickly between buffers
(global-set-key (kbd "C-c <left>")  'windmove-left)
(global-set-key (kbd "C-c <right>") 'windmove-right)
(global-set-key (kbd "C-c <up>")    'windmove-up)
(global-set-key (kbd "C-c <down>")  'windmove-down)

; move lines easily
(global-set-key (kbd "M-s <up>") 'move-text-up) ; move-text-up code at the bottom
(global-set-key (kbd "M-s <down>") 'move-text-down); move-text-down code at bottom

; show-parent-mode: Highlight brackets
(setq show-paren-delay 0)
(show-paren-mode 1)

; mouse integration
(require 'mouse)
(xterm-mouse-mode t)
(defun track-mouse (e))
(setq mouse-sel-mode t)

; enable wheel
(global-set-key (kbd "<mouse-4>") 'down-slightly) ; down-slightly function at the bottom
(global-set-key (kbd "<mouse-5>") 'up-slightly) ; up-slightly function at the bottom

; enable wheel-click pasting
(global-set-key (kbd "<mouse-2>") 'x-clipboard-yank)


;;; %%%%%%%%%%%%%%%%%%%%%% ADDONS CONFIG %%%%%%%%%%%%%%%%%%%%%%

;; speedbar : view files inside emacs
(require 'sr-speedbar)
(global-set-key (kbd "C-c a") 'sr-speedbar-toggle)
; sr-speedbar no refresh
(custom-set-variables
 '(custom-safe-themes (quote ("c4a784404a2a732ef86ee969ab94ec8b8033aee674cd20240b8addeba93e1612" default)))
 '(inhibit-startup-screen t)
 '(sr-speedbar-auto-refresh nil))

;; auto-complete :: TODO: change to company
(require 'auto-complete)
(require 'auto-complete-config)
(ac-config-default)

;; yasnippet
(require 'yasnippet)
(yas-global-mode 1)

;; auto-complete-c-headers
(defun my:ac-c-header-init()
  (require 'auto-complete-c-headers)
  (add-to-list 'ac-sources 'ac-source-c-headers)
  (add-to-list 'achead:include-directories '"/usr/lib/gcc/x86_64-unknown-linux-gnu/5.3.0/include"))
 ; (add-to-list 'achead:include-directories '"/usr/lib/gcc/x86_64-unknown-linux-gnu/*/include"))

(add-hook 'c++-mode-hook 'my:ac-c-header-init)
(add-hook 'c-mode-hook 'my:ac-c-header-init)

;; iedit config
(define-key global-map (kbd "C-c ,") 'iedit-mode)

;; company-mode
;(require 'company)
;(add-hook 'after-init-hook 'global-company-mode)
; company-mode for Clang
;(setq company-backends (delete 'company-semantic company-backends))
;(define-key c-mode  [(tab)] 'company-complete)
;(define-key c++-mode-map  [(tab)] 'company-complete)

;; irony autocompletion
;(add-hook 'c++-mode-hook 'irony-mode)
;(add-hook 'c-mode-hook 'irony-mode)
;(add-hook 'objc-mode-hook 'irony-mode)
; replace the completion-at-point' and complete-symbol' bindings in
; irony-mode's buffers by irony-mode's function
;(defun my-irony-mode-hook ()
; (define-key irony-mode-map [remap completion-at-point]
;    'irony-completion-at-point-async)
;  (define-key irony-mode-map [remap complete-symbol]
;    'irony-completion-at-point-async))
;(add-hook 'irony-mode-hook 'my-irony-mode-hook)
;(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)

;; flycheck
(global-flycheck-mode)


;;; %%%%%%%%%%%%%%%%%%%%%% FILES %%%%%%%%%%%%%%%%%%%%%
;; associate .pl as Ciao
(add-to-list 'auto-mode-alist '("\\.pl$" . ciao-mode))


;;; %%%%%%%%%%%%%%%%%%%%%% MODES %%%%%%%%%%%%%%%%%%%%%
;; ciao-mode: ciao - a prolog interpreter
(add-to-list 'load-path "~/.emacs.d/ciao/")
(load "ciao") ;; best not to include the ending “.el” or “.elc”

;; ghc-mod : extension for emacs mode
;(add-to-list 'load-path "~/.cabal/share/x86_64-linux-ghc-8.0.1/ghc-mod-5.6.0.0/elisp/")
(autoload 'ghc-init "ghc" nil t)
(autoload 'ghc-debug "ghc" nil t)
(add-hook 'haskell-mode-hook (lambda () (ghc-init)))


;;; %%%%%%%%%%%%%%%%%%%%%%  Code %%%%%%%%%%%%%%%%%%%%%
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
  "Move region (transient-mark-mode active) or current line ARG lines down."
  (interactive "*p")
  (move-text-internal arg))

(defun move-text-up (arg)
  "Move region (transient-mark-mode active) or current line ARG lines up."
  (interactive "*p")
  (move-text-internal (- arg)))

;; enable wheel
(defun up-slightly () (interactive) (scroll-up 5))
(defun down-slightly () (interactive) (scroll-down 5))


(provide 'init)
;;; init.el ends here
