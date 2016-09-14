;;; init.el -- My emacs Configuration
;;; Commentary:
;;; This is a work branch.  Here is everything related to ROS and C++

;;; Code:

;;; %%%%%%%%%%%%%%%%%%%%%%%%%% INIT %%%%%%%%%%%%%%%%%%%%%%%%%%%
'(indent-tabs-mode nil)
'(inhibit-startup-screen t)
'(whitespace-line-column 95)


(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list 'package-archives '("elpa" . "http://tromey.com/elpa/"))
  (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
  (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
  (package-initialize))

;;; %%%%%%%%%%%%%%%%%%%%%%%% UTILITIES %%%%%%%%%%%%%%%%%%%%%%%%

;; Allow spanish keyboard
(require 'iso-transl)

;; create the autosave dir if necessary, since emacs won't.
(make-directory "~/.emacs-autosaves/" t)

;; Replace highlighted text in type
(delete-selection-mode 1)

;; Make zshrc be recognized as .zshrc
(add-to-list 'auto-mode-alist '("zshrc" . sh-mode))

;; Enable whitespace mode
(require 'whitespace)
(global-set-key (kbd "C-c w") 'global-whitespace-mode)
;(global-whitespace-mode)

;; - Style
; Theme
(load-theme 'badwolf t)
; Remove GUI toolbar
(tool-bar-mode -1)
; Remove scroll bars
(scroll-bar-mode -1)

;; - Bindkeys
; Show up emacs menus
(global-set-key (kbd "C-<f1O>") 'menu-bar-open)
; Quick buffer switching
(global-set-key (kbd "C-<tab>") 'mode-line-other-buffer)
; Open zsh terminal
(defun open-term ()
  (interactive)
  (ansi-term "/usr/bin/zsh"))
(global-set-key (kbd "C-<f3>") 'open-term)

;; - Funcionality
; windmode : change quickly between buffers
(global-set-key (kbd "C-S-<left>")  'windmove-left)
(global-set-key (kbd "C-S-<right>") 'windmove-right)
(global-set-key (kbd "C-S-<up>")    'windmove-up)
(global-set-key (kbd "C-S-<down>")  'windmove-down)

; move lines easily
(global-set-key (kbd "M-S-<up>") 'move-text-up)
(global-set-key (kbd "M-S-<down>") 'move-text-down)

; show-parent-mode: Highlight brackets
(setq show-paren-delay 0)
(show-paren-mode 1)

; Switch between camel cased & underscored
(require 'string-inflection)
(global-unset-key (kbd "C-q"))
(global-set-key (kbd "C-q C-u") 'string-inflection-all-cycle)


;;; %%%%%%%%%%%%%%%%%%%%%% ADDONS CONFIG %%%%%%%%%%%%%%%%%%%%%%

;; Complete closing braces
(require 'autopair)
(autopair-global-mode)

;; NeoTree: view files inside emacs
(require 'neotree)
(global-set-key [f2] 'neotree-toggle)


;; AutoComplete for C/C++
(require 'company)
(require 'irony)
(add-to-list 'company-backends 'company-irony)
(add-hook 'c++-mode-hook 'irony-mode)
(add-hook 'c-mode-hook 'irony-mode)
(add-hook 'objc-mode-hook 'irony-mode)
(add-hook 'c++-mode-hook 'company-mode)
(add-hook 'c-mode-hook 'company-mode)
(add-hook 'objc-mode-hook 'company-mode)

;; Syntax checking
(global-flycheck-mode)
(require 'flycheck)
(add-hook 'flycheck-mode-hook #'flycheck-irony-setup)
(add-hook 'c++-mode-hook (lambda () (setq flycheck-gcc-language-standard "c++11")))

;; replace the `completion-at-point' and `complete-symbol' bindings in
;; irony-mode's buffers by irony-mode's function
(defun my-irony-mode-hook ()
  (define-key irony-mode-map [remap completion-at-point]
    'irony-completion-at-point-async)
  (define-key irony-mode-map [remap complete-symbol]
    'irony-completion-at-point-async))
(add-hook 'irony-mode-hook 'my-irony-mode-hook)
(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)


;; ROS stuff
(defun ROS-c-mode-hook()
  (setq c-basic-offset 2)
  (setq indent-tabs-mode nil)
  (c-set-offset 'substatement-open 0)
  (c-set-offset 'innamespace 0)
  (c-set-offset 'case-label '+))
(add-hook 'c-mode-common-hook 'ROS-c-mode-hook)

;; iedit config
(define-key global-map (kbd "C-c ,") 'iedit-mode)


;;; %%%%%%%%%%%%%%%%%%%%%% FILES %%%%%%%%%%%%%%%%%%%%%
;; associate .pl as Ciao
(add-to-list 'auto-mode-alist '("\\.pl$" . ciao-mode))

;; Make .launch files be recognized as xml
(add-to-list 'auto-mode-alist '("\\.launch$" . nxml-mode))

;; In order to get namespace indentation correct, .h files must be opened in C++ mode
(add-to-list 'auto-mode-alist '("\\.h$" . c++-mode))


;;; %%%%%%%%%%%%%%%%%%%%%% MODES %%%%%%%%%%%%%%%%%%%%%
;; ciao-mode: ciao - a prolog interpreter
(add-to-list 'load-path "~/.emacs.d/ciao/")
(load "ciao") ;; best not to include the ending “.el” or “.elc”


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


(provide 'init)
;;; init.el ends here
