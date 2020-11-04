;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;          Melpa
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'package) ;; You might already have this line
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize) ;; You might already have this line

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;          Miscellaneous convenience
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; stop with the backups
(setq make-backup-files nil)
(setq auto-save-default nil)

;; some dired shit
(require 'neotree)
(global-set-key [f8] 'neotree-toggle)


;; add the theme of the moment
(setq nord-comment-brightness 20)
(setq nord-region-highlight "snowstorm")
(load-theme 'nord t)


;; I need more rainbows
(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
(add-hook 'ess-mode-hook #'rainbow-delimiters-mode)
(add-hook 'R-mode-hook #'rainbow-delimiters-mode)
(add-hook 'org-mode-hook #'rainbow-delimiters-mode)

;; for line-wrapping convenience
(global-visual-line-mode t)

;; comment-break macro
(fset 'comment-break
      [?# ?# ?\- ?\C-u ?5 ?7 ?# return
          ?# ?# return
          ?# ?# ?\- ?\C-u ?5 ?7 ?# ])


;; temp macro to clean shit
;; (fset 'clean-trap-data
;;    (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([67108896 134217790 134217848 114 101 112 108 97 99 101 return 115 116 114 105 110 103 return 17 33554442 return 32 return 134217788 134217848 114 101 112 108 97 99 101 return 115 116 114 105 110 103 return 32 74 117 return 17 10 74 117 return 134217848 114 101 112 108 97 99 101 return 115 116 114 105 110 103 return 32 77 97 return 17 10 77 97 return] 0 "%d")) arg)))

;; I am sick as fuck of hitting C-{ instead of M-P and closing all my windows
;; 'nil isn't working, so I'll set it to something innocuous. This doesn't
;; work either. FUUUUUUUUUUUCK. FUCKing so annoying.
(global-set-key (kbd "C-{") 'next-line)

;; comment-break macro
(fset 'section-break
      [?# ?# ?\- ?\C-u ?7 ?7 ?# return
          ?# ?# return
          ?# ?# ?\- ?\C-u ?7 ?7 ?# ])

;; ;; move to the next R terminal
;; (fset 'nr
;;       [?\M-x ?e ?s ?s tab ?s ?w ?i tab ?p ?r ?o tab return])

;; ;; make a todolist item
;; (fset 'tli
;;    [?* ?* ?\s ?T ?O ?D ?O ?\s])


;; iterator macro
(fset 'iterator
      [?\C-x ?\C-k tab return])

;; reset iterator macro
(fset 'reset-iterator
   [?\C-x ?\C-k ?\C-c ?0 return])

;; highlight the current line
(setq-default hl-mode t)

;; go straight to an empty buffer
(setq  inhibit-startup-message t)

(setq-default indent-tabs-mode nil)
(define-key text-mode-map (kbd "<tab>") 'tab-to-tab-stop)
(setq-default tab-stop-list (number-sequence 4 120 4))
(setq-default tab-width 4)

;;set CUA mode
(cua-selection-mode 1)

;;automatic linum mode
(global-linum-mode 1)

;;Programming conveniences:
(show-paren-mode t); light-up matching parens
(global-font-lock-mode t) ; turn on syntax highlighting
(setq text-mode-hook (quote (turn-on-auto-fill text-mode-hook-identify)))

;; turn on automatic bracket insertion by pairs. New in emacs 24
(electric-pair-mode 1)

;; clean a line in latex
(fset 'clean-line
      (lambda (&optional arg) "Keyboard macro."
        (interactive "p")
        (kmacro-exec-ring-item (quote ([14 backspace 5 32] 0 "%d")) arg)))

;; make electric-pair-mode work on more brackets
(setq electric-pair-pairs '(
                            (?\" . ?\")
                            (?\{ . ?\}) ) )

;; remove interface bars to look BA
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1) 


;; delete selection as in standard text editing
(delete-selection-mode 1)

;; set emacs start buffer to be a shell
;; (switch-to-buffer (get-buffer-create (shell)))

;; keyboard macro to insert skeleton for R documentation
(fset 'R-fundocs
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (
   kmacro-exec-ring-item (quote (
   [35 39 32 70 117 110 99 116 105 111 110 32 78 97 109 101 return 
   return 70 117 110 99 116 105 111 110 32 68 101 115 99 114 105 
   112 116 105 111 110 return 64 112 97 114 97 109 return 64 107 
   101 121 119 111 114 100 115 return 64 101 120 112 111 114 116 
   return 64 101 120 97 109 112 108 101 115] 0 "%d")) arg)))

;; display time
(display-time-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;          org mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq org-log-done t)
(require 'org)
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;          direx
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'direx)
(global-set-key (kbd "C-x C-j") 'direx:jump-to-directory)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;          lisp functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun my-fixup-whitespace ()
  (interactive "*")
  (if (or (eolp)
 (save-excursion
   (beginning-of-line)
   (looking-at "^\\s *$")))
      (delete-blank-lines)
    (fixup-whitespace)))

;; clear shell
(defun clear-shell ()
   (interactive)
   (let ((old-max comint-buffer-maximum-size))
     (setq comint-buffer-maximum-size 0)
     (comint-truncate-buffer)
     (setq comint-buffer-maximum-size old-max)))

;; Bring to fullscreen (and new desktop)
(defun toggle-fullscreen ()
  "Toggle full screen"
  (interactive)
  (set-frame-parameter
     nil 'fullscreen
     (when (not (frame-parameter nil 'fullscreen)) 'fullboth)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;          custom ido settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(require 'ido)
(ido-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;          ESS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq ess-indent-level 2)
(add-to-list 'load-path "/Users/yoni/Documents/Emacs/elisp/ess-16.10/lisp/")
(load "ess-site")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;          R
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq inferior-R-program-name "/usr/local/bin/R")

;; scroll down after input to an R session
(setq comint-prompt-read-only t)
(setq comint-scroll-to-bottom-on-input t)
(setq comint-scroll-to-bottom-on-output t)
(setq comint-move-point-for-output t)

;; keyboard macro to insert skeleton for R documentation
(fset 'R-fundocs
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (
   kmacro-exec-ring-item (quote (
   [35 39 32 70 117 110 99 116 105 111 110 32 78 97 109 101 return
   return 70 117 110 99 116 105 111 110 32 68 101 115 99 114 105
   112 116 105 111 110 return 64 112 97 114 97 109 return 64 107
   101 121 119 111 114 100 115 return 64 101 120 112 111 114 116
   return 64 101 120 97 109 112 108 101 115] 0 "%d")) arg)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;          Emacs IPython Notebook
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'ein)
;; see https://stackoverflow.com/questions/31877966/use-ein-emacs-ipython-notebook-on-remote-server
;; for how to connect to remote. looks like you'll
;; need to write a macro that tunnels from local
;; port to remote port then connect to the local
;; Port using EIN.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;          Multiple Cursors
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;          ace-window
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-set-key (kbd "M-P") 'ace-window)
(setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;          line length help
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'column-marker)
(add-hook 'R-mode-hook (lambda () (interactive) (column-marker-3 80)))
(add-hook 'Python-mode-hook (lambda () (interactive) (column-marker-3 80)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;          tramp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq tramp-default-method "ssh")
(require 'tramp)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("7527f3308a83721f9b6d50a36698baaedc79ded9f6d5bd4e9a28a22ab13b3cb1" default)))
 '(package-selected-packages
   (quote
    (pyenv-mode haskell-mode neotree direx ein rainbow-delimiters polymode nord-theme multiple-cursors markdown-mode column-marker ace-window))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
