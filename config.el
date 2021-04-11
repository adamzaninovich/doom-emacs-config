;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;;; NOTE: `config.el' is now generated from `config.org'. Please edit that file
;;;       in Emacs and `config.el' will be generated automatically!

(load! "secret.el")

(defun me/random-choice (items)
  "Returns a random item from a given list"
  (let* ((size (length items))
         (index (random size)))
    (nth index items)))

(setq user-full-name me/full-name
      user-mail-address me/mail-address)

(let* ((banner-directory (substitute-in-file-name "$HOME/.config/doom/banners"))
       (command (concat "\\ls -A1d " banner-directory "/*.png"))
       (output (shell-command-to-string command))
       (banners (split-string output "\n" t))
       (banner (me/random-choice banners)))
  (setq +doom-dashboard-banner-file banner))

(setq-default delete-by-moving-to-trash t
              window-combination-resize t
              x-stretch-cursor t)

(setq undo-limit (* 80 1024 1024)
      evil-want-fine-undo t
      auto-save-default t
      truncate-string-ellipsis "…")

(setq evil-move-cursor-back nil)

(global-auto-revert-mode t)

(add-to-list 'initial-frame-alist '(fullscreen . maximized))

(after! doom-ui
  (scroll-bar-mode -1)
  (tool-bar-mode -1)
  (tooltip-mode -1)
  (menu-bar-mode -1))

(setq display-line-numbers-type 'relative)

(setq me/fixed-width-font-family "FiraCode Nerd Font"
      me/variable-pitch-font-family "Overpass")

(setq
 doom-font (font-spec :family me/fixed-width-font-family :size 14 :style "Retina")
 doom-big-font (font-spec :family me/fixed-width-font-family :size 20 :style "Retina")
 doom-variable-pitch-font (font-spec :family me/variable-pitch-font-family :size 16 :style "Regular"))

(setq doom-theme 'doom-palenight)

(defun me/org-font-setup ()
  (dolist (face '((org-level-1 . 1.5)
                  (org-level-2 . 1.4)
                  (org-level-3 . 1.3)
                  (org-level-4 . 1.2)
                  (org-level-5 . 1.0)
                  (org-level-6 . 1.0)
                  (org-level-7 . 1.0)
                  (org-level-8 . 1.0)))
    (set-face-attribute (car face) nil :font me/variable-pitch-font-family :weight 'Semibold :height (cdr face)))

  (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-table nil                 :inherit 'fixed-pitch)
  (set-face-attribute 'org-formula nil               :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil                  :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-table nil                 :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil              :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil       :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil             :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil              :inherit 'fixed-pitch)
  (set-face-attribute 'line-number nil               :inherit 'fixed-pitch)
  (set-face-attribute 'line-number-current-line nil  :inherit 'fixed-pitch))

(require 'org-tempo)

(after! org
  (setq
   org-ellipsis " ▾"
   org-directory "~/projects/org/"
   org-agenda-files '("~/projects/org/agenda.org" "~/projects/org/todo.org")
   org-log-done 'time)

  (add-to-list 'org-structure-template-alist '("el"  . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("sh"  . "src sh"))
  (add-to-list 'org-structure-template-alist '("iex" . "src elixir"))
  (variable-pitch-mode 1)
  (me/org-font-setup))

(add-hook 'org-mode-hook (lambda ()
                           (visual-fill-column-mode 1)
                           (setq
                            visual-fill-column-center-text t
                            visual-fill-column-width 120)

                           (org-indent-mode 1)
                           (visual-line-mode 1)
                           (display-line-numbers-mode 0)))

(defun me/org-babel-tangle-config ()
  (when (member (buffer-file-name)
                (list (expand-file-name "~/.config/doom/config.org")
                      (expand-file-name "~/.config/doom/install.org")))
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))

(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'me/org-babel-tangle-config)))

(after! doom-modeline
  (setq
   doom-modeline-major-mode-icon t
   doom-modeline-height 35
   doom-modeline-persp-name t))

(setq display-time-day-and-date nil
      display-time-default-load-average nil)

(display-time-mode 1)

(if (equal "Battery status not available"
           (battery))
    (display-battery-mode 0)
    (display-battery-mode 1))

(defun me/doom-modeline-conditional-buffer-encoding ()
  "We expect the encoding to be LF UTF-8, so only show the modeline when this is not the case"
  (setq-local doom-modeline-buffer-encoding
              (unless (and (memq (plist-get (coding-system-plist buffer-file-coding-system) :category)
                                 '(coding-category-undecided coding-category-utf-8))
                           (not (memq (coding-system-eol-type buffer-file-coding-system) '(1 2))))
                t)))

(add-hook 'after-change-major-mode-hook #'me/doom-modeline-conditional-buffer-encoding)

(after! evil-escape (evil-escape-mode -1))

(setq magit-revision-show-gravatars '("^Author:     " . "^Commit:     "))

(setq-default flycheck-disabled-checkers '(proselint))

(setq
 treemacs-width 30
 treemacs-follow-mode t
 treemacs-position 'left
 doom-themes-treemacs-theme "doom-colors")

(after! centaur-tabs
  (setq
   centaur-tabs-style "bar"
   centaur-tabs-set-bar 'none
   centaur-tabs-bar-height 30
   centaur-tabs-height 28)

  (centaur-tabs-change-fonts me/variable-pitch-font-family 150)

  (defun centaur-tabs-buffer-groups ()
    "`centaur-tabs-buffer-groups' control buffers' group rules.

    Group centaur-tabs with mode if buffer is derived from `eshell-mode'
    `emacs-lisp-mode' `dired-mode' `org-mode' `magit-mode'.
    All buffer name start with * will group to \"Emacs\".
    Other buffer group by `centaur-tabs-get-group-name' with project name."
    (list
     (cond
      ((or (string-equal "*" (substring (buffer-name) 0 1))
           (memq major-mode '(magit-process-mode
                              magit-status-mode
                              magit-diff-mode
                              magit-log-mode
                              magit-file-mode
                              magit-blob-mode
                              magit-blame-mode
                              )))
       "Emacs")
      ((derived-mode-p 'eshell-mode) "EShell")
      ((derived-mode-p 'dired-mode) "Dired")
      ;; ((derived-mode-p 'emacs-lisp-mode) "Elisp")
      ;; ((memq major-mode '(org-mode org-agenda-mode diary-mode)) "OrgMode")
      (t
       (centaur-tabs-get-group-name (current-buffer))))))
  )

(setq lsp-enable-file-watchers nil)

(after! projectile
  (setq projectile-ignored-projects '("~/" "/tmp/" "~/.emacs.d/" "/opt/homebrew/"))
  (setq projectile-project-search-path '("~/projects/" "~/campaigns/")))

(after! evil-snipe (evil-snipe-mode -1))

(use-package! pdf-tools
  :defer t
  :config
  (pdf-loader-install)
  (setq pdf-view-use-scaling t
        pdf-view-use-imagemagick nil))

(add-hook 'elixir-mode-hook
          (lambda () (add-hook 'before-save-hook 'elixir-format nil t)))
(add-hook 'elixir-format-hook (lambda ()
                                (if (projectile-project-p)
                                    (setq elixir-format-arguments
                                          (list "--dot-formatter"
                                                (concat (locate-dominating-file buffer-file-name ".formatter.exs") ".formatter.exs")))
                                  (setq elixir-format-arguments nil))))

(map! :desc "Open Dired here" :n "-" #'dired-jump)

(map! :desc "Next Tab" :g "s-}" #'centaur-tabs-forward)
(map! :desc "Previous Tab" :g "s-{" #'centaur-tabs-backward)

(map! :desc "Decrease current window width" :g "s-[" #'evil-window-decrease-width)
(map! :desc "Increase current window width" :g "s-]" #'evil-window-increase-width)
