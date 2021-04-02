;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

(load! "functions.el")

;; Shhhhhhhhhhhhh
(load! "secret.el")

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Adam Zaninovich"
      user-mail-address my-gpg-email-address)

;; Sets a random banner on startup
(let* ((files '("~/.config/doom/banners/doom-cyan.png"
                "~/.config/doom/banners/doom-grey.png"
                "~/.config/doom/banners/doom-orange.png"
                "~/.config/doom/banners/doom-purple.png"
                "~/.config/doom/banners/doom-guy.png"
                "~/.config/doom/banners/doom-doc.png"
                "~/.config/doom/banners/doom-mess.png"
                "~/.config/doom/banners/vim.png"))
       (file (random-choice files)))
  (setq +doom-dashboard-banner-file file))
;; (setq +doom-dashboard-banner-padding '(2 . 3))

;; start maximized
(add-to-list 'initial-frame-alist '(fullscreen . maximized))

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
(setq
 doom-font (font-spec :family "FiraCode Nerd Font" :size 14 :style "Retina")
 doom-big-font (font-spec :family "FiraCode Nerd Font" :size 20 :style "Retina")
 doom-variable-pitch-font (font-spec :family "SF Pro" :size 14 :style "Medium"))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function.
;; Some good themes:
;; - doom-one (default)
;; - doom-nord
;; - doom-palenight
(setq doom-theme 'doom-palenight)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(after! org
  (setq
   org-directory "~/projects/org/"
   org-agenda-files '("~/projects/org/agenda.org" "~/projects/org/todo.org")
   org-log-done 'time))

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)

;; Configuring some plugin settings
(setq
 treemacs-width 35
 treemacs-follow-mode t
 treemacs-position 'left)

(after! centaur-tabs
  (setq
   centaur-tabs-style "wave"
   centaur-tabs-set-bar 'none
   centaur-tabs-height 28)
  (centaur-tabs-change-fonts "SF Pro" 140))

(after! projectile
  (setq projectile-project-search-path '("~/projects/"))
  ;; use SPC / to search in project
  ;; (map! :leader
  ;;       :desc "Search in project" "p S" #'projectile-ripgrep)
  )

;; Make S and s work again
(after! evil-snipe
  (evil-snipe-mode -1))

;; Create a buffer-local hook to run elixir-format on save, only when we enable elixir-mode.
(add-hook 'elixir-mode-hook
          (lambda () (add-hook 'before-save-hook 'elixir-format nil t)))
(add-hook 'elixir-format-hook (lambda ()
                                (if (projectile-project-p)
                                    (setq elixir-format-arguments
                                          (list "--dot-formatter"
                                                (concat (locate-dominating-file buffer-file-name ".formatter.exs") ".formatter.exs")))
                                  (setq elixir-format-arguments nil))))

;; Key Bindings

(map! :desc "Open Dired here" :n "-" #'dired-jump)

(map! :desc "Next Tab" :g "s-}" #'centaur-tabs-forward)
(map! :desc "Previous Tab" :g "s-{" #'centaur-tabs-backward)

(map! :desc "Decrease current window width" :g "s-[" #'evil-window-decrease-width)
(map! :desc "Increase current window width" :g "s-]" #'evil-window-increase-width)

;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.
