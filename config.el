;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;;; NOTE: `config.el' is now generated from `config.org'. Please edit that file
;;;       in Emacs and `config.el' will be generated automatically!

(load! "secret.el")

(defun me/random-choice (items)
  "Returns a random item from a given list"
  (let* ((size (length items))
         (index (random size)))
    (nth index items)))

(defun me/read-lines (file)
  "Reads a file, filters out lines starting with #, and returns the lines as a list"
  (let* ((file-contents (with-temp-buffer
                          (insert-file-contents file)
                          (buffer-substring-no-properties
                           (point-min)
                           (point-max))))
         (lines (split-string file-contents "\n" t)))
    (seq-remove (lambda (line) (string-match-p "^#" line)) lines)))

(defun me/random-line-from-file (file)
  "Reads a file and returns a random line"
  (me/random-choice (me/read-lines file)))

(setq user-full-name me/full-name
      user-mail-address me/mail-address)

(let* ((banner-directory (substitute-in-file-name "$HOME/.config/doom/resources/banners"))
       (command (concat "\\ls -A1d " banner-directory "/*.png"))
       (output (shell-command-to-string command))
       (banners (split-string output "\n" t))
       (banner (me/random-choice banners)))
  (setq fancy-splash-image banner))

(setq +doom-quit-messages
      (me/read-lines
       (substitute-in-file-name
        "$HOME/.config/doom/resources/messages.txt")))

(doom-load-envvars-file "~/.emacs.d/.local/doom_only_env")

(setq-default delete-by-moving-to-trash t
              window-combination-resize t
              x-stretch-cursor t)

(setq undo-limit (* 80 1024 1024)
      evil-want-fine-undo t
      auto-save-default t
      truncate-string-ellipsis "…")

(setq evil-move-cursor-back nil)

(global-auto-revert-mode t)

(add-to-list 'initial-frame-alist '(width . (text-pixels . 1180)))
(add-to-list 'initial-frame-alist '(height . (text-pixels . 780)))
(add-to-list 'initial-frame-alist '(top . 50))
(add-to-list 'initial-frame-alist '(left . 45))

(after! doom-ui
  (scroll-bar-mode -1)
  (tool-bar-mode -1)
  (tooltip-mode -1))

(setq display-line-numbers-type 'relative)

(setq-default frame-title-format '(""))

(setq nerd-icons-font-names '("SymbolsNerdFontMono-Regular.ttf"))

(setq me/fixed-width-font '(:family "ComicCode Nerd Font" :style "Medium")
      me/variable-pitch-font '(:family "Overpass" :style "Regular")
      me/variable-pitch-serif-font '(:family "Bookerly" :style "Regular"))

(setq me/org-font-family (plist-get me/variable-pitch-font :family)
      me/ebook-font-family (plist-get me/variable-pitch-serif-font :family))

(setq doom-emoji-fallback-font-families nil)
(setq doom-symbol-fallback-font-families nil)

(setq doom-font
      (font-spec :family (plist-get me/fixed-width-font :family)
                 :style  (plist-get me/fixed-width-font :style)
                 :size   14)
      doom-big-font
      (font-spec :family (plist-get me/fixed-width-font :family)
                 :style  (plist-get me/fixed-width-font :style)
                 :size   20)
      doom-variable-pitch-font
      (font-spec :family (plist-get me/variable-pitch-font :family)
                 :style  (plist-get me/variable-pitch-font :style)
                 :size   16))

(setq doom-theme 'doom-rose-pine-moon)

;; (use-package! catppuccin-theme
;;   :init (setq catppuccin-flavor 'mocha)
;;   :hook (after-init . (lambda () (load-theme 'catppuccin))))

(use-package! envrc
  :hook (after-init . envrc-global-mode))

(defun me/setup-indent (n)
  ;; java/c/c++
  (setq-local c-basic-offset n)

  ;; shell
  (setq-local sh-basic-offset n)

  ;; web development
  (setq-local coffee-tab-width n) ; coffeescript
  (setq-local javascript-indent-level n) ; javascript-mode
  (setq-local js-indent-level n) ; js-mode
  (setq-local js2-basic-offset n) ; js2-mode, in latest js2-mode, it's alias of js-indent-level
  (setq-local web-mode-markup-indent-offset n) ; web-mode, html tag in html file
  (setq-local web-mode-css-indent-offset n) ; web-mode, css in html file
  (setq-local web-mode-code-indent-offset n) ; web-mode, js code in html file
  (setq-local css-indent-offset n) ; css-mode
  (setq-local rustic-mode-indent-offset n) ; css-mode
  )

(defun me/office-code-style ()
  (interactive)
  (message "Office code style!")
  ;; use tab instead of space
  (setq-local indent-tabs-mode t)
  ;; indent 4 spaces width
  (me/setup-indent 4))

(defun me/personal-code-style ()
  (interactive)
  (message "My personal code style!")
  ;; use space instead of tab
  (setq indent-tabs-mode nil)
  ;; indent 2 spaces width
  (me/setup-indent 2))

(defun me/setup-develop-environment ()
  (interactive)
  (me/personal-code-style))

;; How to do this dynamically based on project name:
;; (defun me/setup-develop-environment ()
;;   (interactive)
;;   (let ((proj-dir (file-name-directory (buffer-file-name))))
;;     ;; if hobby project path contains string "hobby-proj1"
;;     (if (string-match-p "hobby-proj1" proj-dir)
;;         (me/personal-code-style))
;;     ;; if commericial project path contains string "commerical-proj"
;;     (if (string-match-p "commerical-proj" proj-dir)
;;         (me/office-code-style))))

;; prog-mode-hook requires emacs24+
(add-hook 'prog-mode-hook 'me/setup-develop-environment)
;; a few major-modes does NOT inherited from prog-mode
(add-hook 'web-mode-hook 'me/setup-develop-environment)

(defun me/org-font-setup ()
  (dolist (face '((:name org-level-1 :weight bold   :height 1.3)
                  (:name org-level-2 :weight bold   :height 1.2)
                  (:name org-level-3 :weight bold   :height 1.1)
                  (:name org-level-4 :weight normal :height 1.1)
                  (:name org-level-5 :weight normal :height 1.1)
                  (:name org-level-6 :weight normal :height 1.1)
                  (:name org-level-7 :weight normal :height 1.1)
                  (:name org-level-8 :weight normal :height 1.1)))

    (set-face-attribute (plist-get face :name) nil
                        :family me/org-font-family
                        :weight (plist-get face :weight)
                        :height (plist-get face :height))))

(require 'org-tempo)
(require 'org-habit)

(after! org
  (setq
   org-ellipsis " ▾"
   org-directory "~/projects/org/"
   org-agenda-files '("~/projects/org/agenda.org"
                      "~/projects/org/todo.org"
                      "~/Documents/FlatHabits/MyHabits.org")
   org-log-done 'time)

  (add-to-list 'org-structure-template-alist '("el"  . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("sh"  . "src sh"))
  (add-to-list 'org-structure-template-alist '("iex" . "src elixir"))
  (variable-pitch-mode 1)
  (me/org-font-setup))

(add-hook 'org-mode-hook (lambda ()
                           (visual-fill-column-mode 1)
                           (setq-local visual-fill-column-center-text t
                                       visual-fill-column-width 100)

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

;; (use-package! org-roam
;;   :defer t
;;   :init
;;   (setq org-roam-directory "~/Documents/OrgRoam")
;;   (setq +org-roam-open-buffer-on-find-file nil))

(setq-default flycheck-disabled-checkers '(proselint))

(setq
 treemacs-width 30
 treemacs-follow-mode t
 treemacs-position 'left
 doom-themes-treemacs-theme "doom-colors")

(after! centaur-tabs
  (centaur-tabs-group-by-projectile-project)
  (setq
   centaur-tabs-style "bar"
   centaur-tabs-set-bar 'none
   centaur-tabs-bar-height 30
   centaur-tabs-height 28)

  (centaur-tabs-change-fonts (plist-get me/fixed-width-font :family) 130)

  (defun centaur-tabs-hide-tab (x)
    "Do no to show buffer X in tabs."
    (let ((name (format "%s" x)))
        (or
        ;; Current window is not dedicated window.
        (window-dedicated-p (selected-window))

        ;; Buffer name not match below blacklist.
        (string-suffix-p "ex[web]" name)
        (string-prefix-p "*epc" name)
        (string-prefix-p "*helm" name)
        (string-prefix-p "*Helm" name)
        (string-prefix-p "*Compile-Log*" name)
        (string-prefix-p "*lsp" name)
        (string-prefix-p "*company" name)
        (string-prefix-p "*Flycheck" name)
        (string-prefix-p "*tramp" name)
        (string-prefix-p " *Mini" name)
        (string-prefix-p "*help" name)
        (string-prefix-p "*straight" name)
        (string-prefix-p " *temp" name)
        (string-prefix-p "*Help" name)
        (string-prefix-p "*mybuf" name)

        ;; Is not magit buffer.
        (and (string-prefix-p "magit" name)
            (not (file-name-extension name)))
        )))
)

(after! projectile
  (add-hook 'projectile-after-switch-project-hook (lambda ()
        (if (s-suffix? "printserver/" (projectile-project-root))
            (setq-local lsp-elixir-project-dir "printserver/packages/ex_printserver/"))))
  (setq projectile-ignored-projects '("~/" "/tmp/" "~/.emacs.d/" "/opt/homebrew/"))
  (setq projectile-project-search-path '("~/projects/" "~/campaigns/")))

(remove-hook 'doom-first-input-hook #'evil-snipe-mode)

(use-package! pdf-tools
  :defer t
  :config
  (pdf-loader-install)
  (setq pdf-view-use-scaling t
        pdf-view-use-imagemagick nil))

(setq lsp-enable-file-watchers nil)

(setq company-idle-delay 0.5)

(use-package! claude-code-ide
  :bind ("C-c C-'" . claude-code-ide-menu)
  :config
  (claude-code-ide-emacs-tools-setup))

;; experimental treesitter setup

;; (use-package
;;   emacs
;;   :ensure nil
;;   :custom

;;   ;; Should use:
;;   ;; (mapc #'treesit-install-language-grammar (mapcar #'car treesit-language-source-alist)) ;
;;   ;; at least once per installation or while changing this list
;;   (treesit-language-source-alist
;;    '((heex "https://github.com/phoenixframework/tree-sitter-heex")
;;      (elixir "https://github.com/elixir-lang/tree-sitter-elixir")))
;;   (major-mode-remap-alist
;;    '((elixir-mode . elixir-ts-mode)))
;; )

;; (use-package
;;  eglot
;;  :ensure nil
;;  :config (add-to-list 'eglot-server-programs '(elixir-ts-mode "language_server.sh")))

;; (use-package
;;  eglot
;;  :ensure nil
;;  :config
;;  (add-to-list
;;   'eglot-server-programs `((elixir-ts-mode heex-ts-mode elixir-mode) . ("language_server.sh"))))


;; (use-package
;;  elixir-ts-mode
;;  :hook (elixir-ts-mode . eglot-ensure)
;;  (elixir-ts-mode
;;   .
;;   (lambda ()
;;     (push '(">=" . ?\u2265) prettify-symbols-alist)
;;     (push '("<=" . ?\u2264) prettify-symbols-alist)
;;     (push '("!=" . ?\u2260) prettify-symbols-alist)
;;     (push '("==" . ?\u2A75) prettify-symbols-alist)
;;     (push '("=~" . ?\u2245) prettify-symbols-alist)
;;     (push '("<-" . ?\u2190) prettify-symbols-alist)
;;     (push '("->" . ?\u2192) prettify-symbols-alist)
;;     (push '("<-" . ?\u2190) prettify-symbols-alist)
;;     (push '("|>" . ?\u25B7) prettify-symbols-alist)))
;;  (before-save . eglot-format))

;; bind alchemist keys
;; (setq alchemist-key-command-prefix (kbd doom-localleader-key))

(map! :after elixir-mode
      :map elixir-mode-map
      :localleader
      :n "f" #'elixir-format)

;; Enable format and iex reload on save
(add-hook 'elixir-mode-hook
          (lambda ()
            (add-hook 'before-save-hook 'elixir-format nil t)
            ;;(add-hook 'after-save-hook 'alchemist-iex-reload-module)
            (delete 'elixir-credo flycheck-checkers)
            ))

(setq buffer-save-without-query t)

(use-package! polymode
  :defer t
  :mode ("\.ex$" . poly-elixir-web-mode)
  :config
  (define-hostmode poly-elixir-hostmode :mode 'elixir-mode)
  (define-innermode poly-liveview-expr-elixir-innermode
    :mode 'web-mode
    :head-matcher (rx line-start (* space) "~H" (= 3 (char "\"'")) line-end)
    :tail-matcher (rx line-start (* space) (= 3 (char "\"'")) line-end)
    :head-mode 'host
    :tail-mode 'host
    :allow-nested nil
    :keep-in-mode 'host
    :fallback-mode 'host)
  (define-polymode poly-elixir-web-mode
    :hostmode 'poly-elixir-hostmode
    :innermodes '(poly-liveview-expr-elixir-innermode)))

(after! web-mode
  (dolist (tuple '(("elixir" . "\\.ex\\'")
                   ("elixir" . "\\.eex\\'")
                   ("elixir" . "\\.heex\\'")))
    (add-to-list 'web-mode-engines-alist tuple)))

(setq tramp-default-method "ssh")
(setq tramp-terminal-type "tramp")

(map! :desc "Open Dired here" :n "-" #'dired-jump)

(map! :desc "Centaur Tabs" :n "SPC t t" #'centaur-tabs-mode)
(map! :desc "Next Tab" :g "s-}" #'centaur-tabs-forward)
(map! :desc "Previous Tab" :g "s-{" #'centaur-tabs-backward)

(map! :desc "Decrease current window width" :g "s-[" #'evil-window-decrease-width)
(map! :desc "Increase current window width" :g "s-]" #'evil-window-increase-width)
