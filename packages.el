;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;;; NOTE: `packages.el' is now generated from `config.org'. Please edit that file
;;;       in Emacs and `packages.el' will be generated automatically!

;; To install a package with Doom you must declare them here and run 'doom sync'
;; on the command line, then restart Emacs for the changes to take effect -- or
;; use 'M-x doom/reload'.

;; To install SOME-PACKAGE from MELPA, ELPA or emacsmirror:
;(package! some-package)

(package! polymode)
(package! catppuccin-theme)
(package! claude-code-ide
  :recipe (:host github :repo "manzaltu/claude-code-ide.el"))

;; (package! gitconfig-mode :recipe (:host github :repo "magit/git-modes" :files ("gitconfig-mode.el")))
;; (package! gitignore-mode :recipe (:host github :repo "magit/git-modes" :files ("gitignore-mode.el")))
;; (package! terraform-mode)
;; (package! zig-mode)
