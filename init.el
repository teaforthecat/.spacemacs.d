;; -*- mode: emacs-lisp -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  "Configuration Layers declaration.
You should not put any user code in this function besides modifying the variable
values."
  (setq-default
   ;; Base distribution to use. This is a layer contained in the directory
   ;; `+distribution'. For now available distributions are `spacemacs-base'
   ;; or `spacemacs'. (default 'spacemacs)
   dotspacemacs-distribution 'spacemacs
   ;; Lazy installation of layers (i.e. layers are installed only when a file
   ;; with a supported type is opened). Possible values are `all', `unused'
   ;; and `nil'. `unused' will lazy install only unused layers (i.e. layers
   ;; not listed in variable `dotspacemacs-configuration-layers'), `all' will
   ;; lazy install any layer that support lazy installation even the layers
   ;; listed in `dotspacemacs-configuration-layers'. `nil' disable the lazy
   ;; installation feature and you have to explicitly list a layer in the
   ;; variable `dotspacemacs-configuration-layers' to install it.
   ;; (default 'unused)
   dotspacemacs-enable-lazy-installation 'unused
   ;; If non-nil then Spacemacs will ask for confirmation before installing
   ;; a layer lazily. (default t)
   dotspacemacs-ask-for-lazy-installation t
   ;; If non-nil layers with lazy install support are lazy installed.
   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
   ;; dotspacemacs-configuration-layer-path '("~/.spacemacs.d/layers/" "~/.emacs.d/private/")
   ;; List of configuration layers to load. If it is the symbol `all' instead
   ;; of a list then all discovered layers will be installed.
    dotspacemacs-configuration-layers
    '(
      typescript
      csv
      nginx
      python
      docker
      ;; ----------------------------------------------------------------
      ;; Example of useful layers you may want to use right away.
      ;; Uncomment some layer names and press <SPC f e R> (Vim style) or
      ;; <M-m f e R> (Emacs style) to install them.
      ;; ----------------------------------------------------------------
      (auto-completion :variables
                       auto-completion-return-key-behavior nil
                       auto-completion-tab-key-behavior 'complete
                       auto-completion-enable-snippets-in-popup t)
      better-defaults
      emacs-lisp
      (deft :variables
        ;; set org-journal default extension to .org to use this
        deft-directory "~/Documents/journal")
      git
      markdown
      (org :variables
           org-enable-reveal-js-support t)
      (shell :variables
             ;; not sure about this yet
             ;; shell-enable-smart-eshell t ;; holds failed commands on the prompt, ready to be edited
             shell-default-height 30
             shell-default-shell 'eshell
             eshell-rc-script "~/.eshell/profile" ;; back to original default
             shell-default-position 'bottom)
      spell-checking
      syntax-checking
      version-control
      clojure
      (ruby :variables ruby-version-manager 'rbenv)
      ruby-on-rails
      restclient
      java
      sql
      yaml
      javascript
      html
      finance
      puppet
      ;; local
      colemak-hjkl
      pianobar ;; I hit F7 to much
      remote-hosts
      where-am-i
      )

   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages, then consider creating a layer. You can also put the
   ;; configuration in `dotspacemacs/user-config'.
   dotspacemacs-additional-packages '(
                                      typit
                                      ag
                                      csharp-mode ; unity
                                      dizzee
                                      gradle-mode
                                      ob-restclient
                                      org-journal
                                      wgrep-ag
                                      ruby-hash-syntax
                                      terraform-mode
                                      vagrant
                                      dired-collapse
                                      )
   ;; A list of packages that cannot be updated.
   dotspacemacs-frozen-packages '()

   ;; A list of packages and/or extensions that will not be install and loaded.
   dotspacemacs-excluded-packages '(tern
                                    ;;maybe
                                    vi-tilde-fringe
                                    )
   ;; Defines the behaviour of Spacemacs when installing packages.
   ;; Possible values are `used-only', `used-but-keep-unused' and `all'.
   ;; `used-only' installs only explicitly used packages and uninstall any
   ;; unused packages as well as their unused dependencies.
   ;; `used-but-keep-unused' installs only the used packages but won't uninstall
   ;; them if they become unused. `all' installs *all* packages supported by
   ;; Spacemacs and never uninstall them. (default is `used-only')
   dotspacemacs-install-packages 'used-but-keep-unused))

(defun dotspacemacs/init ()
  "Initialization function.
This function is called at the very startup of Spacemacs initialization
before layers configuration.
You should not put any user code in there besides modifying the variable
values."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; If non nil ELPA repositories are contacted via HTTPS whenever it's
   ;; possible. Set it to nil if you have no way to use HTTPS in your
   ;; environment, otherwise it is strongly recommended to let it set to t.
   ;; This variable has no effect if Emacs is launched with the parameter
   ;; `--insecure' which forces the value of this variable to nil.
   ;; (default t)
   dotspacemacs-elpa-https t
   ;; Maximum allowed time in seconds to contact an ELPA repository.
   dotspacemacs-elpa-timeout 5
   ;; If non nil then spacemacs will check for updates at startup
   ;; when the current branch is not `develop'. Note that checking for
   ;; new versions works via git commands, thus it calls GitHub services
   ;; whenever you start Emacs. (default nil)
   dotspacemacs-check-for-update nil
   ;; If non-nil, a form that evaluates to a package directory. For example, to
   ;; use different package directories for different Emacs versions, set this
   ;; to `emacs-version'.
   dotspacemacs-elpa-subdirectory nil
   ;; One of `vim', `emacs' or `hybrid'.
   ;; `hybrid' is like `vim' except that `insert state' is replaced by the
   ;; `hybrid state' with `emacs' key bindings. The value can also be a list
   ;; with `:variables' keyword (similar to layers). Check the editing styles
   ;; section of the documentation for details on available variables.
   ;; (default 'vim)
   dotspacemacs-editing-style 'vim
   ;; If non nil output loading progress in `*Messages*' buffer. (default nil)
   dotspacemacs-verbose-loading nil
   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed. (default 'official)
   dotspacemacs-startup-banner 'official
   ;; List of items to show in startup buffer or an association list of
   ;; the form `(list-type . list-size)`. If nil then it is disabled.
   ;; Possible values for list-type are:
   ;; `recents' `bookmarks' `projects' `agenda' `todos'."
   ;; List sizes may be nil, in which case
   ;; `spacemacs-buffer-startup-lists-length' takes effect.
   dotspacemacs-startup-lists '((recents . 5)
                                (projects . 7))
   ;; True if the home buffer should respond to resize events.
   dotspacemacs-startup-buffer-responsive t
   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'emacs-lisp-mode
   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press <SPC> T n to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(spacemacs-dark
                         spacemacs-light)
   ;; If non nil the cursor color matches the state color in GUI Emacs.
   dotspacemacs-colorize-cursor-according-to-state t
   ;; Default font, or prioritized list of fonts. `powerline-scale' allows to
   ;; quickly tweak the mode-line size to make separators look not too crappy.
   dotspacemacs-default-font '( "Inconsolata" ;;"Source Code Pro"
                               :size 16
                               :weight normal
                               :width normal
                               :powerline-scale 1.1)
   ;; The leader key
   dotspacemacs-leader-key "SPC"
   ;; The key used for Emacs commands (M-x) (after pressing on the leader key).
   ;; (default "SPC")
   dotspacemacs-emacs-command-key "SPC"
   ;; The key used for Vim Ex commands (default ":")
   dotspacemacs-ex-command-key ":"
   ;; The leader key accessible in `emacs state' and `insert state'
   ;; (default "M-m")
   dotspacemacs-emacs-leader-key "M-m"
   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. Set it to `nil` to disable it. (default ",")
   dotspacemacs-major-mode-leader-key ","
   ;; Major mode leader key accessible in `emacs state' and `insert state'.
   ;; (default "C-M-m")
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   ;; These variables control whether separate commands are bound in the GUI to
   ;; the key pairs C-i, TAB and C-m, RET.
   ;; Setting it to a non-nil value, allows for separate commands under <C-i>
   ;; and TAB or <C-m> and RET.
   ;; In the terminal, these pairs are generally indistinguishable, so this only
   ;; works in the GUI. (default nil)
   dotspacemacs-distinguish-gui-tab nil
   ;; If non nil `Y' is remapped to `y$' in Evil states. (default nil)
   dotspacemacs-remap-Y-to-y$ nil
   ;; If non-nil, the shift mappings `<' and `>' retain visual state if used
   ;; there. (default t)
   dotspacemacs-retain-visual-state-on-shift t
   ;; If non-nil, J and K move lines up and down when in visual mode.
   ;; (default nil)
   dotspacemacs-visual-line-move-text nil
   ;; If non nil, inverse the meaning of `g' in `:substitute' Evil ex-command.
   ;; (default nil)
   dotspacemacs-ex-substitute-global nil
   ;; Name of the default layout (default "Default")
   dotspacemacs-default-layout-name "Default"
   ;; If non nil the default layout name is displayed in the mode-line.
   ;; (default nil)
   dotspacemacs-display-default-layout nil
   ;; If non nil then the last auto saved layouts are resume automatically upon
   ;; start. (default nil)
   dotspacemacs-auto-resume-layouts nil
   ;; Size (in MB) above which spacemacs will prompt to open the large file
   ;; literally to avoid performance issues. Opening a file literally means that
   ;; no major mode or minor modes are active. (default is 1)
   dotspacemacs-large-file-size 1
   ;; Location where to auto-save files. Possible values are `original' to
   ;; auto-save the file in-place, `cache' to auto-save the file to another
   ;; file stored in the cache directory and `nil' to disable auto-saving.
   ;; (default 'cache)
   dotspacemacs-auto-save-file-location 'cache
   ;; Maximum number of rollback slots to keep in the cache. (default 5)
   dotspacemacs-max-rollback-slots 5
   ;; If non nil, `helm' will try to minimize the space it uses. (default nil)
   dotspacemacs-helm-resize nil
   ;; if non nil, the helm header is hidden when there is only one source.
   ;; (default nil)
   dotspacemacs-helm-no-header nil
   ;; define the position to display `helm', options are `bottom', `top',
   ;; `left', or `right'. (default 'bottom)
   dotspacemacs-helm-position 'bottom
   ;; Controls fuzzy matching in helm. If set to `always', force fuzzy matching
   ;; in all non-asynchronous sources. If set to `source', preserve individual
   ;; source settings. Else, disable fuzzy matching in all sources.
   ;; (default 'always)
   dotspacemacs-helm-use-fuzzy 'always
   ;; If non nil the paste micro-state is enabled. When enabled pressing `p`
   ;; several times cycle between the kill ring content. (default nil)
   dotspacemacs-enable-paste-transient-state nil
   ;; Which-key delay in seconds. The which-key buffer is the popup listing
   ;; the commands bound to the current keystroke sequence. (default 0.4)
   dotspacemacs-which-key-delay 1 ;0.4
   ;; Which-key frame position. Possible values are `right', `bottom' and
   ;; `right-then-bottom'. right-then-bottom tries to display the frame to the
   ;; right; if there is insufficient space it displays it at the bottom.
   ;; (default 'bottom)
   dotspacemacs-which-key-position 'bottom
   ;; If non nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil to boost the loading time. (default t)
   dotspacemacs-loading-progress-bar t
   ;; If non nil the frame is fullscreen when Emacs starts up. (default nil)
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup nil
   ;; If non nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX. (default nil)
   dotspacemacs-fullscreen-use-non-native t
   ;; If non nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (default nil) (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup t
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-active-transparency 90
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-inactive-transparency 90
   ;; If non nil show the titles of transient states. (default t)
   dotspacemacs-show-transient-state-title t
   ;; If non nil show the color guide hint for transient state keys. (default t)
   dotspacemacs-show-transient-state-color-guide t
   ;; If non nil unicode symbols are displayed in the mode line. (default t)
   dotspacemacs-mode-line-unicode-symbols t
   ;; If non nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters point
   ;; when it reaches the top or bottom of the screen. (default t)
   ;; dotspacemacs-smooth-scrolling t
   ;; Control line numbers activation.
   ;; If set to `t' or `relative' line numbers are turned on in all `prog-mode' and
   ;; `text-mode' derivatives. If set to `relative', line numbers are relative.
   ;; This variable can also be set to a property list for finer control:
   ;; '(:relative nil
   ;;   :disabled-for-modes dired-mode
   ;;                       doc-view-mode
   ;;                       markdown-mode
   ;;                       org-mode
   ;;                       pdf-view-mode
   ;;                       text-mode
   ;;   :size-limit-kb 1000)
   ;; (default nil)
   dotspacemacs-line-numbers nil
   ;; Code folding method. Possible values are `evil' and `origami'.
   ;; (default 'evil)
   dotspacemacs-folding-method 'evil
   ;; If non-nil smartparens-strict-mode will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode nil
   ;; If non-nil pressing the closing parenthesis `)' key in insert mode passes
   ;; over any automatically added closing parenthesis, bracket, quote, etc…
   ;; This can be temporary disabled by pressing `C-q' before `)'. (default nil)
   dotspacemacs-smart-closing-parenthesis nil
   ;; Select a scope to highlight delimiters. Possible values are `any',
   ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
   ;; emphasis the current one). (default 'all)
   dotspacemacs-highlight-delimiters 'all
   ;; If non nil, advise quit functions to keep server open when quitting.
   ;; (default nil)
   dotspacemacs-persistent-server nil
   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `ag', `pt', `ack' and `grep'.
   ;; (default '("ag" "pt" "ack" "grep"))
   dotspacemacs-search-tools '("ag" "pt" "ack" "grep")
   ;; The default package repository used if no explicit repository has been
   ;; specified with an installed package.
   ;; Not used for now. (default nil)
   dotspacemacs-default-package-repository nil
   ;; Delete whitespace while saving buffer. Possible values are `all'
   ;; to aggressively delete empty line and long sequences of whitespace,
   ;; `trailing' to delete only the whitespace at end of lines, `changed'to
   ;; delete only whitespace for changed lines or `nil' to disable cleanup.
   ;; (default nil)
   dotspacemacs-whitespace-cleanup 'changed
   ;; start my additions
   git-magit-status-fullscreen t
   ))


(defun dotspacemacs/user-init ()
  "Initialization function for user code.
It is called immediately after `dotspacemacs/init', before layer configuration
executes.
 This function is mostly useful for variables that need to be set
before packages are loaded. If you are unsure, you should try in setting them in
`dotspacemacs/user-config' first."

  (setq eshell-hist-ignoredups t)

  )

(defun dotspacemacs/user-config ()
  "Configuration function for user code.
This function is called at the very end of Spacemacs initialization after
layers configuration.
This is the place where most of your configurations should be done. Unless it is
explicitly specified that a variable should be set before a package is loaded,
you should place your code here."

  (add-hook 'text-mode-hook 'auto-fill-mode)

  (require 'wgrep-ag)

  (eval-after-load 'evil-magit
    '(progn
      (evil-magit-define-key 'normal 'magit-mode-map "h" 'evil-previous-visual-line)

      (evil-magit-define-key 'normal 'magit-mode-map "k" 'evil-next-visual-line)))


  ;; this should be file or directory local
  ;; (setq org-export-babel-evaluate nil)
  (use-package ob-sql)

  (setq avy-all-windows nil) ;; can't go into customize

  (setq epa-file-encrypt-to "teaforthecat@gmail.com")

  ;; ;; ALL FAILS
  ;; ;; (require 'gradle-mode)
  ;; ;; (add-hook 'java-mode-hook '(lambda() (gradle-mode 1)))
  ;; for CORLA
  (add-hook 'java-mode-hook '(lambda() (setq c-basic-offset 2)))
  (remove-hook 'java-mode-hook 'eclim-mode)

  ;; ;; Eclim setup
  ;; ;; brew cask install eclipse #installs to /Applications/Eclipse.app
  ;; ;; mkdir ~/opt #optional
  ;; ;; open /Applications/Eclipse.app #set workspace in UI to ~/opt/eclipse-workspace and quit
  ;; ;; # Not sure how to use curl here actually
  ;; ;; curl  https://github.com/ervandew/eclim/releases/download/2.7.2/eclim_2.7.2.bin > ~/Downloads/eclim_2.7.2.bin
  ;; ;; chmod +x ~/Downloads/eclim_2.7.2.bin
  ;; ;; ~/Downloads/eclim_2.7.2.bin --vimfiles=skip --eclipse=/Applications/Eclipse\ Java.app --plugins jdt
  ;; ;; ln -s /Applications/Eclipse\ Java.app /Applications/Eclipse_Java.app
  ;; (setq eclim-eclipse-dirs '("/Applications/Eclipse.app/Contents/Eclipse/")
  ;;       eclim-executable "/Applications/Eclipse.app/Contents/Eclipse/eclimd"
  ;;       eclimd-default-workspace "~/opt/eclipse-workspace")
  ;; (require 'lsp-java)
  ;; (add-hook 'java-mode-hook #'lsp-java-enable)
  ;; ;; (setq lsp-java-server-install-dir "/Applications/Eclipse.app/Contents/Eclipse")
  ;; (setq lsp-java-server-install-dir "/Users/cthompson/Downloads/jdt-language-server-0.21.0-201806152234")

  ;; (setq eclim-executable "/Applications/Eclipse.app/Contents/Eclipse/eclimd")
  ;; ;; ALL FAILS

  (use-package dired
    :config
    ;; this doesn't work help!
    ;; (bind-key "C-k" 'dired-kill-subdir dired-mode-map)
    ;; (bind-key "I" 'dired-kill-subdir dired-mode-map)
    )

  (use-package dired-collapse)
  ;; not sure how to use this
  ;; (dired-collapse-mode)

  ;; todo use this
  (use-package dizzee)

  (use-package exec-path-from-shell
    :config
    (when (memq window-system '(mac ns x))
      (exec-path-from-shell-initialize)))

  (require  'ox-md)

  (use-package org-journal)

  (defun ct/popup-compilation ()
    (interactive)
    (popwin:popup-buffer "*compilation*"))

  (spacemacs/set-leader-keys
    "wpb" 'popwin:popup-buffer
    "wps" 'popwin:stick-popup-window
    "wpc" 'ct/popup-compilation
    "DEL" 'avy-goto-subword-1
    "p`"  'ct/project-root-shell
    ;; always async
    "!" 'async-shell-command
    "cup" 'copy-unfilled-paragraph
    "aoP" 'org-pomodoro
    )

  (defun ct/project-root-shell ()
    (interactive)
    (let ((default-directory (projectile-project-root)))
      (let ((buff (get-buffer-create (concat "*eshell - "
                                             (projectile-project-name)
                                             "*" ))))
        (with-current-buffer buff
          (unless (eql major-mode 'eshell-mode)
            (eshell-mode)))
        (popwin:close-popup-window) ;; if window already exists
        (popwin:popup-buffer buff)
        (popwin:stick-popup-window))))

  (defun eshell/clear-all ()
    "Clear the eshell buffer."
    (interactive)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (eshell-send-input)))

  (setenv "PGHOST" "127.0.0.1") ;; postgres running in docker

  (setenv "PATH"
          (concat  (getenv "PATH") ":" "/Library/Tex/texbin"))

  ;; maybe put something else here - spc b b is not that bad
  ;; (global-set-key (kbd "<C-escape>") 'helm-mini) ;; switch buffer

  ;; not sure this is neeeded anymore
  ;; (setq cider-cljs-lein-repl "(do (use 'figwheel-sidecar.repl-api) (start-figwheel!) (cljs-repl))")


  (defadvice evil-join-whitespace (after fixup-whitespace activate)
    (fixup-whitespace))

  (defadvice evil-lisp-state-sp-kill-sexp (after fixup-whitespace activate)
    (just-one-space)
    (indent-according-to-mode))

  (defadvice just-one-space (after no-tail-space activate)
    (if (looking-at (regexp-quote ")"))
        ;; the space
        (call-interactively 'delete-backward-char)))

  (defadvice spacemacs/shell-pop-eshell (after cd-to-root activate)
    (ignore-errors ; in case outside of project
      (eshell/cd (projectile-project-root))))


  (setq avy-background t)


  (use-package
     ediff
    :config
    (progn

      (defun ct/ediff-dirs-hide-same ()
        ;; marks equal files for hiding as if =h were typed
        (ediff-meta-mark-equal-files '?h)
        ;; does the hiding
        (ediff-hide-marked-sessions nil))

      (setq ediff-after-session-group-setup-hook '(ct/ediff-dirs-hide-same))

      (setq ediff-default-filtering-regexp
            '(car (or ediff-filtering-regexp-history '("^[^.].*$")) )))))

;; utility function
(defun copy-unfilled-paragraph ()
  (interactive)
  (save-excursion
    (unfill-paragraph)
    (mark-paragraph)
    (call-interactively 'copy-region-as-kill)
    (fill-paragraph)))

;; this may solve the "Text is read only" problem that prevents eshell from
;; being able to close the buffer
(setq eshell-prompt-regexp "^[^λ]+ λ ")

;; help gets in the way when I'm stumbling around
(global-unset-key (kbd "C-h h"))


(defun shasum ()
  (interactive)
  (dired-do-shell-command "shasum -a256" nil (list (file-name-nondirectory
                                                    (dired-get-file-for-visit))))
  (with-current-buffer "*Shell Command Output*"
    (kill-ring-save (point-min) (point-max))))

;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ledger-schedule-file "~/Documents/journal/ledger/schedule.ledger")
 '(linum-format "%4d")
 '(org-babel-load-languages (quote ((sql . t) (shell . t) (emacs-lisp . t))))
 '(org-crypt-key "CB13A538")
 '(package-selected-packages
   (quote
    (pianobar password-store dizzee dired-collapse tabula-rasa terraform-mode tide typescript-mode hledger-mode wgrep mmt powerline rake pcre2el ox-reveal org-category-capture alert log4e gntp org-mime markdown-mode skewer-mode simple-httpd js2-mode parent-mode projectile request haml-mode gitignore-mode fringe-helper git-gutter+ git-gutter flyspell-correct pos-tip flycheck flx magit git-commit ghub let-alist with-editor smartparens iedit anzu evil goto-chg undo-tree json-mode tablist magit-popup docker-tramp json-snatcher json-reformat diminish web-completion-data dash-functional tern restclient know-your-http-well eclim company hydra inflections edn multiple-cursors paredit peg eval-sexp-fu highlight cider seq spinner queue pkg-info clojure-mode epl inf-ruby bind-map bind-key yasnippet packed anaconda-mode pythonic f s dash helm avy helm-core async auto-complete popup csv-mode ob-sql-mode memento org-journal org-ehtml toml-mode racer flycheck-rust cargo rust-mode deft vi-tilde-fringe yapfify yaml-mode xterm-color ws-butler winum which-key wgrep-ag web-mode web-beautify volatile-highlights vagrant uuidgen use-package unfill typit toc-org tagedit sql-indent spaceline smeargle slim-mode shell-pop scss-mode sass-mode rvm ruby-tools ruby-test-mode ruby-hash-syntax rubocop rspec-mode robe restclient-helm restart-emacs rbenv rainbow-delimiters pyvenv pytest pyenv-mode py-isort puppet-mode pug-mode projectile-rails popwin pip-requirements persp-mode paradox orgit org-projectile org-present org-pomodoro org-plus-contrib org-download org-bullets open-junk-file ob-restclient ob-http nginx-mode neotree mwim multi-term move-text mmm-mode minitest markdown-toc magit-gitflow macrostep lorem-ipsum livid-mode live-py-mode linum-relative link-hint less-css-mode ledger-mode js2-refactor js-doc jinja2-mode info+ indent-guide hy-mode hungry-delete htmlize hl-todo highlight-parentheses highlight-numbers highlight-indentation hide-comnt help-fns+ helm-themes helm-swoop helm-pydoc helm-projectile helm-mode-manager helm-make helm-gitignore helm-flx helm-descbinds helm-css-scss helm-company helm-c-yasnippet helm-ag gradle-mode google-translate golden-ratio gnuplot gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link git-gutter-fringe git-gutter-fringe+ gh-md fuzzy flyspell-correct-helm flycheck-pos-tip flycheck-ledger flx-ido floobits fill-column-indicator feature-mode fancy-battery eyebrowse expand-region exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-search-highlight-persist evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-magit evil-lisp-state evil-indent-plus evil-iedit-state evil-exchange evil-escape evil-ediff evil-args evil-anzu eshell-z eshell-prompt-extras esh-help emmet-mode elisp-slime-nav dumb-jump dockerfile-mode docker diff-hl define-word cython-mode csharp-mode company-web company-tern company-statistics company-restclient company-emacs-eclim company-anaconda column-enforce-mode coffee-mode clojure-snippets clj-refactor clean-aindent-mode cider-eval-sexp-fu chruby bundler auto-yasnippet auto-highlight-symbol auto-dictionary auto-compile aggressive-indent ag adaptive-wrap ace-window ace-link ace-jump-helm-line ac-ispell)))
 '(projectile-ignored-project-function (quote file-remote-p))
 '(safe-local-variable-values
   (quote
    ((cider-ns-refresh-after-fn . "integrant.repl/resume")
     (cider-ns-refresh-before-fn . "integrant.repl/suspend")
     (org-reveal-root . "../bower_components/reveal.js/")
     (cider-refresh-after-fn . "integrant.repl/resume")
     (cider-refresh-before-fn . "integrant.repl/suspend")
     (whitespace-cleanup)
     (org-html-postamble)
     (org-reveal-title-slide . "<h1 class=\"title\">%t</h1>"))))
 '(tramp-histfile-override "$HOME/.sh_history"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(linum ((t (:background "#212026" :foreground "gridColor")))))
