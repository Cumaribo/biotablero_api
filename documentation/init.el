;; Paul Johnson
;; 2015-10-01
;;
;; This version? 2015-10-01?
;; I ran into some trouble swapping various versions of ESS in
;; my user directory, so I started tearing this apart. Cutting out
;; stuff I don't remember.
;;
;; My init file alters user interaction with R via ESS. Most
;; importantly
;; 1. *R* should start in its own frame
;; 2. S-[Return] is hot key to send line to *R*. This replaces
;; now default C-[Return]. I need this to protect CUA
;; mode rectangle selection
;; 3. General Emacs settings discourage "frame splitting" that
;; the usual Emacs users seem to like.
;; 4. Slight simplification of the menu under File. I don't see
;; need for separate "create" and "open" choices. So I put them
;; together
;; 5. See next paragraph about the MELPA package manager
;;
;; See my companion lecture
;; "Emacs Has No Learning Curve"
;; http://pj.freefaculty.org/guides/Rcourse
;;
;; 2015-03-02
;; Following allows use of Emacs24 package manager.
;; To use, run emacs, then do
;; M-x list-packages
;; To see everything available at that moment on melpa.
;; After that, seriously consider installing ess-smart-underscore.
;; M-x package-refresh-contents
(when (>= emacs-major-version 24)
  (require 'package)
  (package-initialize)
  (add-to-list 'package-archives
	       '("melpa" . "http://melpa.milkbox.net/packages/") t)
  )
(require 'package)
(add-to-list 'package-archives
	     '("elpy" . "http://jorgenschaefer.github.io/packages/"))
;; Some python things I use, you may not need
;;(setq python-shell-interpreter "ipython3")
;;(autoload 'python-mode "python-mode" "Python Mode." t)
;;(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
;;(add-to-list 'interpreter-mode-alist '("python" . python-mode))
;;(package-initialize)
;;(elpy-enable)
(require 'ess-site)
;; soft require: no error if package not found
(require 'ess-smart-underscore nil 'noerror)
(add-to-list 'auto-mode-alist '("\\.Rmd" . poly-markdown+r-mode))
;; Org-mode with R doesn't work without this
;; http://orgmode.org/worg/org-contrib/babel/how-to-use-Org-Babel-for-R.html
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#2e3436" "#a40000" "#4e9a06" "#c4a000" "#204a87" "#5c3566" "#729fcf" "#eeeeec"])
 '(cua-auto-tabify-rectangles nil)
 '(cua-mode t nil (cua-base))
 '(custom-enabled-themes '(moe-dark))
 '(custom-safe-themes
   '("afeb7b07dbc1a4cfadb24f3ef6c8cf5e63051bf76411779f03a0fe3aadc07768" "776c1ab52648f98893a2aa35af2afc43b8c11dd3194a052e0b2502acca02bfce" "a68624bd5c4bec879ee59cd3039531b6229766a8b8ed0e79eef2642f14dbda32" "58c996beb973f7e988ee4fd21c367b7a5bbdb0622ddfbbd112672a7b4e3d3b81" default))
 '(elpy-rpc-python-command "python3")
 '(org-babel-load-languages '((emacs-lisp . t) (R . t)))
 '(org-confirm-babel-evaluate nil)
 '(show-paren-mode t))
(setq default-tab-width 4)
;; Section II. Keyboard and mouse customization
;; Mouse and cursor in the usual Mac/Windows way
(delete-selection-mode t)
;; http://stackoverflow.com/questions/13036155/how-to-to-combine-emacs-primary-clipboard-copy-and-paste-behavioron-ms-windows
(setq select-active-regions t)
;; Trying to make mouse middle-click only paste from primary
;; X11 selection, not clipboard and kill ring:
(global-set-key [mouse-2] 'mouse-yank-primary)
;; highlight does not alter Kill ring:
(setq mouse-drag-copy-region nil)
;; windows style binding C-x, C-v, C-c, C-z:
(cua-mode t)
;; Don't tabify after rectangle commands:
(setq cua-auto-tabify-rectangles nil)
;; Section III. Programming conveniences:
;; light-up matching parens:
(show-paren-mode t)
;; turn on syntax highlighting:
(global-font-lock-mode t)
;; Auto fill is TRUE in text modes:
(setq text-mode-hook (quote (turn-on-auto-fill text-mode-hook-identify)))
;; Section IV. ESS Emacs Statistics
;; start R in current working directory, don't let R ask user:
(setq ess-ask-for-ess-directory nil)
;; Change shortcut "run this line" to use Shift-Return
;; Suggested by Vitalie Spinu 2013-09-30 to co-exist with Windows Emacs
(eval-after-load "ess-mode"
  '(progn
     (define-key ess-mode-map [(control return)] nil)
     (define-key ess-mode-map [(shift return)] 'ess-eval-region-or-line-and-step))
  )
;; Help in frames? Consider;; 1) create a new frame for each help instance
;; (setq ess-help-own-frame t)
;; or 2) If you want all help buffers to go into one frame do:
(setq ess-help-own-frame 'one)
;; I want the *R* process in its own frame
(setq inferior-ess-own-frame t)
(setq inferior-ess-same-window nil)
;; Section V. Frames oriented Emacs
;; Discourage Emacs from horizontal splitting:
;; http://www.gnu.org/software/emacs/elisp/html_node/Choosing-Window.html
;;(setq split-window-preferred-function nil)
;; (setq pop-up-windows nil)
;; Make files opened from the menu bar appear in their own
;; frames. Adapted from Emacs menu-bar.el
(defun menu-find-existing ()
  "Edit the existing file FILENAME."
  (interactive)
  (let* ((mustmatch (not (and (fboundp 'x-uses-old-gtk-dialog)
			      (x-uses-old-gtk-dialog))))
	 (filename (car (find-file-read-args "Find file: " mustmatch))))
    (if mustmatch
	(find-file-other-frame filename)
      (find-file filename))))
(define-key menu-bar-file-menu [new-file]
  '(menu-item "Open/Create" find-file-other-frame
	      :enable (menu-bar-non-minibuffer-window-p)
	      :help "Create a new file"))
(define-key menu-bar-file-menu [open-file]
  '(menu-item ,(purecopy "Open File...") menu-find-existing
	      :enable (menu-bar-non-minibuffer-window-p)
	      :help ,(purecopy "Read an existing file into an Emacs buffer")))
;; Open directory list in new frame.
(define-key menu-bar-file-menu [dired]
  '(menu-item "Open Directory..." dired-other-frame
	      :help "Read a directory; operate on its files (Dired)"
	      :enable (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))))
;; Section VI: Miscellaneous convenience
;; Remove Emacs "splash screen" http://fuhm.livejournal.com/
(defadvice command-line-normalize-file-name
    (before kill-stupid-startup-screen activate)
  (setq inhibit-startup-screen t))
(setq inhibit-splash-screen t)
;; Show file name in title bar
;; http://www.thetechrepo.com/main-articles/549
(setq frame-title-format "%b - Emacs")
;; Make Emacs scroll smoothly with down arrow key.
;; faq 5.45 http://www.gnu.org/s/emacs/emacs-faq.html#Modifying-pull_002ddown-menus
(setq scroll-conservatively most-positive-fixnum)
;; adjust the size of the frames, uncomment this, adjust values
;;(setq default-frame-alist '((width . 90) (height . 65)))
;; Remember password when connected to remote sites via Tramp
;; http://stackoverflow.com/questions/840279/passwords-in-emacs-tramp-mode-editing
;; Emacs "tramp" service (ssh connection) constantly
;; asks for the log in password without this
(setq password-cache-expiry nil)
;; Emacs shells work better
;; http://snarfed.org/why_i_run_shells_inside_emacs
(setq ansi-color-for-comint-mode 'filter)
(setq comint-prompt-read-only t)
(setq comint-scroll-to-bottom-on-input t)
(setq comint-scroll-to-bottom-on-output t)
(setq comint-move-point-for-output t)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
;; A stata stanza from Brendan Halpin
;; Stata equivalent of ess-eval-region
(defun delimit-do (start end toggle &optional clear message)
  "Send the current region to the inferior ESS process, Stata do-editor style.
Creates a temporary file, \"do\"-es it, deletes it.
With prefix argument toggle the meaning of `ess-eval-visibly-p';
this does not apply when using the S-plus GUI, see `ess-eval-region-ddeclient'."
  (interactive "r\nP")
  (ess-force-buffer-current "Process to use: ")
  (message "Starting evaluation...")
  (setq message (or message "Eval region"))
  (save-excursion
    ;; don't send new lines (avoid screwing the debugger)
    (goto-char start)
    (skip-chars-forward "\n\t ")
    (setq start (point))
    (unless mark-active
      (ess-blink-region start end))
    ;; don't send new lines at the end (avoid screwing the debugger)
    (goto-char end)
    (skip-chars-backward "\n\t ")
    (setq end (point)))
  (let* (delimit
	 (commands (buffer-substring-no-properties start end))
	 (delimit-do-file (make-temp-file "delimit-do" nil ".do"))
	 (proc (get-process ess-local-process-name))
	 (visibly (if toggle (not ess-eval-visibly-p) ess-eval-visibly-p))
	 (dev-p (process-get proc 'developer))
	 (tb-p (process-get proc 'tracebug)))
    ;; Go to the start of the section and look back for #delimit
    ;; if found set delimit unless the delimiter is not ";"
    (save-excursion
      (goto-char start)
      (setq delimit (re-search-backward "^#delimit +\\(.+\\)$" nil t))
      (if delimit
	  (if (not (string-match ";" (match-string 1))) (setq delimit nil))))
    (with-temp-buffer
      (if clear (insert "clear\n"))
      (if delimit (insert "#delimit ;\n"
			  commands
			  "\n#delimit cr\n")
	(insert commands "\n"))
      (write-file delimit-do-file nil)
      (kill-buffer (current-buffer)))
    (process-send-string
     (get-ess-process ess-current-process-name)
     (format "do %s\nrm %s\n" delimit-do-file delimit-do-file))
    )
  (if (and (fboundp 'deactivate-mark) ess-eval-deactivate-mark)
      (deactivate-mark))
  ;; return value
  (list start end))

