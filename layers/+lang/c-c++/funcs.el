;;; funcs.el --- C/C++ Layer functions File for Spacemacs
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(require 'cl-lib)
(require 'subr-x)

(defun spacemacs//c-toggle-auto-newline ()
  "Toggle auto-newline."
  (c-toggle-auto-newline 1))


;; clang

(defun spacemacs/clang-format-function (&optional style)
  "Format the current function with clang-format according to STYLE."
  (interactive)
  (save-excursion
    (c-mark-function)
    (clang-format (region-beginning) (region-end) style)
    (deactivate-mark) ; If the function is already formatted, then remove the mark
    (message "Formatted function %s" (c-defun-name))))

(defun spacemacs/clang-format-region-or-buffer (&optional style)
  "Format the current region or buffer with clang-format according to STYLE."
  (interactive)
  (save-excursion
    (if (region-active-p)
        (progn
          (clang-format-region (region-beginning) (region-end) style)
          (message "Formatted region"))
      (progn
        (clang-format-buffer style)
        (message "Formatted buffer %s" (buffer-name))))))

(defun spacemacs//clang-format-on-save ()
  "Format the current buffer with clang-format on save when
`c-c++-enable-clang-format-on-save' is non-nil."
  (when c-c++-enable-clang-format-on-save
    (spacemacs/clang-format-region-or-buffer)))

(defun spacemacs/clang-format-on-save ()
  "Add before-save hook for clang-format."
  (add-hook 'before-save-hook 'spacemacs//clang-format-on-save nil t))

(defun spacemacs/company-more-than-prefix-guesser ()
  (spacemacs/c-c++-load-clang-args)
  (company-clang-guess-prefix))

;; Based on the Sarcasm/irony-mode compilation database code.
(defun spacemacs/company-find-clang-complete-file ()
  (when buffer-file-name
    (let ((dir (locate-dominating-file buffer-file-name ".clang_complete")))
      (when dir
        (concat (file-name-as-directory dir) ".clang_complete")))))

;; Based on the Sarcasm/irony-mode compilation database code.
(defun spacemacs/company-load-clang-complete-file (cc-file)
  "Load the flags from CC-FILE, one flag per line."
  (let ((invocation-dir (expand-file-name (file-name-directory cc-file)))
        (case-fold-search nil)
        (include-regex "\\(-I\\|-isystem\\|-iquote\\|-idirafter\\)\\s-*\\(\\S-+\\)")
        compile-flags)
    (with-temp-buffer
      (insert-file-contents cc-file)
      ;; Replace relative paths with absolute paths (by @trishume)
      ;; (goto-char (point-min))
      (while (re-search-forward include-regex nil t)
        (replace-match (format "%s%s" (match-string 1)
                               (expand-file-name (match-string 2)
                                                 invocation-dir))))
      ;; Turn lines into a list
      (setq compile-flags
            ;; remove whitespaces at the end of each line, if any
            (mapcar #'(lambda (line)
                        (if (string-match "[ \t]+$" line)
                            (replace-match "" t t line)
                          line))
                    (split-string (buffer-string) "\n" t))))
    compile-flags))

(defun spacemacs//c-c++-get-standard-include-paths (lang)
  "Returns the default system header include paths for LANG if gcc is on the
system and supports it, else returns a default set of include paths."
  (let* ((start "#include <...> search starts here:")
         (end "End of search list.")
         (gcc-tmplt "echo | gcc -x%s -E -v - 2>&1")
         (sed-tmplt " | sed -n '/%s/,/%s/{/%s/b;/%s/b;p}' | sed -e 's/^ *//g'")
         (template (concat gcc-tmplt sed-tmplt))
         (inc-dirs-cmd (format template lang start end start end))
         (inc-dirs (split-string (shell-command-to-string inc-dirs-cmd)
                                 "\n" t)))
    (if (and inc-dirs (every 'file-exists-p inc-dirs))
        inc-dirs
      '("/usr/include" "/usr/local/include"))))

(defun spacemacs//filter-and-substring (flags filter-prefix substr-index)
  "Returns all the strings in FLAGS starting with FILTER-PREFIX. The returned
strings are substringed from SUBSTR-INDEX inclusive to the end of the string."
  (mapcar (lambda (f) (substring f substr-index))
          (remove-if-not (lambda (f) (string-prefix-p filter-prefix f))
                         flags)))

(defun spacemacs/c-c++-load-clang-args ()
  "Sets the arguments for company-clang, the system paths for company-c-headers
and the arguments for flyckeck-clang based on a project-specific text file."
  (unless company-clang-arguments
    (let* ((cc-file (spacemacs/company-find-clang-complete-file))
           (flags (if cc-file
                      (spacemacs/company-load-clang-complete-file cc-file)
                    '()))
           (i-paths (spacemacs//filter-and-substring flags
                                                     "-I" 2))
           (iquote-paths (spacemacs//filter-and-substring flags
                                                          "-iquote" 7))
           (isystem-paths (spacemacs//filter-and-substring flags
                                                           "-isystem" 8))
           (idirafter-paths (spacemacs//filter-and-substring flags
                                                             "-idirafter" 10)))
      (setq-local company-clang-arguments flags)
      (setq-local flycheck-clang-args flags)
      (setq-local company-c-headers-path-user
                  (append '(".")
                          iquote-paths))
      (setq-local company-c-headers-path-system
                  (append i-paths
                          isystem-paths
                          (when (string-equal major-mode "c++-mode")
                            (spacemacs//c-c++-get-standard-include-paths "c++"))
                          (when (string-equal major-mode "c-mode")
                            (spacemacs//c-c++-get-standard-include-paths "c"))
                          idirafter-paths)))))


;; rtags

(defun spacemacs/c-c++-use-rtags (&optional useFileManager)
  "Return non-nil if rtags function should be used."
  ;; this function is used to fallback on gtags function if rtags is not
  ;; supported. So if gtags layer is not used we disable the fallback by
  ;; returning always t.
  (or (not (configuration-layer/layer-used-p 'gtags))
      (and (rtags-executable-find "rc")
           (cond ((not (gtags-get-rootpath)) t)
                 ((and (not (eq major-mode 'c++-mode))
                       (not (eq major-mode 'c-mode))) (rtags-has-filemanager))
                 (useFileManager (rtags-has-filemanager))
                 (t (rtags-is-indexed))))))

(defun spacemacs/c-c++-tags-find-symbol-at-point (&optional prefix)
  (interactive "P")
  (if (and (not (rtags-find-symbol-at-point prefix))
           rtags-last-request-not-indexed)
      (gtags-find-tag)))

(defun spacemacs/c-c++-tags-find-references-at-point (&optional prefix)
  (interactive "P")
  (if (and (not (rtags-find-references-at-point prefix))
           rtags-last-request-not-indexed)
      (gtags-find-rtag)))

(defun spacemacs/c-c++-tags-find-symbol ()
  (interactive)
  (call-interactively (if (spacemacs/c-c++-use-rtags)
                          'rtags-find-symbol 'gtags-find-symbol)))

(defun spacemacs/c-c++-tags-find-references ()
  (interactive)
  (call-interactively (if (spacemacs/c-c++-use-rtags)
                          'rtags-find-references 'gtags-find-rtag)))

(defun spacemacs/c-c++-tags-find-file ()
  (interactive)
  (call-interactively (if (spacemacs/c-c++-use-rtags t)
                          'rtags-find-file 'gtags-find-file)))

(defun spacemacs/c-c++-tags-imenu ()
  (interactive)
  (call-interactively (if (spacemacs/c-c++-use-rtags t)
                          'rtags-imenu 'idomenu)))


;; lsp
(defun spacemacs//c-c++-lsp-enabled ()
  "Return true if one or other of the lsp backends is enabled"
  (member c-c++-backend c-c++-lsp-backends))

;; -- BEGIN helper functions for common configuration of cquery and ccls backends
(defun spacemacs//c-c++-lsp-backend ()
  "Return a string representation of the LSP backend specified by the `c-c++-backend' configuration variable, without the `lsp-' prefix."
  (ecase c-c++-backend
    ('lsp-ccls "ccls")
    ('lsp-cquery "cquery")))

(defun spacemacs//c-c++-lsp-string (prefix suffix)
  (concat prefix (spacemacs//c-c++-lsp-backend) suffix))

(defun spacemacs//c-c++-lsp-symbol (prefix suffix)
  "Return a symbol for the LSP backend specified by the `c-c++-backend' configuration variable."
  (intern (spacemacs//c-c++-lsp-string prefix suffix)))

(defun spacemacs//c-c++-lsp-call-function (prefix suffix &rest args)
  (apply (spacemacs//c-c++-lsp-symbol prefix suffix) args))

(defun spacemacs//c-c++-lsp-funcall-interactively (prefix suffix &rest args)
  (funcall-interactively (spacemacs//c-c++-lsp-symbol prefix suffix) args))

(defun spacemacs//c-c++-lsp-funcall-interactively-no-args (prefix suffix)
  (funcall-interactively (spacemacs//c-c++-lsp-symbol prefix suffix)))

(defun spacemacs//c-c++-lsp-set-symbol (prefix suffix value)
  (set (spacemacs//c-c++-lsp-symbol prefix suffix) (symbol-value value)))

(defun spacemacs//c-c++-lsp-set-config (param prefix suffix)
  (when (symbol-value param) (spacemacs//c-c++-lsp-set-symbol prefix suffix param)))

(defun spacemacs//c-c++-lsp-apply-config (suffix)
  (spacemacs//c-c++-lsp-set-config (intern (concat "c-c++-lsp-" suffix)) nil (concat "-" suffix)))
;; -- END helper functions for common configuration of cquery and ccls backends


(defun spacemacs//c-c++-lsp-enable ()
  "Enable the LSP backend specified by the `c-c++-backend' configuration variable."
    (progn (condition-case nil
             (spacemacs//c-c++-lsp-call-function "lsp-" "-enable")
             (user-error nil))))

(defun spacemacs//c-c++-lsp-config ()
  "Configure the LSP backend specified by the `c-c++-backend' configuration variable."
    (progn
      (spacemacs//c-c++-lsp-setup-company)
      (spacemacs//c-c++-lsp-customise-lsp-ui-peek)
      (spacemacs//c-c++-lsp-wrap-functions)
      (setq-default flycheck-disabled-checkers '(c/c++-clang c/c++-gcc))

      (if (eq c-c++-lsp-cache-dir nil)
        (progn
          (setq c-c++-lsp-cache-dir (file-truename(concat "~/.emacs.d/.cache/" (symbol-name c-c++-backend))))
          (message (concat "c-c++: No c-c++-lsp-cache-dir specified: defaulting to " c-c++-lsp-cache-dir))))

      (dolist (param '("executable" "extra-init-params" "cache-dir" "project-whitelist" "project-blacklist" "sem-highlight-method"))
        (spacemacs//c-c++-lsp-apply-config param))

      (when c-c++-lsp-sem-highlight-rainbow
        (unless c-c++-lsp-sem-highlight-method
          (progn
            (setq c-c++-lsp-sem-highlight-method 'font-lock)
            (message "c-c++: No semantic highlight method specified. Defaulting to `font-lock'.")))
        (ecase c-c++-backend
          ('lsp-cquery (cquery-use-default-rainbow-sem-highlight))
          ('lsp-ccls (ccls-use-default-rainbow-sem-highlight))))

      (dolist (mode c-c++-modes)
        (spacemacs/lsp-bind-keys-for-mode mode)
        (spacemacs//c-c++-lsp-bind-keys-for-mode mode))

      (evil-set-initial-state '(spacemacs//c-c++-lsp-symbol nil "-tree-mode") 'emacs)
      ;;evil-record-macro keybinding clobbers q in cquery-tree-mode-map for some reason?
      (evil-make-overriding-map (symbol-value (spacemacs//c-c++-lsp-symbol nil "-tree-mode-map")))))

(defun spacemacs//c-c++-lsp-setup-company ()
  "Setup LSP backend auto-completion."
    (progn
      (spacemacs|add-company-backends :backends company-lsp :modes c-mode-common)
      ;;Disable client-side cache and sorting, as server does a better job
      (setq company-transformers nil company-lsp-async t company-lsp-cache-candidates nil)))

(defun spacemacs//c-c++-lsp-wrap-functions ()
  "Wrap navigation functions for the LSP backend specified by the `c-c++-backend' configuration variable."
  (defun c-c++/call-hierarchy () (interactive) (spacemacs//c-c++-lsp-funcall-interactively nil "-call-hierarchy" nil))
  (defun c-c++/call-hierarchy-inv () (interactive) (spacemacs//c-c++-lsp-funcall-interactively nil "-call-hierarchy" t))
  (defun c-c++/inheritance-hierarchy () (interactive) (spacemacs//c-c++-lsp-funcall-interactively nil "-inheritance-hierarchy"))
  (defun c-c++/inheritance-hierarchy-inv () (interactive) (spacemacs//c-c++-lsp-funcall-interactively nil "-inheritance-hierarchy" t))
  (defun c-c++/member-hierarchy () (interactive) (spacemacs//c-c++-lsp-funcall-interactively-no-args nil "-member-hierarchy"))
  (defun c-c++/freshen-index () (interactive) (spacemacs//c-c++-lsp-funcall-interactively nil "-freshen-index"))
  (defun c-c++/preprocess-file () (interactive) (spacemacs//c-c++-lsp-funcall-interactively nil "-preprocess-file")))

(defun spacemacs//c-c++-lsp-bind-keys-for-mode (mode)
  "Bind LSP backend functions for the specificed mode."
  (spacemacs/set-leader-keys-for-major-mode mode
    ;; goto
    ;; FIXME gf conflicts with `lsp-ui-flycheck-list' binding in lsp layer
    ;; Better corrected in lsp-layer? rebinding here for now
    "ge" #'lsp-ui-flycheck-list
    "gf" 'find-file-at-point
    "gF" 'ffap-other-window
    "g&" #'c-c++/references-address
    "gR" #'c-c++/references-read
    "gW" #'c-c++/references-write
    "gc" #'c-c++/callers
    "gv" #'c-c++/vars
    ;; help/hierarchy
    "hb" #'c-c++/base ;;Replace this with lsp-goto-implementation in lsp-layer?
    "hc" #'c-c++/call-hierarchy
    "hC" #'c-c++/call-hierarchy-inv
    "hi" #'c-c++/inheritance-hierarchy
    "hI" #'c-c++/inheritance-hierarchy-inv
    "hm" #'c-c++/member-hierarchy
    ;; lsp/backend
    "lf" #'c-c++/freshen-index
    "lp" #'c-c++/preprocess-file))

(defun spacemacs//c-c++-lsp-customise-lsp-ui-peek ()
  "Wrap some backend-specific features using the `lsp-ui-peek' functions provided by the `lsp' layer (`lsp-ui' package)"
  (spacemacs//c-c++-lsp-call-function "spacemacs//c-c++-lsp-" "-customise-lsp-ui-peek")

  (defun c-c++/vars () (interactive) (lsp-ui-peek-find-custom 'vars (spacemacs//c-c++-lsp-string "$" "/vars")))

  (defun c-c++/references-address ()
    (interactive)
    (lsp-ui-peek-find-custom
      'address "textDocument/references"
      (plist-put (lsp--text-document-position-params) :context
        '(:role 128))))

  (defun c-c++/references-read ()
    (interactive)
    (lsp-ui-peek-find-custom
      'read "textDocument/references"
      (plist-put (lsp--text-document-position-params) :context
        '(:role 8))))

  (defun c-c++/references-write ()
    (interactive)
    (lsp-ui-peek-find-custom
      'write "textDocument/references"
      (plist-put (lsp--text-document-position-params) :context
        '(:role 16)))))

(defun spacemacs//c-c++-lsp-cquery-customise-lsp-ui-peek ()
  (defun c-c++/callers () (interactive) (lsp-ui-peek-find-custom 'callers "$cquery/callers"))
  (defun c-c++/base () (interactive) (lsp-ui-peek-find-custom 'base "$cquery/base")))

(defun spacemacs//c-c++-lsp-ccls-customise-lsp-ui-peek ()
  (defun c-c++/callers () (interactive) (lsp-ui-peek-find-custom 'callers "$ccls/call"))
  (defun c-c++/base ()
    (interactive)
    (lsp-ui-peek-find-custom 'base "$ccls/inheritance"
      (append (lsp--text-document-position-params) '(:flat t :level 3)))))
