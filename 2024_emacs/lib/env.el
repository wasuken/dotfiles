(defconst IS-MAC (eq system-type 'darwin))
(defconst IS-LINUX (memq system-type '(gnu gnu/linux gnu/kfreebsd berkeley-unix)))
(defconst IS-WINDOWS (memq system-type '(cygwin windows-nt ms-dos)))

(when IS-WINDOWS
  ;; shift-jisよりcp932を優先させる
  (set-coding-system-priority 'utf-8
                              'euc-jp
                              'iso-2022-jp
                              'cp932)
  (setq-default default-process-coding-system '(utf-8-unix . japanese-cp932-dos)))

(setq native-comp-async-report-warnings-errors 'silent)
(setq native-compile-prune-cache t)

(setq gc-cons-percentage 0.2)
(add-hook 'focus-out-hook #'garbage-collect)
(setq garbage-collection-messages t)
(setq-default show-trailing-whitespace t)
(setq process-adaptive-read-buffering t)

(setq highlight-nonselected-windows nil)
(setq fast-but-imprecise-scrolling t)
(setq ffap-machine-p-known 'reject)
(setq idle-update-delay 1.0)
(setq redisplay-skip-fontification-on-input t)

(when IS-WINDOWS
  (setq w32-use-native-image-API t))

(unless IS-MAC
  (setq command-line-ns-option-alist nil))

(unless IS-LINUX
  (setq command-line-x-option-alist nil))

;; (let ((font-size (getenv "EMACS_FONT")))
;;   (cond
;;    (font-size
;;     (set-frame-font font-size))
;;    (t (set-frame-font "Noto Sans Mono-8"))))

