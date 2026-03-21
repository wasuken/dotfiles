;;; ai.el --- AI integrations -*- lexical-binding: t; -*-

;;; Commentary:
;; GPTel, Ollama, Ellama等のAI統合

;;; Code:

;; GPTel - Gemini統合
(use-package gptel
  :ensure t
  :config
  (setq gptel-api-key gemini-api-key)

  (setq-default gptel-backend (gptel-make-gemini "Gemini"
                                :key gptel-api-key
                                :stream t))

  (setq gptel-model 'gemini-2.5-flash)

  (global-set-key (kbd "C-c g") 'gptel-menu)
  (global-set-key (kbd "C-c <return>") 'gptel-send))

;; Ellama - Ollama統合
(use-package ellama
  :ensure t
  :config
  (require 'llm-ollama)
  (setq ellama-provider
        (make-llm-ollama
         :host ollama-host
         :port ollama-port
         :chat-model ollama-model
         :embedding-model ollama-model)))

;; Ollama - ローカルLLM
(use-package ollama
  :straight (:host github :repo "niklasbuehler/ollama.el")
  :config
  (setq ollama:endpoint (format "http://%s:%d/api/generate" ollama-host ollama-port))
  (setq ollama:model ollama-model)

  :bind
  ("C-c o l" . ollama/instruct-show-stream)
  ("C-c o p" . ollama/instruct-stream)
  ("C-c o r" . ollama/replace-current-line-stream)
  ("C-c o R" . ollama/replace-current-selection-stream)
  ("C-c o s" . ollama/select-model))

(provide 'ai)
;;; ai.el ends here
