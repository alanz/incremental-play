;;; lsp-inc.el --- Incremental play support for lsp-mode

;;; Commentary:

;;; Code:

(require 'lsp)

(lsp-define-stdio-client lsp-inc
                         "inc"
                         #'(lambda () default-directory)
                         '("incremental-play" ))

(eval-after-load 'lsp '(lsp-register-client
    (make-lsp--client
     :new-connection (lsp-stdio-connection "incremental-play")
     :major-modes '(inc-mode text-mode)
     :server-id 'incremental-play
     ;; :multi-root t
     )))

(eval-after-load 'lsp
  '(add-to-list 'lsp-language-id-configuration '(text-mode . "inc") ))

(provide 'lsp-inc)
;;; lsp-inc.el ends here
