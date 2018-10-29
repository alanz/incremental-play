;;; lsp-inc.el --- Incremental play support for lsp-mode

;;; Commentary:

;;; Code:

(require 'lsp-mode)

(lsp-define-stdio-client lsp-inc
                         "inc"
                         #'(lambda () default-directory)
                         '("incremental-play" ))

(provide 'lsp-inc)
;;; lsp-inc.el ends here
