# incremental-play

Playing with incremental parsing and later compiler passes

First pass will be to build a haskell version of the approach from "Efficient
and Flexible Incremental Parsing",
https://pdfs.semanticscholar.org/4d22/fab95c78b3c23fa9dff88fb82976edc213c2.pdf

This is also implemented in https://github.com/tree-sitter/tree-sitter

## To experiment

### happy

Install the custom version of `happy` from https://github.com/alanz/happy, using
the `incremental` branch.

I rename the installed executable to `happy-az`, so that it does not
interfere with the system version.

### alex

Install the custom version of `alex` from https://github.com/alanz/alex, using
the `incremental` branch.

I rename the installed executable to `alex-az`, so that it does not
interfere with the system version.

### building

In this directory

    make

Load `app/Main.hs` into ghci and run it

## Emacs integration

First run `stack install`

```elisp
(package-install 'lsp-mode)
(package-install 'lsp-ui)

(require 'lsp-ui)
(require 'lsp-mode)

(add-to-list 'load-path "/home/alanz/mysrc/github/alanz/incremental-play/elisp")
(require 'lsp-inc)

(add-hook 'inc-mode-hook #'lsp-inc-enable)
(add-hook 'inc-mode-hook 'flycheck-mode)
```
