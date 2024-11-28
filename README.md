# modern-evil-paredit.el - Prevent Parenthesis Imbalance when Using Evil-mode with Paredit
![Build Status](https://github.com/jamescherti/modern-evil-paredit.el/actions/workflows/ci.yml/badge.svg)
![License](https://img.shields.io/github/license/jamescherti/modern-evil-paredit.el)
![](https://raw.githubusercontent.com/jamescherti/modern-evil-paredit.el/main/.images/made-for-gnu-emacs.svg)

The **modern-evil-paredit** package prevents parenthesis imbalance when using *evil-mode* with *paredit*. It intercepts *evil-mode* modifier commands (such as delete, change, paste, and yank) and blocks their execution if they would break parenthetical structure. This ensures your Lisp code maintains proper syntax while preserving evil-mode's powerful editing capabilities.

## Installation

### Install with straight

To install `modern-evil-paredit` with `straight.el`:

1. It if hasn't already been done, [add the straight.el bootstrap code](https://github.com/radian-software/straight.el?tab=readme-ov-file#getting-started) to your init file.
2. Add the following code to the Emacs init file:
```emacs-lisp
(use-package modern-evil-paredit
  :ensure t
  :straight (modern-evil-paredit
             :type git
             :host github
             :repo "jamescherti/modern-evil-paredit.el")
  :hook
  (paredit-mode . modern-evil-paredit-mode))
```

## Frequently asked questions

### What are the differences between modern-evil-paredit and evil-paredit?

The `modern-evil-paredit` package is a modernized version of `evil-paredit`. It has been enhanced and fully functions in recent versions of Emacs (Emacs >= 28). The author decided to develop `modern-evil-paredit` because the `evil-paredit` package is no longer maintained and does not function in recent versions of Emacs and Evil.

Here are the enhancements in `modern-evil-paredit`:
* Handles paste using `p` and `P`, ensuring that the pasted text has balanced parentheses.
* Fix call to a non-existent function `(evil-called-interactively-p)`, which has been replaced by `(called-interactively-p 'any)`.
* Add new functions: `modern-evil-paredit-backward-delete` and `modern-evil-paredit-forward-delete`.
* `modern-evil-paredit-mode` only uses the paredit functions when paredit is enabled. Otherwise, `modern-evil-paredit-mode` uses Evil functions.
* Add lexical binding with `lexical-binding: t`.
* Suppress Emacs Lisp warnings and add Melpa tests.
* Refactor and improve `modern-evil-paredit`.
* Create a `modern-evil-paredit` customization group for user configuration.
* Remove Evil state change from `modern-evil-paredit-mode`.
* Improve error handling in `modern-evil-paredit-check-region`.
* Enhance docstrings.
* Remove keymap bindings that are reserved by Emacs.
* Add `&optional` after the `end` argument to make it similar to Evil functions.

## Author and License

The `modern-evil-paredit` Emacs package has been written by [James Cherti](https://www.jamescherti.com/), Roman Gonzalez, and is distributed under terms of the GNU General Public License version 3, or, at your choice, any later version.

Copyright (C) 2024 James Cherti
Copyright (C) 2012-2015 Roman Gonzalez

This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with this program.

## Links

- [modern-evil-paredit.el @GitHub](https://github.com/jamescherti/modern-evil-paredit.el)

Other Emacs packages by the same author:
- [minimal-emacs.d](https://github.com/jamescherti/minimal-emacs.d): This repository hosts a minimal Emacs configuration designed to serve as a foundation for your vanilla Emacs setup and provide a solid base for an enhanced Emacs experience.
- [compile-angel.el](https://github.com/jamescherti/compile-angel.el): **Speed up Emacs!** This package guarantees that all .el files are both byte-compiled and native-compiled, which significantly speeds up Emacs.
- [easysession.el](https://github.com/jamescherti/easysession.el): Easysession is lightweight Emacs session manager that can persist and restore file editing buffers, indirect buffers/clones, Dired buffers, the tab-bar, and the Emacs frames (with or without the Emacs frames size, width, and height).
- [vim-tab-bar.el](https://github.com/jamescherti/vim-tab-bar.el): Make the Emacs tab-bar Look Like Vim’s Tab Bar.
- [elispcomp](https://github.com/jamescherti/elispcomp): A command line tool that allows compiling Elisp code directly from the terminal or from a shell script. It facilitates the generation of optimized .elc (byte-compiled) and .eln (native-compiled) files.
- [tomorrow-night-deepblue-theme.el](https://github.com/jamescherti/tomorrow-night-deepblue-theme.el): The Tomorrow Night Deepblue Emacs theme is a beautiful deep blue variant of the Tomorrow Night theme, which is renowned for its elegant color palette that is pleasing to the eyes. It features a deep blue background color that creates a calming atmosphere. The theme is also a great choice for those who miss the blue themes that were trendy a few years ago.
- [Ultyas](https://github.com/jamescherti/ultyas/): A command-line tool designed to simplify the process of converting code snippets from UltiSnips to YASnippet format.
- [dir-config.el](https://github.com/jamescherti/dir-config.el): Automatically find and evaluate .dir-config.el Elisp files to configure directory-specific settings.
- [flymake-bashate.el](https://github.com/jamescherti/flymake-bashate.el): A package that provides a Flymake backend for the bashate Bash script style checker.
- [flymake-ansible-lint.el](https://github.com/jamescherti/flymake-ansible-lint.el): An Emacs package that offers a Flymake backend for `ansible-lint`.
- [inhibit-mouse.el](https://github.com/jamescherti/inhibit-mouse.el): A package that disables mouse input in Emacs, offering a simpler and faster alternative to the `disable-mouse` package.
- [quick-sdcv.el](https://github.com/jamescherti/quick-sdcv.el): This package enables Emacs to function as an offline dictionary by using the sdcv command-line tool directly within Emacs.
