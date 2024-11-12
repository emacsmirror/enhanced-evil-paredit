# modern-evil-paredit.el
![Build Status](https://github.com/jamescherti/modern-evil-paredit.el/actions/workflows/ci.yml/badge.svg)
![License](https://img.shields.io/github/license/jamescherti/modern-evil-paredit.el)
![](https://raw.githubusercontent.com/jamescherti/modern-evil-paredit.el/main/.images/made-for-gnu-emacs.svg)

This package prevents parenthesis imbalance when using evil-mode with paredit. It intercepts evil-mode modifier commands (such as delete, change, and yank) and blocks their execution if they would break parenthetical structure. This ensures your Lisp code maintains proper syntax while preserving evil-mode's powerful editing capabilities.

## Installation

### Install using straight

To install `modern-evil-paredit` using `straight.el`:

1. It if hasn't already been done, [add the straight.el bootstrap code](https://github.com/radian-software/straight.el?tab=readme-ov-file#getting-started) to your init file.
2. Add the following code to the Emacs init file:
```emacs-lisp
(use-package modern-evil-paredit
  :ensure t
  :straight (modern-evil-paredit
             :type git
             :host github
             :repo "jamescherti/modern-evil-paredit.el"))
```

## Author and License

The `modern-evil-paredit` Emacs package has been written by [James Cherti](https://www.jamescherti.com/) and is distributed under terms of the GNU General Public License version 3, or, at your choice, any later version.

Copyright (C) 2024 James Cherti

This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with this program.

## Links

- [modern-evil-paredit.el @GitHub](https://github.com/jamescherti/modern-evil-paredit.el)
