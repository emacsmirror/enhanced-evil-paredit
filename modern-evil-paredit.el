;;; modern-evil-paredit.el --- Paredit support for evil keybindings  -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 James Cherti | https://www.jamescherti.com/contact/
;; Copyright (C) 2012-2015 Roman Gonzalez

;; Mantainer: James Cherti
;; Original author: Roman Gonzalez <romanandreg@gmail.com>
;; Version: 0.0.2
;; URL: https://github.com/jamescherti/modern-evil-paredit.el
;; Keywords: convenience
;; Package-Requires: ((emacs "24.1") (evil "1.0.9") (paredit "25beta"))
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;; This package prevents parenthesis imbalance when using evil-mode with
;; paredit. It intercepts evil-mode modifier commands (such as delete, change,
;; and yank) and blocks their execution if they would break parenthetical
;; structure. This ensures your Lisp code maintains proper syntax while
;; preserving evil-mode's powerful editing capabilities.

;;; Code:

(require 'evil)
(require 'paredit)

(defgroup modern-evil-paredit nil
  "Evil Customization group for paredit-style structural editing."
  :group 'modern-evil-paredit
  :prefix "modern-evil-paredit-")

(defvar modern-evil-paredit-mode-map (make-sparse-keymap)
  "Keymap for `modern-evil-paredit-mode'.")

;;;###autoload
(define-minor-mode modern-evil-paredit-mode
  "Minor mode for setting up Evil with paredit in a single buffer."
  :lighter " EParedit"
  :group 'modern-evil-paredit
  :keymap modern-evil-paredit-mode-map)

(defun modern-evil-paredit--check-region (beg end)
  "Ensure region from BEG to END maintains parenthesis balance.
Signals an error if deleting the region would break structure."
  (when (and beg end)
    (if (fboundp 'paredit-check-region-state)
        (save-excursion
          (goto-char beg)
          (let* ((state (paredit-current-parse-state))
                 (state* (parse-partial-sexp beg end nil nil state)))
            (paredit-check-region-state state state*)))
      (paredit-check-region-for-delete beg end))))

(evil-define-operator modern-evil-paredit-yank (beg end type register yank-handler)
  "Yank text from BEG to END of TYPE into REGISTER with YANK-HANDLER."
  :move-point nil
  :repeat nil
  (interactive "<R><x><y>")
  (modern-evil-paredit--check-region beg end)
  (cond
   ((eq type 'block)
    (evil-yank-rectangle beg end register yank-handler))
   ((eq type 'line)
    (evil-yank-lines beg end register yank-handler))
   (t
    (evil-yank-characters beg end register yank-handler))))

(evil-define-operator modern-evil-paredit-yank-line (beg end type register)
  "Saves whole lines into the `kill-ring'."
  :motion evil-line
  :move-point nil
  (interactive "<R><x>")
  (let* ((beg (point))
         (end (modern-evil-paredit-kill-end)))
    (modern-evil-paredit-yank beg end type register)))

(evil-define-operator modern-evil-paredit-delete
  (beg end type register yank-handler)
  "Delete text from BEG to END with TYPE respecting parenthesis.
Save in REGISTER or in the `kill-ring' with YANK-HANDLER."
  (interactive "<R><x><y>")
  (modern-evil-paredit-yank beg end type register yank-handler)
  (if (eq type 'block)
      (evil-apply-on-block #'delete-region beg end nil)
    (delete-region beg end))
  ;; place cursor on beginning of line
  (when (and (called-interactively-p 'any)
             (eq type 'line))
    (evil-first-non-blank)))

(evil-define-operator modern-evil-paredit-delete-line
  (beg end type register yank-handler)
  "Delete to end of line respecting parenthesis."
  :motion nil
  :keep-visual t
  (interactive "<R><x>")
  (let* ((beg (point))
         (end (modern-evil-paredit-kill-end)))
    (modern-evil-paredit-delete beg end
                         type register yank-handler)))

(defun modern-evil-paredit-kill-end ()
  "Return the position where `paredit-kill' would kill to."
  (when (paredit-in-char-p)             ; Move past the \ and prefix.
    (backward-char 2))                  ; (# in Scheme/CL, ? in elisp)
  (let* ((eol (line-end-position))
         (end-of-list-p (save-excursion
                          (paredit-forward-sexps-to-kill (point) eol))))
    (if end-of-list-p (progn (up-list) (backward-char)))
    (cond ((paredit-in-string-p)
           (if (save-excursion (paredit-skip-whitespace t (line-end-position))
                               (eolp))
               (kill-line)
             (save-excursion
               ;; Be careful not to split an escape sequence.
               (if (paredit-in-string-escape-p)
                   (backward-char))
               (min (line-end-position)
                    (cdr (paredit-string-start+end-points))))))
          ((paredit-in-comment-p)
           eol)
          (t (if (and (not end-of-list-p)
                      (eq (line-end-position) eol))
                 eol
               (point))))))

(evil-define-operator modern-evil-paredit-change
  (beg end type register yank-handler delete-func)
  "Change text from BEG to END of TYPE using REGISTER and YANK-HANDLER.
Save in REGISTER or the `kill-ring' with YANK-HANDLER.
DELETE-FUNC is a function for deleting text, default `evil-delete'.
If TYPE is `line', insertion starts on an empty line.
If TYPE is `block', the inserted text in inserted at each line
of the block."
  (interactive "<R><x><y>")
  (let ((delete-func (or delete-func #'modern-evil-paredit-delete))
        (nlines (1+ (- (line-number-at-pos end)
                       (line-number-at-pos beg)))))
    (funcall delete-func beg end type register yank-handler)
    (cond
     ((eq type 'line)
      (evil-open-above 1))
     ((eq type 'block)
      (evil-insert 1 nlines))
     (t
      (evil-insert 1)))))

(evil-define-operator modern-evil-paredit-change-line
  (beg end type register yank-handler)
  "Yank line from BEG to END of TYPE into REGISTER."
  :motion evil-end-of-line
  (interactive "<R><x><y>")
  (let* ((beg (point))
         (end (modern-evil-paredit-kill-end)))
    (modern-evil-paredit-change beg end type register yank-handler)))

(defun modern-evil-paredit-change-whole-line ()
  "Change whole line."
  (interactive)
  (beginning-of-line)
  (modern-evil-paredit-change-line nil nil)
  (indent-according-to-mode))

(evil-define-operator modern-evil-paredit-backward-delete
  (beg end type register yank-handler)
  "Delete character forward.
Delete the character forward from BEG to END of TYPE into REGISTER with
YANK-HANDLER."
  :motion evil-backward-char
  :keep-visual t
  (interactive "<r><x><y>")
  (if (and beg end)
      (modern-evil-paredit-delete beg end type register yank-handler)
    (modern-evil-paredit-delete
     (1- (point)) (point) type register yank-handler)))

(evil-define-operator modern-evil-paredit-forward-delete
  (beg end type register yank-handler)
  "Delete character at point."
  :motion evil-forward-char
  :keep-visual t
  (interactive "<r><x><y>")
  (if (and beg end)
      (modern-evil-paredit-delete beg end type register yank-handler)
    (modern-evil-paredit-delete
     (point) (1+ (point)) type register yank-handler)))

(evil-define-key 'normal modern-evil-paredit-mode-map
  (kbd "d") #'modern-evil-paredit-delete
  (kbd "c") #'modern-evil-paredit-change
  (kbd "y") #'modern-evil-paredit-yank
  (kbd "D") #'modern-evil-paredit-delete-line
  (kbd "C") #'modern-evil-paredit-change-line
  (kbd "S") #'modern-evil-paredit-change-whole-line
  (kbd "Y") #'modern-evil-paredit-yank-line
  (kbd "X") #'modern-evil-paredit-backward-delete
  (kbd "x") #'modern-evil-paredit-forward-delete)

(provide 'modern-evil-paredit)

;;; modern-evil-paredit.el ends here
