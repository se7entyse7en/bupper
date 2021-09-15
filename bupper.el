;;; bupper.el --- Bu(ffer Swa)pper                       -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Lou Marvin Caraig

;; Author: Lou Marvin Caraig <loumarvincaraig@gmail.com>
;; URL: https://github.com/se7entyse7en/bupper
;; Package-Requires: ((emacs "24"))
;; Version: 0.1.2
;; Keywords: convenience, frames

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package allows to swap the buffer of the current window with the
;; one of another window.

;;; Code:

(defface bupper-face
;; '((:height 160)
;;   (:weight ultra-bold)
;;   (:foreground "red")
;;   (:background "white")
;;   (:box "red")))
;; See also documentation for `defface' "type" for specifying `window-system'.
  '((((class grayscale)
      (background light)) (:background "DimGray"))
    (((class grayscale)
      (background dark))  (:background "LightGray"))
    (((class color)
      (background light)) (:foreground "White" :background "DarkOrange1"))
    (((class color)
      (background dark))  (:foreground "Black" :background "DarkOrange1")))
  "Face used to highlight bupper window ID numbers."
  :group 'cursor-flash)

(defun bupper--add-string-overlay-to-window (window string)
  "Add the STRING `string` as overlay in `WINDOW` in the first visible position."
  (with-current-buffer (window-buffer window)
    (setq string (concat " " string " "))
    (save-mark-and-excursion
      (let ((pos (window-start window))
            ov)
        (dotimes (x 3)
          (goto-char pos)
          (when (> 2 (progn (end-of-line) (current-column)))
            (insert "   "))
          (setq ov (make-overlay pos (+ 3 (goto-char pos))))
          (overlay-put ov 'face 'bupper-face)
          (overlay-put ov 'display (if (= x 1) string "   "))
          (setq pos (1+ (line-end-position))))))))

(defun bupper--remove-overlays-from-window (window)
  "Remove all overlays from `WINDOW."
  (with-current-buffer (window-buffer window)
    (remove-overlays)))

(defun bupper--add-string-overlays ()
  "Add string overlays to all windows."
  (let ((counter 0))
    (dolist (window (window-list))
      (setq counter (+ counter 1))
      (bupper--add-string-overlay-to-window window (number-to-string counter)))))

(defun bupper--remove-string-overlays ()
  "Remove string overlays from all windows."
  (dolist (window (window-list))
    (bupper--remove-overlays-from-window window)))

(defun bupper--prompt-target-window ()
  "Prompt user to enter a number corresponding to the target window."
  (let ((n -1)
        (total-windows (length (window-list))))
    (while (or (< n 0)
                (> n total-windows))
      (setq n (read-number "Target window number: ")))
    (nth (- n 1) (window-list))))

;;;###autoload
(defun bupper-swap ()
  "Prompt user to swap the buffer with the one of another window."
  (interactive)
  (bupper--add-string-overlays)
  (unwind-protect
   (let ((target-window (bupper--prompt-target-window)))
     (let ((target-buffer (window-buffer target-window)))
       (set-window-buffer target-window (current-buffer))
       (set-window-buffer nil target-buffer)
       (select-window target-window)))
   (bupper--remove-string-overlays)))

(provide 'bupper)

;;; bupper.el ends here
