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

(defmacro bupper--with-variable (var value &rest body)
  "Run `BODY` with the symbol `VAR` temporary set to `VALUE`."
  `(let ((original-value ,var))
    (setq ,var ,value)
    (progn ,@body)
    (setq ,var original-value)))

(defun bupper--ensure-enough-space (required-space)
  "Ensure that at least `REQUIRED-SPACE` columns are available in the current line."
  (save-mark-and-excursion
    (end-of-line)
    (let* ((available-space (current-column))
           (missing-space (- required-space available-space)))
      (when (> missing-space 0)
        (insert (make-string missing-space ? ))))))

(defun bupper--backup-content ()
  "Backup content of the windows."
  (let ((content-map (make-hash-table :test 'equal)))
    (dolist (window (window-list))
      (with-current-buffer (window-buffer window)
        (puthash (buffer-name) (list (point) (buffer-substring (point-min) (point-max))) content-map)))
    content-map))

(defun bupper--restore-content (content-map)
  "Restore content of the windows from the `CONTENT-MAP` mapping."
  (dolist (window (window-list))
    (with-current-buffer (window-buffer window)
      (bupper--with-variable
       inhibit-read-only t
       (let* ((value (gethash (buffer-name) content-map))
              (pos (pop value))
              (content (pop value)))
         (save-mark-and-excursion
           (goto-char (point-min))
           (delete-region (point-min) (point-max))
           (insert content)
           (set-buffer-modified-p nil))
         (set-window-point window pos))))))

(defun bupper--add-string-overlay-to-window (window string)
  "Add the STRING `string` as overlay in `WINDOW` in the first visible position."
  (with-current-buffer (window-buffer window)
    (bupper--with-variable
     inhibit-read-only t
     (setq string (concat " " string " "))
     (save-mark-and-excursion
       (let ((pos (window-start window))
             (width (length string))
             ov)
         (let ((empty-row (make-string width ? )))
           (dotimes (i 3)
             (goto-char pos)
             (bupper--ensure-enough-space width)
             (let ((start pos)
                   (end (+ width (goto-char pos))))
               (setq ov (make-overlay start end))
               (overlay-put ov 'face 'bupper-face)
               (overlay-put ov 'display (if (= i 1) string empty-row))
               (setq pos (+ 1 (line-end-position)))))))))))

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
  (let ((content-map (bupper--backup-content)))
    (bupper--add-string-overlays)
    (unwind-protect
        (let ((target-window (bupper--prompt-target-window)))
          (let ((target-buffer (window-buffer target-window)))
            (set-window-buffer target-window (current-buffer))
            (set-window-buffer nil target-buffer)
            (select-window target-window)))
      (bupper--remove-string-overlays)
      (bupper--restore-content content-map))))

(provide 'bupper)

;;; bupper.el ends here
