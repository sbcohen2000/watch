;;; watch.el --- watch a file for changes and automatically reload the
;;;              buffer when changes occur.

;; Copyright (C) 2022  Samuel Burns Cohen

;; Author: Samuel Burns Cohen <sbcohen2000@gmail.com>
;; Keywords: file watching notification reload
;; Version: 0.0.1

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package exposes two interactive functions: watch-file and
;; unwatch-file.  watch-file places a file watch on the file visited
;; by the current buffer and reloads the buffer every time a change
;; occurs.  unwatch-file removes the file watch placed by watch-file,
;; returning the buffer to normal.

;;; Code:

(require 'filenotify)

;; watch-watches is an association list where the key
;; is the descriptor returned by file-notify-add-watch
;; and the value is the name of the watched buffer.
(defvar watch-watches '()
  "The list of every file-buffer pair being watched.")

(defun buffer-name-of-descriptor (descriptor)
  "Find the buffer name associated with the entry in watch-watches with DESCRIPTOR."
  (cdr (assoc descriptor watch-watches)))

(defun descriptor-of-buffer-name (name)
  "Find the descriptor associated with the entry in watch-watches with value NAME."
  (car (rassoc name watch-watches)))

(defun is-watching-name (name assoc-list)
  "Find if NAME is in the values of ASSOC-LIST."
  (if (null assoc-list) nil
    (or (equal name (cdr (car assoc-list)))
	(is-watching-name name (cdr assoc-list)))))

(defun on-file-changed (event)
  "Handle change EVENT to watched file."
  (let ((buffer-name (buffer-name-of-descriptor (car event))))
    (with-current-buffer buffer-name
      (revert-buffer-quick))))

;;;###autoload
(defun watch-file ()
  "Start watching the visited by the current buffer."
  (interactive)
  (if (is-watching-name (buffer-name) watch-watches)
      (message "Already watching %S." (buffer-file-name))
    (setq watch-watches (cons
			 (cons (file-notify-add-watch
				(buffer-file-name) '(change) 'on-file-changed)
			       (buffer-name)) watch-watches))))

;;;###autoload
(defun unwatch-file ()
  "Unwatch the file visited by the current buffer."
  (interactive)
  (if (is-watching-name (buffer-name) watch-watches)
      (let ((descriptor (descriptor-of-buffer-name (buffer-name))))
	(progn
	  (file-notify-rm-watch descriptor)
	  (setq watch-watches (assoc-delete-all descriptor watch-watches))))
    (message "%S isn't being watched." (buffer-file-name))))

(provide 'watch)

;;; watch.el ends here
