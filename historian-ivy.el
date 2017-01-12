;;; historian-ivy.el --- Persistently store selected minibuffer candidates -*- lexical-binding: t -*-

;; Copyright (C) 2017 PythonNut

;; Author: PythonNut <pythonnut@pythonnut.com>
;; Keywords: convenience, ivy
;; Version: 20170111
;; URL: https://github.com/PythonNut/historian.el
;; Package-Requires: ((emacs "24.4") (historian "20170111"))

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; historian-ivy.el integrates historian with ivy.

;;; Code:

(require 'historian)

(defgroup historian-ivy nil
  "Persistently store selected minibuffer candidates"
  :group 'convenience
  :prefix "historian-ivy-")

(defcustom historian-ivy-auto-enable-historian-mode t
  "Whether to enable historian-mode when historian-ivy-mode is enabled."
  :type 'boolean
  :group 'historian-ivy)

(defcustom historian-ivy-freq-boost-factor 100
  "Relative weight of frequency boost.
(Requires some experimenting to get a feel for values)"
  :type 'number
  :group 'historian-ivy)

(defcustom historian-ivy-recent-boost 100
  "Relative weight of recency boost.
(Requires some experimenting to get a feel for values)"
  :type 'number
  :group 'historian-ivy)

(defcustom historian-ivy-recent-decrement 5
  "Decrease in score as item gets less recent.
(Requires some experimenting to get a feel for values)"
  :type 'number
  :group 'historian-ivy)

(defvar historian-ivy--saved-this-command nil)

(defun historian-ivy--nadvice/ivy-read (old-fun &rest args)
  (setq historian-ivy--saved-this-command this-command)
  (cl-letf* ((old-rfm (symbol-function #'read-from-minibuffer))
             ((symbol-function #'read-from-minibuffer)
              (lambda (&rest args)
                (historian-push-item this-command
                                     (apply old-rfm args)))))
    (apply old-fun args)))

(defun historian-ivy--nadvice/ivy--flx-sort (old-fun name cands)
  (if (not historian-mode)
      (funcall old-fun name cands)
    (cl-letf*
        ((old-flx-score (symbol-function #'flx-score))
         ((symbol-function #'flx-score)
          (lambda (str query &optional cache)
            (let* ((orig-score
                    (funcall old-flx-score str query cache))
                   (history (gethash historian-ivy--saved-this-command
                                     historian--history-table)))
              (if history
                  (let* ((freq (if (gethash str (cdr history))
                                   (/ (float (gethash str (cdr history) 0))
                                      (let ((total 0))
                                        (maphash
                                         (lambda (_key value)
                                           (cl-incf total value))
                                         (cdr history))
                                        total))
                                 0))
                         (freq-boost (* freq historian-ivy-freq-boost-factor))
                         (recent-index (cl-position str (car history)))
                         (recent-boost (if recent-index
                                           (- historian-ivy-recent-boost
                                              (* historian-ivy-recent-decrement
                                                 recent-index))
                                         0)))
                    (cons
                     (+ (car orig-score) freq-boost recent-boost)
                     (cdr orig-score)))
                orig-score)))))
      (funcall old-fun name cands))))

;;;###autoload
(define-minor-mode historian-ivy-mode
  "historian minor mode"
  :init-value nil
  :group 'historian
  :global t
  (require 'ivy)
  (if historian-ivy-mode
      (progn
        (when historian-ivy-auto-enable-historian-mode
          (historian-mode +1))
        (advice-add 'ivy-read :around
                    #'historian-ivy--nadvice/ivy-read)
        (advice-add 'ivy--flx-sort :around
                    #'historian-ivy--nadvice/ivy--flx-sort))

    (advice-remove 'ivy-read #'historian-ivy--nadvice/ivy-read)
    (advice-remove 'ivy--flx-sort
                   #'historian-ivy--nadvice/ivy--flx-sort)))

(provide 'historian-ivy)

;;; historian-ivy.el ends here
