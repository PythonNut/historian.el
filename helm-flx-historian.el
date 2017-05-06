;;; helm-flx-historian.el --- Persistently store selected minibuffer candidates -*- lexical-binding: t -*-

;; Copyright (C) 2017 PythonNut

;; Author: PythonNut <pythonnut@pythonnut.com>
;; Keywords: convenience, helm
;; Version: 20170111
;; URL: https://github.com/PythonNut/historian.el
;; Package-Requires: ((emacs "24.4") (historian "20170111") (helm "0.8.0") (flx "0.6.1"))

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

;; helm-flx-historian.el integrates historian with helm-flx.

;;; Code:

(require 'historian)

(defgroup helm-flx-historian nil
  "Persistently store selected minibuffer candidates"
  :group 'convenience
  :prefix "helm-flx-historian-")

(defcustom helm-flx-historian-auto-enable-historian-mode t
  "Whether to enable historian-mode when helm-flx-historian-mode is enabled."
  :type 'boolean
  :group 'helm-flx-historian)

(defcustom helm-flx-historian-freq-boost-factor 100
  "Relative weight of frequency boost.
(Requires some experimenting to get a feel for values)"
  :type 'number
  :group 'helm-flx-historian)

(defcustom helm-flx-historian-recent-boost 100
  "Relative weight of recency boost.
(Requires some experimenting to get a feel for values)"
  :type 'number
  :group 'helm-flx-historian)

(defcustom helm-flx-historian-recent-decrement 5
  "Decrease in score as item gets less recent.
(Requires some experimenting to get a feel for values)"
  :type 'number
  :group 'helm-flx-historian)

(defvar helm-flx-historian--saved-this-command nil)

(defun helm-flx-historian--nadvice/helm-internal (old-fun &rest args)
  (setq helm-flx-historian--saved-this-command this-command)
  (apply old-fun args))

(defun helm-flx-historian--nadvice/helm-comp-read (old-fun &rest args)
  (let ((historian-this-command this-command)
        (return (apply old-fun args)))
    (historian-push-item historian-this-command return)))

(defun helm-flx-historian--nadvice/helm-execute-selection-action-1 (old-fun &rest args)
  (cl-letf* ((item-pushed nil)
             (old-helm-get-selection (symbol-function #'helm-get-selection))
             ((symbol-function #'helm-get-selection)
              (lambda (&rest inner-args)
                (let ((return (apply old-helm-get-selection inner-args)))
                  (unless item-pushed
                    (historian-push-item helm-flx-historian--saved-this-command
                                         (format "%s" return))
                    (setq item-pushed t))
                  return))))
    (apply old-fun args)))

(defun helm-flx-historian--nadvice/helm-flx-sort (old-fun &rest args)
  (if (not historian-mode)
      (apply old-fun args)
    (cl-letf*
        ((old-flx-score (symbol-function 'flx-score))
         ((symbol-function 'flx-score)
          (lambda (str query &optional cache)
            (let* ((orig-score
                    (funcall old-flx-score str query cache))
                   (history (gethash helm-flx-historian--saved-this-command
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
                         (freq-boost (* freq helm-flx-historian-freq-boost-factor))
                         (recent-index (cl-position str (car history) :test #'string=))
                         (recent-boost (if recent-index
                                           (- helm-flx-historian-recent-boost
                                              (* helm-flx-historian-recent-decrement
                                                 recent-index))
                                         0)))
                    (cons
                     (+ (or (car orig-score) most-negative-fixnum)
                        freq-boost
                        recent-boost)
                     (cdr orig-score)))
                orig-score)))))
      (apply old-fun args))))

;;;###autoload
(define-minor-mode helm-flx-historian-mode
  "historian minor mode"
  :init-value nil
  :group 'helm-flx-historian
  :global t
  (require 'helm-flx)
  (if helm-flx-historian-mode
      (progn
        (when helm-flx-historian-auto-enable-historian-mode
          (historian-mode +1))
        (advice-add 'helm-comp-read :around
                    #'helm-flx-historian--nadvice/helm-comp-read)
        (advice-add 'helm-internal :around
                    #'helm-flx-historian--nadvice/helm-internal)
        (advice-add 'helm-execute-selection-action-1 :around
                    #'helm-flx-historian--nadvice/helm-execute-selection-action-1)
        (advice-add 'helm-flx-sort :around
                    #'helm-flx-historian--nadvice/helm-flx-sort))

    (advice-remove 'helm-comp-read #'helm-flx-historian--nadvice/helm-comp-read)
    (advice-remove 'helm-internal
                   #'helm-flx-historian--nadvice/helm-internal)
    (advice-remove 'helm-execute-selection-action-1
                   #'helm-flx-historian--nadvice/helm-execute-selection-action-1)
    (advice-remove 'helm-flx-sort
                   #'helm-flx-historian--nadvice/helm-flx-sort)))

(provide 'helm-flx-historian)

;;; helm-flx-historian.el ends here
