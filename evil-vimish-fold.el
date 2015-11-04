;;; evil-vimish-fold.el --- Integrate vimish-fold with evil

;; Copyright (c) 2015 Alex Murray
;;
;; Author: Alex Murray <murray.alex@gmail.com>
;; Maintainer: Alex Murray <murray.alex@gmail.com>
;; URL: https://github.com/alexmurray/evil-vimish-fold
;; Version: 0.1
;; Package-Requires: ((emacs "24.4") (evil "1.0.0") (vimish-fold "0.2.0"))

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
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
;;
;; Integrate `vimish-fold' with `evil'.
;;
;; Provides bindings to create and delete folds via "zf" and "zd" respectively,
;; and provides integration of usual vim fold commands via `vimish-fold`.
;;

;;; Code:

(require 'evil)
(require 'vimish-fold)

;; define commands which may not be in upstream packages yet
(unless (fboundp 'vimish-fold-refold-all)
  (defun vimish-fold-refold-all ()
    "Refold all closed folds in current buffer."
    (interactive)
    (mapc #'vimish-fold--refold
          (vimish-fold--folds-in
           (point-min)
           (point-max)))))

(evil-define-operator evil-create-fold (beg end)
  "Create a fold from the current region.
See also `evil-delete-fold'."
  (when vimish-fold-mode
    (vimish-fold beg end)))

(evil-define-command evil-delete-fold ()
  "Delete a fold under point.
See also `evil-create-fold'."
  (evil-fold-action evil-fold-list :delete))

(define-key evil-motion-state-map "zd" 'evil-delete-fold)
(define-key evil-motion-state-map "zf" 'evil-create-fold)

(add-to-list 'evil-fold-list
             `((vimish-fold-mode)
               :delete     vimish-fold-delete
               :open-all   vimish-fold-unfold-all
               :close-all  vimish-fold-refold-all
               :toggle     vimish-fold-toggle
               :open       vimish-fold-unfold
               :open-rec   nil
               :close      vimish-fold-refold))

(provide 'evil-vimish-fold)

;;; evil-vimish-fold.el ends here
