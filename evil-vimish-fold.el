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
(eval-when-compile
  '(require 'cl-lib))

(evil-define-operator evil-vimish-fold/create (beg end)
  "Create a fold from the current region.
See also `evil-delete-fold'."
  (when vimish-fold-mode
    (vimish-fold beg end)))

(evil-define-operator evil-vimish-fold/create-line (beg end)
  "Create a fold from the current region.
See also `evil-delete-fold'."
  :motion evil-line
  (interactive "<r>")
  (when vimish-fold-mode
    (vimish-fold beg end)))

(evil-define-command evil-vimish-fold/delete ()
  "Delete a fold under point.
See also `evil-create-fold'."
  (evil-fold-action evil-fold-list :delete))

;;;###autoload
(define-minor-mode evil-vimish-fold-mode
  "Evil-vimish-fold-mode."
  :global t
  :lighter " zf"
  :keymap (let ((map (make-sparse-keymap)))
            (evil-define-key 'motion map "zd" 'evil-vimish-fold/delete)
            (evil-define-key 'motion map "zf" 'evil-vimish-fold/create)
            (evil-define-key 'motion map "zF" 'evil-vimish-fold/create-line)
            map)
  (vimish-fold-global-mode (if evil-vimish-fold-mode 1 -1))
  (if evil-vimish-fold-mode
      (add-to-list 'evil-fold-list
                   `((vimish-fold-mode)
                     :delete     vimish-fold-delete
                     :open-all   vimish-fold-unfold-all
                     :close-all  vimish-fold-refold-all
                     :toggle     vimish-fold-toggle
                     :open       vimish-fold-unfold
                     :open-rec   nil
                     :close      vimish-fold-refold))
    (setq evil-fold-list (cl-remove-if
                          #'(lambda (e) (eq (caar e) 'vimish-fold-mode))
                          evil-fold-list))))

(provide 'evil-vimish-fold)

;;; evil-vimish-fold.el ends here
