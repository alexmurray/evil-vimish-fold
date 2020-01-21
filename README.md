# Evil vimish-fold

[![License GPL 3](https://img.shields.io/badge/license-GPL_3-green.svg)](http://www.gnu.org/licenses/gpl-3.0.txt)
[![MELPA](http://melpa.org/packages/evil-vimish-fold-badge.svg)](http://melpa.org/#/evil-vimish-fold)
[![Build Status](https://travis-ci.org/alexmurray/evil-vimish-fold.svg?branch=master)](https://travis-ci.org/alexmurray/evil-vimish-fold)

Integration of [vimish-fold](https://github.com/mrkkrp/vimish-fold) with [evil](https://bitbucket.org/lyro/evil/wiki/Home)

Adds standard vim keybindings of <kbd>zf</kbd> and <kbd>zd</kbd> to create and delete folds (via
`vimish-fold`) respectively. Also hooks into `evil` so the usual vim
keybindings for fold toggling (<kbd>za</kbd>), opening (<kbd>zo</kbd>), closing (<kbd>zc</kbd>) etc all work as expected with `vimish-fold`.

Finally, also supports navigation between folds using <kbd>zj</kbd> and <kbd>zk</kbd>.

This provides a near-complete vim folding experience in evil for Emacs.


## Installation

### MELPA

The preferred way to install `evil-vimish-fold` is via
[MELPA](http://melpa.org). You can just <kbd>M-x package-install RET
evil-vimish-fold RET</kbd>.


### Manual

If you would like to install the package manually, download or clone it and
place within Emacs' `load-path`, then you can require it in your init file like
this:

```emacs-lisp
(require 'evil-vimish-fold)
```

NOTE: This will also require the manual installation of `evil` and `vimish-fold`
if you have not done so already.

## Configuration

You can configure `evil-vimish-fold` to run on a per mode basis using hooks
or as a global mode activated on specific modes.

By default, `global-evil-vimish-fold-mode` will enable
`evil-vimish-fold-mode` in modes derived from those specified in
`evil-vimish-fold-target-modes`. By default `evil-vimish-fold-target-modes`
is set to `prog-mode` (and thus all modes derived from `prog-mode`). This
will allow you to avoid having `evil-vimish-mode` enabled in modes where its
key bindings conflict, e.g., magit.

### Vanilla configuration examples

Per mode (no use of global mode):

```emacs-lisp
(add-hook 'prog-mode-hook 'evil-vimish-fold-mode)
(add-hook 'text-mode-hook 'evil-vimish-fold-mode)
```

Globally for a set of modes:

```emacs-lisp
(setq evil-vimish-fold-target-modes '(prog-mode conf-mode text-mode))
(global-evil-vimish-fold-mode 1)
```

### use-package configuration examples

A configuration using mode hooks (no use of global mode):

```emacs-lisp
(use-package vimish-fold
  :ensure
  :after evil)

(use-package evil-vimish-fold
  :ensure
  :after vimish-fold
  :hook ((prog-mode conf-mode text-mode) . evil-vimish-fold-mode))
```

or

A configuration that sets the lighter, i.e., visual indicator of the mode's activation in the modeline, sets target modes, then turns on global mode:

```emacs-lisp
(use-package vimish-fold
  :ensure
  :after evil)

(use-package evil-vimish-fold
  :ensure
  :after vimish-fold
  :init
  (setq evil-vimish-fold-mode-lighter " ⮒")
  (setq evil-vimish-fold-target-modes '(prog-mode conf-mode text-mode))
  :config
  (global-evil-vimish-fold-mode))
```


## License

Copyright © 2015 Alex Murray

Distributed under GNU GPL, version 3.
