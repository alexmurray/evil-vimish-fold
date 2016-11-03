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
[MELPA](http://melpa.org) - then you can just <kbd>M-x package-install
RET evil-vimish-fold RET</kbd> then temporarily enable it with
<kbd>M-x evil-vimish-fold-mode</kbd> or permanently by putting this in
your `init.el`:

```emacs-lisp
(evil-vimish-fold-mode 1)
```
### Manual

If you would like to install the package manually, download or clone it and
place within Emacs' `load-path`, then you can require it in your init file like
this:

```emacs-lisp
(require 'evil-vimish-fold)
(evil-vimish-fold-mode 1)
```

NOTE: This will also require the manual installation of `evil` and `vimish-fold`
if you have not done so already.

## License

Copyright © 2015 Alex Murray

Distributed under GNU GPL, version 3.
