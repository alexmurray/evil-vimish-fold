# Evil vimish-fold

[![License GPL 3](https://img.shields.io/badge/license-GPL_3-green.svg)](http://www.gnu.org/licenses/gpl-3.0.txt)
[![Build Status](https://travis-ci.org/alexmurray/evil-vimish-fold.svg?branch=master)](https://travis-ci.org/alexmurray/evil-vimish-fold)

Integration of [vimish-fold](https://github.com/mrkkrp/vimish-fold) with [evil](https://bitbucket.org/lyro/evil/wiki/Home)

Adds normal vim keybindings of `zf` and `zd` to create and delete folds (via
`vimish-fold`) respectively. Also adds normal vim keybindings for fold toggling,
opening, closing etc.

This provides a near-complete vim folding experience in evil.


## Installation

If you would like to install the package manually, download or clone it and
put on Emacs' `load-path`, then you can require it in your init file like
this:

```emacs-lisp
(require 'evil-vimish-fold)
```


## License

Copyright © 2015 Alex Murray

Distributed under GNU GPL, version 3.
