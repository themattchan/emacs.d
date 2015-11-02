.emacs.d
========

## Installing

The preferred way is to grab it along with all my dotfiles ---

```
git clone --recursive https://github.com/themattchan/dotfiles.git ~/dotfiles
```

For just the Emacs configs, try

```
git clone --recursive https://github.com/themattchan/emacs.d.git ~/.emacs.d
```

Don't forget to compile all the files!

```
M-x matt/recompile-settings
```

## Updating

```
git pull
git submodule foreach git pull origin master
```

Then as always,

```
M-x matt/recompile-settings
```

## Related

My configs are mostly a hodgepodge of good things taken from others, look here
for more inspiration

- https://github.com/bryangarza/dot-emacs

## License

Copyright (C) 2013-2015 Matthew Chan

This program is free software: you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free Software
Foundation, either version 3 of the License, or (at your option) any later
version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
PARTICULAR PURPOSE.  See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with
this program.  If not, see <http://www.gnu.org/licenses/>.

[See here for the whole thing](https://github.com/themattchan/emacs.d/blob/master/LICENSE).
