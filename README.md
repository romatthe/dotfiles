# dotfiles
:neckbeard: A classic dotfile repo with small amount of my dotfiles that should/could be useful on multiple systems.

I tried to keep the setup as simple as humanly possible, so I decided to structure the repo so that it is easily usable with [GNU Stow][1]. It's pretty much available on any system out there, and it's simple e
nough while still not requiring any manual symlinking.

Using these files is as simple as:
```
$ git clone https://github.com/romatthe/dotfiles ~/.dotfiles
$ cd ~/.dotfiles
$ stow git
$ stow shell
... etc
```

Removing any of the symlinks you previously established is equally simple:
```
$ cd ~/.dotfiles
$ stow -D git
```

And finally, when you've added or deleted some files:
```
$ cd ~/.dotfiles
$ stow -R git
```

[1]: https://www.gnu.org/software/stow/
