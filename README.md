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

## Xmonad

Requires:
* Haskell Stack
* iwlib C headers:
  * Debian: `sudo apt-get install libiw-dev`
  * Arch: `pacman -S wireless_tools`
* dzen2
* conky
* xdotool
* `git clone "https://github.com/xmonad/xmonad" ~/.xmonad/xmonad`
* `git clone "https://github.com/xmonad/xmonad-contrib" ~/.xmonad/xmonad-contrib`
* `git clone "https://github.com/jaor/xmobar" ~/.xmonad/xmobar`
* `git clone "https://github.com/troydm/xmonad-dbus" ~/.xmonad/xmonad-dbus` 
* `xmonad --recompile && stack install`
* `cp ~/.xmonad/xmonad.desktop /usr/share/xsessions`

Whenever you update your xmonad, xmonad-contrib, or xmobar repositories, just `cd
~/.xmonad` and run `stack install`

More info on running and building Xmonad with Stack, look [here][2] and [here][3].

## Dunst

Requires:
* Dunst installed on the system
* `libnotify` installed on the system

[1]: https://www.gnu.org/software/stow/
[2]: https://brianbuccola.com/how-to-install-xmonad-and-xmobar-via-stack/
[3]: http://sitr.us/2018/05/13/build-xmonad-with-stack.html
