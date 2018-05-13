# Setup

1. Put emacs-goodies in any path

``` console
$ git clone https://code.google.com/p/emacs-goodies/
```

2. Setup instructions

``` console
$ ln -s <path>/emacs-goodies/dot-emacs $HOME/.emacs
```

3. Zenburn theme

   - This package comes with a customized version of Emacs zenburn theme.

     - Tweaked to use a darker background color (#202020) and support outline
       mode.

     - Upstream version is at http://github.com/bbatsov/zenburn-emacs

   - Opt to use https://github.com/ctchuang/zenburn-terminal to setup Zenburn color in terminal 

4. Emacs Key Bindings in Mac OS X

   Install Karabiner-Elements, and enable “Emacs key bindings [control+keys]”
   and “Emacs key bindings [option+keys]” rules.

5. Emacs keybindings for tig

```
$ ln -s <path>/emacs-goodies/dot-tigrc $HOME/.tigrc
```

# Others config examples

- Sample `.bashrc`

   Check `samples/doc-bashrc-sample` for some tricks such as fixing Ctrl+S in Emacs
   console mode.

- Sample `~/.emacs.d/local-setting.el`  (machine-local settings)

   We can put machine-local settings in this file separately while keeping
   common `.emacs` shared between machines.

   Check `samples/local-setting.el-sample`

# Misc tools

- Prettify Emacs outline mode to contain numbered labels:

```console
$ tools/prettify-outline.py < outline.txt
```
