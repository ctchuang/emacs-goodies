# Setup

```
$ mkdir -p ~/src/misc/ && cd ~/src/misc/
$ git clone git@github.com:ctchuang/emacs-goodies.git
$ ln -s ~/src/misc/emacs-goodies/dot-emacs ~/.emacs
```

# Features

- ido everywhere
  - Open file (`C-x C-f`) and buffer (`C-x b`)
  - Recentf (`C-c r`) - recently opened files
  - Scan source files in current directory (`C-c s`) and quick switch with ido (`C-c f`)
  - Better imenu with ido (`C-c i`)
- Enable outline minor mode (prefix with `C-c C-c`)
- Enable auto completion by default
- Better grep (`C-c g`)
- Better occur (`C-c o`)
- Set Tmux window name to current buffer name
- Use `S-+` and `S--` to increase/decrease font size on X11 and Mac window mode
- Support Mac style keys (Command key combinations) on X11
- Load machine-local settings from `~/.emacs.d/lisp/local-setting.el`

# TODO

- Learn from [Purcell's .emacs](https://github.com/purcell/emacs.d)
- [lsp-treemacs](https://github.com/emacs-lsp/lsp-treemacs)
