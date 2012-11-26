Emacs configuration based on https://github.com/cofi/dotfiles.git and https://github.com/doitian/emacs.d.git

Dependencies
------------

-   I'm using Emacs 24, may fail on 23.

Install
-------

1.  Initialize submodules

        git submodule init
        git submodule update

2.  To filter loaded files, create `custom.el`, and set `tahti-blacklist`, e.g.,
disable tex mode:

        (custom-set-variables
          '(tahti-blacklist (list 'tahti-tex-mode)))
3.  Secret or personal settings can be added in `secrets.el`, e.g.

        echo '(setq tumble-password "xx")' >> secrets.el
4.  Start emacs

5.  Restart If a package failed to install. Or eval:

        (el-get 'sync (reverse el-get-packages))

