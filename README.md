At least Emacs 27 is required for this init.el. I'm currently using it on macOS 14 with emacs 29.3 from [emacsformacosx.com](http://emacsformacosx.com/).

## Installation
1. Back up `~/.emacs` and `~/.emacs.d` if there is anything you wish to keep, then remove both.
2. `git clone git://github.com/egamble/.emacs.d.git` from your home directory.
3. To open a file in the currently running Emacs.app from a terminal shell, put a link to /Applications/Emacs.app/Contents/MacOS/bin/emacsclient somewhere on your PATH. E.g. `ln -s /Applications/Emacs.app/Contents/MacOS/bin/emacsclient /usr/local/bin/emacsclient`. You may need to update your PATH in ~/.zshrc with e.g. `PATH=/usr/local/bin:$PATH`.
4. Verify with `which emacsclient` that emacsclient comes from /usr/local/bin rather than /usr/bin.
5. A nice shortcut for invoking emacsclient is `alias e='emacsclient -n'`, which you could put in your ~/.zshrc.
6. To use cider, first install [leiningen](https://github.com/technomancy/leiningen). Then add [cider-nrepl](https://github.com/clojure-emacs/cider-nrepl) to the `:repl` profile in `~/.lein/profiles.clj`. You may also need to add `tools.nrepl` as a dependency, e.g., `{:repl {:plugins [[cider/cider-nrepl "0.11.0-SNAPSHOT"]] :dependencies [[org.clojure/tools.nrepl "0.2.12"]]}}`.
7. Whenever you update your ~/.emacs.d from this repo with `git pull`, remove `~/.emacs.d/elpa` prior to restarting emacs. This ensures that the latest versions of the emacs packages are installed.
