Emacs 24.3 is required for this init.el. I'm using it on Mac OS X 10.9 with emacs 24.3.50 from [emacsformacosx.com](http://emacsformacosx.com/), available in the [Nightlies](http://emacsformacosx.com/builds#Nightlies) section of [Builds](http://emacsformacosx.com/builds), rather than the current release, which is still at 24.3.1.  Fullscreen on OS X looks better starting at version 24.3.50.

NB: The very latest builds in "Nightlies" seem to throw an error while reading init.el. The build from [September 12, 2013](http://emacsformacosx.com/emacs-builds/Emacs-2013-09-12-114240-universal-10.6.8.dmg) works well for me.

## Installation
1. Back up ~/.emacs.d if there is anything you wish to keep.
2. `git clone git://github.com/egamble/.emacs.d.git` from your home directory.
3. Either `cp ~/.emacs.d/.emacs ~` or merge ~/.emacs.d/.emacs with your existing ~/.emacs.
4. To open a file in the currently running Emacs.app from a terminal shell, put a link to /Applications/Emacs.app/Contents/MacOS/bin/emacsclient somewhere on your PATH. E.g. `ln -s /Applications/Emacs.app/Contents/MacOS/bin/emacsclient /usr/local/bin/emacsclient`. You may need to update your PATH in ~/.bashrc or ~/.bash_profile with e.g. `PATH=/usr/local/bin:$PATH`.
5. Verify with `which emacsclient` that emacsclient comes from /usr/local/bin rather than /usr/bin.
6. A nice shortcut for invoking emacsclient is `alias e='emacsclient -n'`, which you could put in your ~/.bashrc or ~/.bash_profile.
7. To use cider, first install [leiningen](https://github.com/technomancy/leiningen). Then add [cider-nrepl](https://github.com/clojure-emacs/cider-nrepl) to the `:repl` profile in `~/.lein/profiles.clj`. You may also need to add `tools.nrepl` as a dependency, e.g., `{:repl {:plugins [[cider/cider-nrepl "0.11.0-SNAPSHOT"]] :dependencies [[org.clojure/tools.nrepl "0.2.12"]]}}`.
9. Whenever you update your ~/.emacs.d from this repo with `git pull`, remove `~/.emacs.d/elpa` prior to restarting emacs. This ensures that the latest versions of the emacs packages are installed.
