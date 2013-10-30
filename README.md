Emacs 24.3 is required for this init.el. I'm using it on Mac OS X 10.9 with emacs 24.3.50 from [emacsformacosx.com](http://emacsformacosx.com/), available in the "Nightlies" section of [Builds](http://emacsformacosx.com/builds).  Fullscreen on OS X looks better starting at version 24.3.50.

## Installation
1. Back up ~/.emacs.d if there is anything you wish to keep.
2. `git clone git://github.com/egamble/.emacs.d.git` from your home directory.
3. Either `cp ~/.emacs.d/.emacs ~` or merge ~/.emacs.d/.emacs with your existing ~/.emacs
4. To open a file from a terminal shell in the currently running Emacs.app, put a link to /Applications/Emacs.app/Contents/MacOS/bin/emacsclient somewhere on your PATH. E.g. `ln -s /Applications/Emacs.app/Contents/MacOS/bin/emacsclient /usr/local/bin/emacsclient; PATH=/usr/local/bin:$PATH`
5. A nice shortcut for invoking emacsclient is `alias e='emacsclient -n'`.
