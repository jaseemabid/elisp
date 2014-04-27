# YouTrack mode for Emacs

## Installation

Put this file on your Emacs-Lisp load path, add following into your $HOME/.emacs
startup file.

```elisp
     (add-to-list 'load-path "~/.emacs.d/youtrack")
     (require 'youtrack)
```

Youtrack authentication variables can be set by customizing the group `youtrack
or by setting them directly.

My configuration looks something like this:

```elisp
     (setq yt-baseurl "https://bug.idvc.es/"
           yt-password "yourpassword"
           yt-project "youtrack"
           yt-user "j")
```

## Configuration

M-x customize-group RET and select youtrack

## Usage examples

'youtrack-status is bound to 'C-c y' by default

## Documentation/Code overview

Youtrack mode `yt-mode' is a major mode inspired by magit to work with youtrack
bug tracker.

It's derived from special-mode[1][1], [2][2] and hence gets a bunch of usefull
features for free.
- Generated buffers are read only
- A bunch of helper key bindings like `q' to exit.

## Coding conventions

- No extra line for trailing `)`. Group them together
- `M-x checkdoc' before commits
- Sane commit messages please! Refer to
[A note about commit messages][commit-message] and
[Stop writing rambling commit messages][ramble].

<!-- Links -->

[1]: http://www.gnu.org/software/emacs/manual/html_node/elisp/Basic-Major-Modes.html "Basic Major Modes"
[2]: http://www.gnu.org/software/emacs/manual/html_node/elisp/Derived-Modes.html "Derived Modes"
[commit-message]: http://tbaggery.com/2008/04/19/a-note-about-git-commit-messages.html
[ramble]: http://stopwritingramblingcommitmessages.com/
