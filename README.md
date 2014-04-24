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
