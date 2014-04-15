# YouTrack mode for Emacs

## Getting started

1. Add `youtrack.el` to emacs load path or, `~/.emacs.d/youtrack/youtrack.el`
2. Add this code to init.el

```elisp
(add-to-list 'load-path "~/.emacs.d/youtrack")
(require 'youtrack)
(setq yt-user "you@ideadevice.com")
(setq yt-password "yourpassword")
(setq yt-baseurl "https://bug.idvc.es")
```

## Usage examples

1. Create youtrack bugs

    M-x yt-bug
