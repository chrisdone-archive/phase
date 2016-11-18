# Running in Emacs

* Run `M-x phase-start`.
* In the buffer you want to publish, run `M-x phase-enable-buffer`.
* Open up http://127.0.0.1:8080/ in your browser.
* If you don't get anything displayed or it's garbled, use `M-x
  phase-refresh-buffer`.

Use `M-x phase-disable-buffer` once you're done.

# Sharing with ngrok

    $ ngrok http 8080
