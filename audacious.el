;;; audacious.el --- Emacs interface to control audacious

;; Last Modified: Jul 24th, 2021
;; Version      : 1.0
;; Author       : Hitoshi Uchida <hitoshi . uchida @ gmail . com>

;;; Commentary:
;;
;; audacious.el is an Emacs package to control the playback functions of audacious.
;; It depends on a command audtool installed with audacious.

;;; Code:

(defcustom audacious-command (executable-find "audtool")
  "CL interface of audacious."
  :type 'string
  :group 'audacious)

(defun audacious-run ()
  "Launch audacious with headless mode as daemon."
  (interactive)
  (call-process "audacious" nil 0 nil "-H" "2>/dev/null"))

(defun audacious-kill ()
  "Shutdown audacious process."
  (interactive)
  (when (yes-or-no-p "Quit Audacious ?")
    (call-process audacious-command nil 0 nil "--shutdown")))

(defun audacious-volume (vol)
  "Manually increase / decrease the volume by the specified VOL percent."
  (interactive "M[+|-]percent: ")
  (call-process audacious-command nil nil nil "--set-volume" vol))

(defun audacious-volume-up ()
  "Increase the volume by 10%."
  (interactive)
  (call-process audacious-command nil nil nil "--set-volume" "+10%")
  (message "%s" (delete-newline (shell-command-to-string "audtool --get-volume"))))

(defun audacious-volume-down ()
  "Decrease the volume by 10%."
  (interactive)
  (call-process audacious-command nil nil nil "--set-volume" "-10%")
  (message "%s" (delete-newline (shell-command-to-string "audtool --get-volume"))))

(defun audacious-play ()
  "Start to play."
  (interactive)
  (if (string-equal (shell-command-to-string "audtool --playback-status") "")
      (audacious-run))
  (call-process audacious-command nil nil nil "--playback-play")
  (sleep-for 0 20)
  (audacious-song-show-current-info))

(defun audacious-pause ()
  "Pause the playback."
  (interactive)
  (call-process audacious-command nil nil nil "--playback-pause")
  (audacious-status))

(defun audacious-stop ()
  "Stop the playback."
  (interactive)
  (call-process audacious-command nil nil nil "--playback-stop"))

(defun audacious-status ()
  "Show the current status of audacious."
  (interactive)
  (message "%s" (delete-newline (shell-command-to-string "audtool --playback-status"))))

(defun audacious-song-next ()
  "Play the next song in the current playlist."
  (interactive)
  (call-process audacious-command nil nil nil "--playlist-advance")
  (sleep-for 0 20)
  (audacious-song-show-current-info))

(defun audacious-song-prev ()
  "Play the previous song in the current playlist."
  (interactive)
  (call-process audacious-command nil nil nil "--playlist-reverse")
  (sleep-for 0 20)
  (audacious-song-show-current-info))

(defun audacious-song-goto ()
  "Select a song with an inputted number."
  (interactive)
  (setq res "")
  (dolist (line (split-string (shell-command-to-string "audtool --playlist-display") "\n"))
    (if (string-match-p (regexp-quote "|") line)
        (setq res (concat res (concat line "\n")))))
  (setq num (read-string (concat res "Song No.: ")))
  (if (string-integer-p num)
    (progn
      (call-process audacious-command nil nil nil "--playlist-jump" num)
      (sleep-for 0 20)
      (audacious-song-show-current-info))
    (message "\"%s\" is not number." num)))

(defun helm-audacious-song-goto ()
  "Select a song with helm interface."
  (interactive)
  (let ((title (helm :sources (helm-build-sync-source "audacious"
                                :candidates (butlast (butlast (cdr (split-string (shell-command-to-string "audtool --playlist-display") "\n"))))
                                :fuzzy-match nil)
                     :buffer "*helm audacious*")))
    (if title
        (progn
          (setq num (string-trim (car (split-string title "|"))))
          (call-process audacious-command nil nil nil "--playlist-jump" num)
          (sleep-for 0 20)
          (audacious-song-show-current-info)))))

(defun audacious-song-seek (time)
  "Seek the song by TIME in seconds."
  (interactive "MSeek +- sec: ")
  (call-process audacious-command nil nil nil ".--playback-seek-relative" time))

(defun audacious-song-seek-backward ()
  "Seek backward by 10 seconds."
  (interactive)
  (call-process audacious-command nil nil nil "--playback-seek-relative" "-10")
  (audacious-song-show-current-info))

(defun audacious-song-seek-forward ()
  "Seek forward by 10 seconds."
  (interactive)
  (call-process audacious-command nil nil nil "--playback-seek-relative" "+10")
  (audacious-song-show-current-info))

(defun audacious-song-show-current-info ()
  "Show information of the current song."
  (interactive)
  (setq current-playlist-position (delete-newline (shell-command-to-string "audtool --playlist-position")))
  (setq current-playlist-length (delete-newline (shell-command-to-string "audtool --playlist-length")))
  (setq current-song-title (delete-newline (shell-command-to-string "audtool --current-song")))
  (setq current-song-position (delete-newline (shell-command-to-string "audtool --current-song-output-length")))
  (setq current-song-length (delete-newline (shell-command-to-string "audtool --current-song-length")))
  (message "[%s/%s]: %s [%s / %s]" current-playlist-position current-playlist-length current-song-title current-song-position current-song-length))

(defun audacious-random-toggle ()
  "Toggle the random playback."
  (interactive)
  (if (string-match "off" (shell-command-to-string "audtool --playlist-shuffle-status"))
    (message "Random: ON")
    (message "Random: OFF"))
  (call-process audacious-command nil nil nil "--playlist-shuffle-toggle"))

(defun audacious-repeat-toggle ()
  "Toggle the repeat playback."
  (interactive)
  (if (string-match "off" (shell-command-to-string "audtool --playlist-repeat-status"))
    (message "Repeat: ON")
    (message "Repeat: OFF"))
  (call-process audacious-command nil nil nil "--playlist-repeat-toggle"))

(defun audacious-playlist ()
  "Show the songs of the current playlist."
  (interactive)
  (setq res "")
  (dolist (line (split-string (shell-command-to-string "audtool --playlist-display") "\n"))
    (if (string-match-p (regexp-quote "|") line)
        (setq res (concat res (concat line "\n")))))
  (message "%s" res))

(defun audacious-playlist-show-current-info ()
  "Show the name of the current playlist."
  (interactive)
  (setq current-playlist-name (delete-newline (shell-command-to-string "audtool --current-playlist-name")))
  (setq current-playlist-position (delete-newline (shell-command-to-string "audtool --current-playlist")))
  (setq playlist-length (delete-newline (shell-command-to-string "audtool --number-of-playlists")))
  (message "[%s/%s] \"%s\"" current-playlist-position playlist-length current-playlist-name))

(defun audacious-playlist--goto (num)
  "Select a playlist by NUM."
  (call-process audacious-command nil nil nil "--set-current-playlist" num)
  (sleep-for 0 20)
  (call-process audacious-command nil nil nil "--play-current-playlist")
  (audacious-playlist-show-current-info))

(defun audacious-playlist-goto ()
  "Select a playlist with an inputted number."
  (interactive)
  (setq playlist-length (delete-newline (shell-command-to-string "audtool --number-of-playlists")))
  (setq num (read-string (format "Playlist No. [1 - %s]: " playlist-length)))
  (if (string-integer-p num)
    (progn
      (call-process audacious-command nil nil nil "--set-current-playlist" num)
      (sleep-for 0 20)
      (call-process audacious-command nil nil nil "--play-current-playlist")
      (audacious-playlist-show-current-info))
    (message "\"%s\" is not number." num)))

(defun audacious-playlist-next ()
  "Select a next playlist."
  (interactive)
  (setq current-playlist-name (delete-newline (shell-command-to-string "audtool --current-playlist-name")))
  (setq current-playlist-position (string-to-number (delete-newline (shell-command-to-string "audtool --current-playlist"))))
  (setq playlist-length (string-to-number (delete-newline (shell-command-to-string "audtool --number-of-playlists"))))

  (let ((next-playlist-position (+ current-playlist-position 1)))
    (if (<= next-playlist-position playlist-length)
        (progn
          (audacious-playlist--goto (number-to-string next-playlist-position))
          (message "[%s/%s] \"%s\"" current-playlist-position playlist-length current-playlist-name)
          (sit-for 2)
          (audacious-song-show-current-info))
      (message "Last playlist"))))

(defun audacious-playlist-prev ()
  "Select a previous playlist."
  (interactive)
  (setq current-playlist-name (delete-newline (shell-command-to-string "audtool --current-playlist-name")))
  (setq current-playlist-position (string-to-number (delete-newline (shell-command-to-string "audtool --current-playlist"))))
  (setq playlist-length (string-to-number (delete-newline (shell-command-to-string "audtool --number-of-playlists"))))
  (let ((next-playlist-position (- current-playlist-position 1)))
    (if (>= next-playlist-position 1)
        (progn
          (audacious-playlist--goto (number-to-string next-playlist-position))
          (message "[%s/%s] \"%s\"" next-playlist-position playlist-length current-playlist-name)
          (sit-for 2)
          (audacious-song-show-current-info))
      (message "First playlist"))))

(defun delete-newline (str)
  (replace-regexp-in-string "\n$" "" str))

(defun string-integer-p (string)
  (if (string-match "\\`[-+]?[0-9]+\\'" string)
      t
    nil))

(provide 'audacious)
;;; audacious.el ends here
