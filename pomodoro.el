(defvar log-to-buffer t)
(defvar pomodoro-buffer-name "*pomodoro*")
(defvar pomodoro-buffer nil)

;; a function to control pomodoro

;; function to control break

;; Function to void a pomodoro

;; ability to update an org file entry with pomodoro.


;; Function for timer;;

;; Function to update the existing mode line
;; function to reset the mode line.

;; ability to log to a buffer
;; have to be able to define a new buffer.
(defun create-pomodoro-buffer()
  (interactive)
  (setq pomodoro-buffer (generate-new-buffer pomodoro-buffer-name)))
