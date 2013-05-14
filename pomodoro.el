(defvar pomodoro-log-to-buffer t)
(defvar pomodoro-buffer-name "*pomodoro*")
(defvar pomodoro-buffer nil)
(defvar pomodoro-in-progress nil)
(defvar pomodoro-task nil)
(defvar pomodoro-timer nil)

;; a function to control pomodoro
(defun pomodoro-start(task)
  (interactive "MTask Name:")
  (if (null pomodoro-timer)
      (progn
        (setq pomodoro-task task)
        (pomodoro-log-to-buffer  "Starting pomodoro for " pomodoro-task)
        (setq pomodoro-timer (run-at-time "25 min" nil 'pomodoro-complete)))
    (progn 
      (pomodoro-log-to-buffer "There is an existing timer running for task: " pomodoro-task))))

(defun pomodoro-complete()
  (setq pomodoro-in-progress nil)
  (setq pomodoro-timer nil)
  (pomodoro-log-to-buffer "Pomodoro complete for " pomodoro-task))

(defun cancel-pomodoro()
  (cancel-timer pomodoro-timer)
  (setq pomodoro-timer nil)
  (pomodoro-log-to-buffer "Pomodoro cancelled for " pomodoro-task))


;; Have to be able to define a set of tasks
;; Have the ability to look up tasks from org-mode

;; function to control break

;; Function to void a pomodoro

;; ability to update an org-mode file entry with pomodoro.
;; Ability to update pomodoro-buffer


;; Function for timer;;
(defun pomodoro-timer(time function)
  (run-at-time time nil function))

;; Function to update the existing mode line
;; function to reset the mode line.

;; ability to log to a buffer
(defun pomodoro-log-to-buffer(&rest log-message)
  (save-excursion
    (save-current-buffer 
      (pomodoro-create-log-buffer)
      (set-buffer pomodoro-buffer)
      (goto-char (point-max))             ;
      (insert "[" (current-time-string) "]: "  (apply 'concat log-message) "\n"))))


(defun pomodoro-create-log-buffer()
  (setq pomodoro-buffer (get-buffer-create pomodoro-buffer-name)))

(print global-mode-string)

(display
