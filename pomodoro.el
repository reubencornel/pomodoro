;;; -*- lexical-binding: t -*-	
;; Local Variables:
;; lexical-binding: t
;; End:

(eval-when-compile
  (require 'cl))

(defvar pomodoro-log-to-buffer t)
(defvar pomodoro-buffer-name "*pomodoro*")
(defvar pomodoro-buffer nil)
(defvar pomodoro-in-progress nil)
(defvar pomodoro-task nil)
(defvar pomodoro-timer nil)

(defvar pomodoro-max-size 25)


;; a function to control pomodoro
(defun pomodoro-start(task)
  (interactive "MTask Name:")
  (if (null pomodoro-timer)
      (progn
        (setq pomodoro-task task)
        (pomodoro-log-to-buffer  "Starting pomodoro for " pomodoro-task)
        (setq pomodoro-timer (run-at-time 
                              0
                              60
                              (pomodoro-tick 2 "Completed Task:"))))
    (progn 
      (pomodoro-log-to-buffer "There is an existing timer running for task: " pomodoro-task))))


(defun pomodoro-tick(time complete-message)
  (lexical-let ((pomodoro-minute -1)
                (max-time time)
                (finish-message complete-message))
    #'(lambda ()
        (incf pomodoro-minute)
        (pomodoro-log-to-buffer "Current pomodoro minute: ")
        (when (>= pomodoro-minute max-time)
          (pomodoro-log-to-buffer finish-message pomodoro-task)
          (setq pomodoro-in-progress nil)
          (cancel-timer pomodoro-timer)
          (setq pomodoro-timer nil)))))

(defun cancel-pomodoro()
  (interactive)
  (cancel-timer pomodoro-timer)
  (setq pomodoro-timer nil)
  (pomodoro-log-to-buffer "Pomodoro cancelled for " pomodoro-task))


(setq pomodoro-tick-test (pomodoro-tick 2 "test"))
(funcall pomodoro-tick-test)
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


