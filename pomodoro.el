;;; -*- lexical-binding: t -*-
;; Local Variables:
;; lexical-binding: t
;; End:

;; Have to be able to define a set of tasks
;; Have the ability to look up tasks from org-mode
;; ability to update an org-mode file entry with pomodoro.
;; Ability to update pomodoro-buffer
;; Function to update the existing mode line
;; function to reset the mode line.


(eval-when-compile
  (require 'cl))

(defvar pomodoro-log-to-buffer t)
(defvar pomodoro-buffer-name "*pomodoro*")
(defvar pomodoro-buffer nil)
(defvar pomodoro-in-progress nil)
(defvar pomodoro-task nil)
(defvar pomodoro-timer nil)

(defconst pomodoro-max-size 25)
(defconst pomodoro-short-break-size 5)
(defconst pomodoro-long-break-size 15)

(defmacro pomodoro-timer-template(pomodoro-task-string max-size pomodoro-task-complete-str)
  `(if (null pomodoro-timer)
       (progn
         (setq pomodoro-task ,pomodoro-task)
         (setq pomodoro-timer (run-at-time 0
                                           60
                                           (pomodoro-tick ,max-size ,pomodoro-task-complete-str))))
     (pomodoro-log-to-buffer "There is a timer already running")))


(defun pomodoro-tick(time complete-message)
  (lexical-let ((pomodoro-minute 0)
                (max-time time)
                (finish-message complete-message))
    #'(lambda ()
        (incf pomodoro-minute)
        (pomodoro-log-to-buffer "Tick")
        (when (> pomodoro-minute max-time)
          (pomodoro-log-to-buffer finish-message)
          (setq pomodoro-in-progress nil)
          (cancel-timer pomodoro-timer)
          (setq pomodoro-timer nil)))))

;; a function to control pomodoro
(defun pomodoro-start(task)
  (interactive "MTask Name:")
  (pomodoro-timer-template task pomodoro-max-size (concat "Completed Task: " task)))

;; function to control break
(defun pomodoro-short-break()
  (interactive)
  (pomodoro-timer-template "" pomodoro-short-break-size "Completed short break"))

(defun pomodoro-long-break()
  (interactive)
  (pomodoro-timer-template "" pomodoro-long-break-size "Completed long break"))

;; Function to void a pomodoro
(defun cancel-pomodoro()
  (interactive)
  (cancel-timer pomodoro-timer)
  (setq pomodoro-timer nil)
  (pomodoro-log-to-buffer "Pomodoro cancelled for " pomodoro-task))


;; Function for timer;;
(defun pomodoro-timer(time function)
  (run-at-time time nil function))


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


