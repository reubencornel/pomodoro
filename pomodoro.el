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

(defmacro pomodoro-timer-template(pomodoro-task-string max-size tick-functions complete-functions)
  "Macro for a pomodoro timer, checks if a timer is active, if not then sets a timer."
  `(if (null pomodoro-timer)
       (progn
         (setq pomodoro-task ,pomodoro-task)
         (setq pomodoro-timer (run-at-time 0
                                           60
                                           (pomodoro-tick ,max-size ,tick-function ,complete-functions))))
     (pomodoro-log-to-buffer "There is a timer already running")))

(defun map-functions(function-list)
  "Function to map across a list of functions."
  (mapcar #'(lambda(function)
              (funcall function))
          function-list))

(defun pomodoro-tick(time tick-functions complete-functions)
  "Function that provides the ticking..."
  (lexical-let ((pomodoro-minute 0)
                (max-time time)
                (on-complete-functions complete-functions)
                (on-tick-functions tick-functions))
    #'(lambda ()
        (incf pomodoro-minute)
        (pomodoro-log-to-buffer "Tick")
        (map-functions on-tick-functions)
        (when (> pomodoro-minute max-time)
          (map-functions end-functions)))))

;; Called when a timer is up
(defun generic-cleanup-function()
  (cancel-timer pomodoro-timer)
  (setq pomodoro-timer nil))

;; a function to start a pomodoro
(defun pomodoro-start(task)
  "Function that starts a pomodoro"
  (interactive "MTask Name:")
  (pomodoro-timer-template task
                           2
                           '()
                           (list #'generic-cleanup-function
                                 #'(lambda()
                                     (pomodoro-log-to-buffer "Completed Task: " task)))))

;; function to control break
(defun pomodoro-short-break()
  "Function that controls a short break"
  (interactive)
  (pomodoro-timer-template ""
                           pomodoro-short-break-size
                           '()
                           (list #'generic-cleanup-function
                                 #'(lambda()
                                     (pomodoro-log-to-buffer "Completed Short Break")))))

(defun pomodoro-long-break()
  "Function that controls a long break"
  (interactive)
  (pomodoro-timer-template "" pomodoro-long-break-size 
                           '()
                           (list #'generic-cleanup-function                              
                                 #'(lambda()
                                     (pomodoro-log-to-buffer "Completed Long Break")))))

;; Function to void a pomodoro
(defun cancel-pomodoro()
  "Cancels an existing pomodoro"
  (interactive)
  (cancel-timer pomodoro-timer)
  (setq pomodoro-timer nil)
  (pomodoro-log-to-buffer "Pomodoro cancelled for " pomodoro-task))

;; ability to log to a buffer
(defun pomodoro-log-to-buffer(&rest log-message)
  "Logging to a pomodoro buffer, in case we need to audit this stuff later"
  (save-excursion
    (save-current-buffer
      (pomodoro-create-log-buffer)
      (set-buffer pomodoro-buffer)
      (goto-char (point-max))             ;
      (insert "[" (current-time-string) "]: "  (apply 'concat log-message) "\n"))))


(defun pomodoro-create-log-buffer()
  (setq pomodoro-buffer (get-buffer-create pomodoro-buffer-name)))
