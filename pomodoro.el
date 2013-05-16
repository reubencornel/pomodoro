;;; -*- lexical-binding: t -*-
;; Local Variables:
;; lexical-binding: t
;; End:

;; Have to be able to define a set of tasks
;; Have the ability to look up tasks from org-mode
;; Ability to record distractions
;; ability to update an org-mode file entry with pomodoro.
;; Ability to update pomodoro-buffer
;; Function to update the existing mode line
;; function to reset the mode line.


(eval-when-compile
  (require 'cl))

(defvar pomodoro-log-to-buffer t)
(defvar pomodoro-buffer-name "*pomodoro*")
(defvar pomodoro-buffer nil)
(defvar pomodoro-state "") ;; LB:SB:TK
(defvar pomodoro-task nil)
(defvar pomodoro-timer nil)
(defvar pomodoro-mode-line-string "")

(defconst pomodoro-default-max-size 25)
(defconst pomodoro-default-short-break-size 5)
(defconst pomodoro-default-long-break-size 15)
(defvar pomodoro-size-of-tick 60)

(defvar pomodoro-max-size 25)
(defvar pomodoro-short-break-size 5)
(defvar pomodoro-long-break-size 15)
(defvar pomodoro-external-minute 0)

(defvar pomodoro-custom-on-start-functions '())
(defvar pomodoro-custom-on-complete-functions '())
(defvar pomodoro-custom-on-tick-functions '())
(defvar pomodoro-custom-on-cancel-functions '())

(defun map-functions(function-list)
  "Function to map across a list of functions."
  (mapcar #'(lambda(function)
              (funcall function))
          function-list))

(defmacro pomodoro-timer-template(max-size start-functions tick-functions complete-functions)
  "Macro for a pomodoro timer, checks if a timer is active, if not then sets a timer."
  `(if (null pomodoro-timer)
       (progn
         (map-functions ,start-functions)
         (setq pomodoro-timer 
               (run-at-time 0
                            pomodoro-size-of-tick
                            (pomodoro-tick ,max-size 
                                           ,tick-functions
                                           (append (list #'generic-cleanup-function)
                                                   ,complete-functions)))))
     (pomodoro-log-to-buffer "There is a timer already running")))

(defun pomodoro-tick(time tick-functions complete-functions)
  "Function that provides the ticking..."
  (lexical-let ((pomodoro-minute 0)
                (max-time time)
                (on-complete-functions complete-functions)
                (on-tick-functions tick-functions))
    #'(lambda ()
        (incf pomodoro-minute)
        (setq pomodoro-external-minute (- max-time pomodoro-minute))
        (map-functions on-tick-functions)
        (when (> pomodoro-minute max-time)
          (map-functions on-complete-functions)))))

(defun pomodoro-message (msg)
  "Function to write to the pomodoro buffer, just a wrapper, so that I don't have to write lambdas everywhere."
  (lexical-let ((message msg))
    #'(lambda()
        (pomodoro-log-to-buffer message))))
             

;; Called when a timer is up
(defun generic-cleanup-function()
  (setq pomodoro-task "")
  (cancel-timer pomodoro-timer)
  (setq pomodoro-timer nil))

;; a function to start a pomodoro
(defun pomodoro-start(task)
  "Function that starts a pomodoro"
  (interactive "MTask Name:")
  (setq pomodoro-task task)
  (setq pomodoro-state "TK")
  (pomodoro-timer-template pomodoro-max-size
                           (append (list (update-mode-line)
                                         (pomodoro-message (concat "Starting pomodoro for: " task)))
                                 pomodoro-custom-on-start-functions)
                           (cons (update-mode-line)
                                 pomodoro-custom-on-tick-functions)
                           (append (list (update-mode-line)
                                         (pomodoro-message (concat "Completed Task:" task)))
                                   pomodoro-custom-on-complete-functions)))

;; function to control break
(defun pomodoro-short-break()
  "Function that controls a short break"
  (interactive)
  (setq pomodoro-task "Short Break")
  (setq pomodoro-state "SB")
  (pomodoro-timer-template pomodoro-short-break-size
                           (append (list (update-mode-line)
                                         (pomodoro-message "Starting short break"))
                                   pomodoro-custom-on-start-functions)
                           (cons (update-mode-line)
                                 pomodoro-custom-on-tick-functions)
                           (append (list #'(lambda()(pomodoro-log-to-buffer "Completed Short Break"))
                                         (update-mode-line))
                                   pomodoro-custom-on-complete-functions)))

(defun pomodoro-long-break()
  "Function that controls a long break"
  (interactive)
  (setq pomodoro-state "LB")
  (setq pomodoro-task "Long Break")
  (pomodoro-timer-template pomodoro-long-break-size 
                           (cons (update-mode-line) pomodoro-custom-on-start-functions)
                           (cons (update-mode-line)
                                 pomodoro-custom-on-tick-functions)
                           (append (list (update-mode-line)
                                         #'(lambda()(pomodoro-log-to-buffer "Completed Long Break")))
                                   pomodoro-custom-on-complete-functions)))

(defun update-mode-line()
  "Returns a function that updates the value of the mode line"
  (lambda()
    (if (null pomodoro-timer)
        (progn
          (delq 'pomodoro-mode-line-string global-mode-string)
          (setq pomodoro-mode-line-string ""))
      (progn 
        (add-to-list 'global-mode-string 'pomodoro-mode-line-string 'append)
        (setq pomodoro-mode-line-string
              (format " %s %02d:%02d" 
                      pomodoro-state
                      00
                      pomodoro-external-minute))))
      (force-mode-line-update)))
                      
               
;; Function to void a pomodoro
(defun cancel-pomodoro()
  "Cancels an existing pomodoro"
  (interactive)
  (map-functions (append pomodoro-custom-on-cancel-functions
                         (list #'generic-cleanup-function 
                               (pomodoro-message (concat "Pomodoro cancelled for " pomodoro-task))))))


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
