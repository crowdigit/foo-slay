(defpackage asdarf
  (:use :cl))
(in-package :asdarf)

(defstruct state
  running 
  player
  foos)

(defparameter *eof* (gensym))

(defun dice-min (dice) (car dice))
(defun dice-max (dice) (* (car dice) (cdr dice)))
(defun dice-desc (dice) (format nil "~dd~d" (car dice) (cdr dice)))

(defun 1df (f *random-state*)
  (1+ (random f *random-state*)))

(defun ndf (n f *random-state*)
  (reduce #'+
          (loop for x from 1 to n
                collect (1df f *random-state*))))

(defun print-state (state)
  (reduce (lambda (acc foo)
              (format t "[~d] " acc)
              (foo-print foo)
              (1+ acc))
            (state-foos state)
            :initial-value 0)
    (format t "attack damage: ~a~c"
            (player-damage-desc (state-player state))
            #\newline))

(defun print-prompt ()
  (princ "> "))

(defun game-r ()
  (handler-case
    (let ((input (make-string-input-stream (read-line))))
      (loop for x = (read input nil *eof*)
            while (not (eq x *eof*)) 
            collect x))
    (reader-error () '(reader-error "invalid command syntax"))))

(defun case-attack (state command args)
  (format t "attack damage: ~a~c" (player-damage-det (state-player state) *random-state*) #\newline))

(defun cond-command (state command args)
  (cond ((eq 'quit command) (setf (state-running state) nil))
        ((eq 'reader-error command) (format t "~a~c" (car args) #\newline))
        ((eq 'attack command) (case-attack state command args))
        (t (format t "unknown command: ~a~c" command #\newline))))

(defun game-e (state commands)
  (let ((command (car commands))
        (args (cdr commands)))
    (cond-command state command args)
    state))

(defun game-p (state)
  (print-state state)
  (print-prompt))

(defun game-l (state)
  (let* ((commands (game-r))
         (state (game-e state commands)))
    (if (state-running state)
        (progn (game-p state)
               (game-l state)))))

(defun main ()
  (let ((state (make-state :running t
                         :player (make-player :damage '(6 . (1 . 3)))
                         :foos (init-foos *random-state*))))
  (game-p state)
  (game-l state)))
