(defpackage asdarf
  (:use :cl))
(in-package :asdarf)

(defstruct state
  running 
  player
  foos
  foo-num)

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

(defun state-clear-foos (state)
  (setf (state-foos state)
        (reduce (lambda (acc foo)
                  (if (/= 0 (foo-hp foo))
                      (cons foo acc)
                      acc))
                (reverse (state-foos state))
                :initial-value nil))
  (setf (state-foo-num state) (length (state-foos state))))

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

(defun case-attack (state args)
  (let ((target (car args)))
    (if (typep target 'integer)
        (if (< target (state-foo-num state))
            (let* ((player (state-player state))
                   (dmg-range (player-damage-desc player))
                   (dmg-det (player-damage-det player *random-state*))
                   (foo (nth target (state-foos state)))
                   (foo-hp-after (max 0 (- (foo-hp foo) dmg-det))))
              (format t "damage = ~a = ~a~c" dmg-range dmg-det #\newline)
              (format t "foo hp = max(~a - ~a, 0) = ~a~c" (foo-hp foo) dmg-det foo-hp-after #\newline) 
              (setf (foo-hp foo) foo-hp-after)
              (if (= foo-hp-after 0)
                  (progn (format t "you killed target ~a~c" target #\newline)
                         (state-clear-foos state))))
            (format t "invalid target number~c" #\newline))
        (error (make-condition 'invalid-argument-type
                               :expected-type 'integer
                               :value (car args))))))

(defun cond-command (state command args)
  (cond ((eq 'quit command) (setf (state-running state) nil))
        ((eq 'reader-error command) (format t "~a~c" (car args) #\newline))
        (t (handler-case
             (cond ((eq 'attack command) (case-attack state args))
                   (t (format t "unknown command: ~a~c" command #\newline)))
             (asdarf-error (err) (format t "~a~c" err #\newline))))))

(defun game-e (state commands)
  (let ((command (car commands))
        (args (cdr commands)))
    (cond-command state command args)
    (if (= 0 (state-foo-num state))
        (progn (setf (state-running state) nil)
               (format t "You killed them all! You murderer!")))
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
                         :foos (init-foos *random-state*)
                         :foo-num *foo-num*)))
  (game-p state)
  (game-l state)))
