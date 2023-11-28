(defpackage asdarf
  (:use :cl))
(in-package :asdarf)

(defstruct foo
  hp)

(defstruct player
  damage)

(defparameter *foo-num* 5)
(defparameter *foo-hp-max* 25)

(defparameter *player* (make-player :damage '(6 . (1 . 3))))

(defun init-foos (*random-state*)
  (mapcar (lambda (hp) (make-foo :hp hp))
          (loop for x to *foo-num*
                collect (random *foo-hp-max*))))

(defun foo-print (foo)
  (format t "Foo (HP: ~d)~c" (foo-hp foo) #\newline))

(defun dice-min (dice) (car dice))
(defun dice-max (dice) (* (car dice) (cdr dice)))
(defun dice-desc (dice) (format nil "~dd~d" (car dice) (cdr dice)))

(defun player-damage-desc (player)
  (let ((damage-a (car (player-damage player)))
        (damage-b (cdr (player-damage player))))
    (format t "[~d~~~d] ~d+~a~c"
            (+ damage-a (dice-min damage-b))
            (+ damage-a (dice-max damage-b))
            damage-a (dice-desc damage-b) #\newline)))

(defun 1df (f *random-state*)
  (1+ (random f *random-state*)))

(defun ndf (n f *random-state*)
  (reduce #'+
          (loop for x from 1 to n
                collect (1df f *random-state*))))

(defun player-damage-det (player *random-state*)
  (let ((damage-a (car (player-damage player)))
        (damage-b (cdr (player-damage player))))
    (+ damage-a (ndf (car damage-b) (cdr damage-b) *random-state*))))

(player-damage-det *player* *random-state*)

(defun main ()
  (let ((foos ())))
  )
