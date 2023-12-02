(defpackage asdarf
  (:use :cl))
(in-package :asdarf)

(defstruct player
  damage)

(defun player-damage-desc (player)
  (let ((damage-a (car (player-damage player)))
        (damage-b (cdr (player-damage player))))
    (format nil "[~d~~~d] ~d+~a"
            (+ damage-a (dice-min damage-b))
            (+ damage-a (dice-max damage-b))
            damage-a (dice-desc damage-b))))

(defun player-damage-det (player *random-state*)
  (let ((damage-a (car (player-damage player)))
        (damage-b (cdr (player-damage player))))
    (+ damage-a (ndf (car damage-b) (cdr damage-b) *random-state*))))
