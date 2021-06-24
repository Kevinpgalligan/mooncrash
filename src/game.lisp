(in-package mooncrash)

;; Space constants.
;; Mass in kg, radius in m.
(defparameter *earth-mass* 5.972e24)
(defparameter *earth-radius* 6371e3)

(defparameter *moon-mass* 7.348e22)
(defparameter *moon-radius* 1737e3)

(defparameter *G* 6.6743e-11)

(defun gravity (pos1 m1 pos2 m2)
  (let ((r (dist pos1 pos2)))
    (/ (* *G* m1 m2) (* r r))))

;; Relationship between pixels & distance.
(defparameter *pixels-per-m* (/ 75 *earth-radius*))

(defun distance-to-pixels (d)
  (ceiling (* d *pixels-per-m*)))

;; Screen constants.
(defparameter *width* (* 10 *earth-radius*))
(defparameter *height* (* 8 *earth-radius*))
(defparameter *width-pixels* (distance-to-pixels *width*))
(defparameter *height-pixels* (distance-to-pixels *height*))

;; Colours.
(defparameter *blue* (gamekit:vec4 0 0 1 1))
(defparameter *black* (gamekit:vec4 0 0 0 1))
(defparameter *white* (gamekit:vec4 1 1 1 1))

;; Entities and functions for drawing them.
(defclass entity ()
  ())
(defgeneric draw (entity))

(defmethod draw ((entity entity))
  (gamekit:draw-rect (gamekit:vec2 0 0) 10 10))

(defclass ball (entity)
  ((pos
    :initarg :pos
    :accessor pos)
   (radius
    :initarg :radius
    :accessor radius)
   (colour
    :initarg :colour
    :accessor colour)))

(defun make-ball (x y r c)
  (make-instance 'ball
		 :pos (make-vec2 x y)
		 :radius r
		 :colour c))

(defmethod draw ((ball ball))
  (with-accessors ((pos pos)
		   (radius radius)
		   (colour colour))
      ball
    (gamekit:draw-circle (gamekit:vec2 (distance-to-pixels (vx pos))
				       (distance-to-pixels (vy pos)))
			 (distance-to-pixels radius)
			 :fill-paint colour)))

;; Actual entities.
(defparameter *earth*
  (make-ball (/ *width* 2)
	     (/ *height* 2)
	     *earth-radius*
	     *blue*))

(defparameter *moon*
  (make-ball (+ (* 3 *earth-radius*) (/ *width* 2))
	     (+ (* 3 *earth-radius*) (/ *height* 2))
	     *moon-radius*
	     *white*))

;; Finally, all the stuff to run the game.
(gamekit:defgame spacesim () ()
  (:viewport-title "Mooncrash")
  (:viewport-width *width-pixels*)
  (:viewport-height *height-pixels*))

(defmethod gamekit:draw ((app spacesim))
  (gamekit:draw-rect (gamekit:vec2 0 0) *width-pixels* *height-pixels* :fill-paint *black*)
  (draw *earth*)
  (draw *moon*))
