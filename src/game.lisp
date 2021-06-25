(in-package mooncrash)

;; Space constants and functions.
;; Mass in kg, radius in m.
(defparameter *earth-mass* 5.972e24)
(defparameter *earth-radius* 6371e3)

(defparameter *moon-mass* 7.348e22)
(defparameter *moon-radius* 1737e3)

(defparameter *G* 6.6743e-11)

(defparameter *dt* 1e-19
  "seconds per timestep")

(defun gravitational-acceleration (pos1 m1 pos2 m2)
  "Returns gravitational acceleration between two masses, as a vector.
The direction is from the first mass to the second."
  (let ((r (dist pos1 pos2)))
    (v-scale (/ (* *G* m1 m2) (* r r))
	     (direction-from pos1 pos2))))

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
   (velocity
    :initarg :velocity
    :accessor velocity)
   (radius
    :initarg :radius
    :reader radius)
   (colour
    :initarg :colour
    :reader colour)
   (mass
    :initarg :mass
    :reader mass)))

(defun make-ball (x y r c mass &key (vx 0) (vy 0))
  (make-instance 'ball
		 :pos (make-vec2 x y)
		 :velocity (make-vec2 vx vy)
		 :radius r
		 :colour c
		 :mass mass))

(defmethod draw ((ball ball))
  (with-accessors ((pos pos)
		   (radius radius)
		   (colour colour))
      ball
    (gamekit:draw-circle (gamekit:vec2 (distance-to-pixels (vec2-x pos))
				       (distance-to-pixels (vec2-y pos)))
			 (distance-to-pixels radius)
			 :fill-paint colour)))

;; Actual entities.
(defvar *earth*)
(defvar *moon*)

;; Finally, all the stuff to run the game.
(gamekit:defgame spacesim () ()
  (:viewport-title "Mooncrash")
  (:viewport-width *width-pixels*)
  (:viewport-height *height-pixels*))

(defmethod gamekit:post-initialize ((app spacesim))
  (setf *earth* (make-ball (/ *width* 2)
			   (/ *height* 2)
			   *earth-radius*
			   *blue*
			   *earth-mass*))
  (setf *moon* (make-ball (+ (* 3 *earth-radius*) (/ *width* 2))
			  (+ (* 3 *earth-radius*) (/ *height* 2))
			  *moon-radius*
			  *white*
			  *earth-mass*
			  :vx 10000
			  :vy 3000)))

(defmethod gamekit:draw ((app spacesim))
  (gamekit:draw-rect (gamekit:vec2 0 0) *width-pixels* *height-pixels* :fill-paint *black*)
  (draw *earth*)
  (draw *moon*))

(defmethod gamekit:act ((app spacesim))
  (format t "position ~a velocity ~a distance ~a~%" (pos *moon*) (velocity *moon*) (v-scale *dt* (velocity *moon*)))
  (format t "and acceleration: ~a~%" (gravitational-acceleration (pos *moon*) (mass *moon*) (pos *earth*) (mass *earth*)))
  (setf (pos *moon*)
	(v+ (pos *moon*) (v-scale *dt* (velocity *moon*))))
  (setf (velocity *moon*)
	(v+ (velocity *moon*)
	    (v-scale *dt* (gravitational-acceleration (pos *moon*) (mass *moon*) (pos *earth*) (mass *earth*))))))
