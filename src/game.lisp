(in-package mooncrash)

;; Space constants and functions.
;; Mass in kg, radius in m.
(defparameter *earth-mass* 5.972e24)
(defparameter *earth-radius* 6371e3)

(defparameter *moon-mass* 7.348e22)
(defparameter *moon-radius* 1737e3)

(defparameter *G* 6.6743e-11)
(defparameter *friction-coefficient* 0.1)

(defparameter *dt* 1e-9
  "seconds per timestep")

(defparameter *launch-multiplier* 100000000)
(defparameter *points-in-trajectory* 10)

;; Screen constants.
(defparameter *width-pixels* 800)
(defparameter *height-pixels* 600)

;; Relationship between pixels & distance.
(defparameter *pixels-per-m* (/ *width-pixels* (* 15 *earth-radius*)))

(defun distance-to-pixels (d)
  (ceiling (* d *pixels-per-m*)))

(defun pixels-to-distance (p)
  (/ p *pixels-per-m*))

(defparameter *width* (pixels-to-distance *width-pixels*))
(defparameter *height* (pixels-to-distance *height-pixels*))

;; Star stuff.
(defparameter *star-box-size* (pixels-to-distance (/ *width-pixels* 80)))
(defparameter *star-probability* 0.00625)
(defparameter *star-size-pixels* (distance-to-pixels (/ *star-box-size* 4)))

;; Colours.
(defparameter *blue* (gamekit:vec4 0 0 1 1))
(defparameter *black* (gamekit:vec4 0 0 0 1))
(defparameter *white* (gamekit:vec4 1 1 1 1))
(defparameter *moon-colour* (gamekit:vec4 0.68 0.73 0.76 1.0))

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

(defun copy-ball (ball)
  (make-instance 'ball
		 :pos (pos ball)
		 :velocity (velocity ball)
		 :radius (radius ball)
		 :colour (colour ball)
		 :mass (mass ball)))

(defun balls-intersect (b1 b2)
  (< (dist (pos b1) (pos b2)) (+ (radius b1) (radius b2))))

(defun make-moon (position)
  (make-ball (vec2-x position) (vec2-y position) *moon-radius* *moon-colour* *moon-mass*))

(defun make-earth ()
  (make-ball (/ *width* 2) (/ *height* 2) *earth-radius* *blue* *earth-mass*))

(defmethod draw ((ball ball))
  (with-accessors ((pos pos)
		   (radius radius)
		   (colour colour))
      ball
    (gamekit:draw-circle (gamekit:vec2 (distance-to-pixels (vec2-x pos))
				       (distance-to-pixels (vec2-y pos)))
			 (distance-to-pixels radius)
			 :fill-paint colour)))

(defun gravitational-acceleration (ball-from ball-to)
  "Returns gravitational acceleration between two masses, as a vector."
  (with-accessors ((pos1 pos)
		   (m1 mass))
      ball-from
    (with-accessors ((pos2 pos)
		     (m2 mass))
	ball-to
      (let ((r (dist pos1 pos2)))
	(v-scale (/ (* *G* m1 m2) (* r r))
		 (direction-from pos1 pos2))))))

;; Game state.
(defvar *earth*)
(defvar *moon*)
(defvar *moon-frozen*)
(defvar *trajectory*)
(defvar *cursor-position* (gamekit:vec2 0 0))
(defvar *star-positions*)

(defun cursor-to-vec2 ()
  (make-vec2 (pixels-to-distance (gamekit:x *cursor-position*))
	     (pixels-to-distance (gamekit:y *cursor-position*))))

(defun vec2-to-gamekit (v)
  (gamekit:vec2 (distance-to-pixels (vec2-x v))
		(distance-to-pixels (vec2-y v))))

(defun launch-velocity (ball)
  (v-scale *launch-multiplier* (v- (pos ball) (cursor-to-vec2))))

(defun update-ball-state! (ball)
  (let* ((new-pos (v+ (pos ball) (v-scale *dt* (velocity ball))))
	 (offset (- (+ (radius *earth*) (radius ball)) (dist new-pos (pos *earth*)))))
    (when (< 0 offset)
      ;; Collision! Don't let the bodies overlap.
      (setf new-pos (v+ new-pos (v-scale offset (direction-from (pos *earth*) new-pos))))
      ;; Also need to neutralise the velocity in the direction of Earth.
      ;; ...but then gravity causes it to accelerate back towards Earth immediately?
      ;; I guess that's okay?
      ;; So, project velocity onto direction vector from Earth to the ball.
      ;; Then reverse it and subtract from the velocity vector.
      (let ((dir (direction-from new-pos (pos *earth*))))
	(setf (velocity ball) (v- (velocity ball)
				  (v-scale (vdot (velocity ball) dir) dir)))
	;; Now accounting for friction so that the ball doesn't roll around Earth
	;; like a mad yolk.
	(setf (velocity ball) (v+ (velocity ball)
				  (v-scale (- *friction-coefficient*)
					   (v- (velocity ball)
					       (v-scale (vdot (velocity ball) dir) dir)))))))
    (setf (pos ball) new-pos))
  (setf (velocity ball)
	(v+ (velocity ball)
	    (v-scale *dt* (gravitational-acceleration ball *earth*)))))

;; Finally, all the gamekit stuff to run the game.
(gamekit:defgame spacesim () ()
  (:viewport-title "Mooncrash")
  (:viewport-width *width-pixels*)
  (:viewport-height *height-pixels*))

(defmethod gamekit:post-initialize ((app spacesim))
  (setf *earth* (make-earth))
  (setf *moon* nil)
  (setf *moon-frozen* nil)
  (setf *trajectory* (list))
  (setf *star-positions* (list))
  (loop for x = 0 then (+ x *star-box-size*)
	while (< x *width*)
	do (loop for y = 0 then (+ y *star-box-size*)
		 while (< y *height*)
		 when (< (random 1.0) *star-probability*)
		   do (push (vec2-to-gamekit
			     (make-vec2 (+ x (* (random 1.0) *star-box-size*))
					(+ y (* (random 1.0) *star-box-size*))))
			    *star-positions*)))
  (gamekit:bind-cursor
   (lambda (x y)
     (setf (gamekit:x *cursor-position*) x
	   (gamekit:y *cursor-position*) y)
     (when (and *moon-frozen* *moon*)
       (let ((faux-moon (copy-ball *moon*)))
	 (setf *trajectory* nil)
	 (setf (velocity faux-moon) (launch-velocity faux-moon))
	 (loop repeat *points-in-trajectory*
	       do (push (vec2-to-gamekit (pos faux-moon)) *trajectory*)
	       do (update-ball-state! faux-moon))))))
  (gamekit:bind-button
   :mouse-left :pressed
   (lambda ()
     (let ((potential-moon (make-moon (cursor-to-vec2))))
       (when (not (balls-intersect potential-moon *earth*))
	 (setf *moon-frozen* t)
	 (setf *moon* potential-moon)))))
  (gamekit:bind-button
   :mouse-left :released
   (lambda ()
     (when (and *moon-frozen* *moon*)
       (setf *trajectory* nil)
       (setf *moon-frozen* nil)
       (setf (velocity *moon*) (launch-velocity *moon*))))))

(defmethod gamekit:draw ((app spacesim))
  (gamekit:draw-rect (gamekit:vec2 0 0) *width-pixels* *height-pixels* :fill-paint *black*)
  (loop for position in *star-positions*
	do (gamekit:draw-rect position *star-size-pixels* *star-size-pixels* :fill-paint *white*))
  (draw *earth*)
  (when *moon*
    (draw *moon*))
  (when (and *moon-frozen* *trajectory*)
    (gamekit:draw-polyline *trajectory* (gamekit:vec4 1.0 0.0 0.0 1.0))))

(defmethod gamekit:act ((app spacesim))
  (when (and *moon* (not *moon-frozen*))
    (update-ball-state! *moon*)))
