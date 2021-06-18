(ql:quickload 'sketch)

(defun make-vec2 (x y)
  (list x y))

(defun vx (v) (car v))
(defun vy (v) (cadr v))

(defun v+ (&rest vs)
  (apply #'vec2-piecewise-op (cons '+ vs)))

(defun v- (v1 v2)
  (vec2-piecewise-op '- v1 v2))

(defun v* (&rest vs)
  (apply #'vec2-piecewise-op (cons '* vs)))

(defun v-scale (a v)
  (vec2-piecewise-op (lambda (x) (* a x)) v))

(defun v^ (v exp)
  (vec2-piecewise-op (lambda (x) (expt x exp)) v))

(defun vdot (v1 v2)
  (vec2-sum (v* v1 v2)))

(defun vec2-sum (v)
  (reduce '+ v :initial-value 0))

(defun vec2-piecewise-op (f &rest vs)
  (apply #'mapcar (cons f vs)))

(defun dist (v1 v2)
  (sqrt
   (vec2-sum
    (v^ (v- v1 v2) 2))))

;; Mass in kg, radius in m.
(defparameter *earth-mass* 5.972e24)
(defparameter *earth-radius* 6371e3)
(defparameter *earth-position* (make-vec2 0 0))

(defparameter *moon-mass* 7.348e22)
(defparameter *moon-radius* 1737e3)

(defparameter *escape-distance* (* 10 *earth-radius*))
(defparameter *default-run-time* (* 30 60))

(defparameter *G* 6.6743e-11)

(defparameter *width* 800)
(defparameter *height* 600)
(defparameter *pixels-per-m* (/ 75 *earth-radius*))

(defun distance-to-pixels (d)
  (ceiling (* d *pixels-per-m*)))

(defun gravity (pos1 m1 pos2 m2)
  (let ((r (dist pos1 pos2)))
    (/ (* *G* m1 m2) (* r r))))

(defun bodies-intersect (pos1 r1 pos2 r2)
  (> (+ r1 r2) (dist pos1 pos2)))

(defun vec2-length (v)
  (dist v (make-vec2 0 0)))

(defun vec2-to-unit (v)
  (v-scale (/ 1 (vec2-length v)) v))

(defun direction-from (v1 v2)
  (vec2-to-unit (v- v2 v1)))

(defun run (a dt &key (max-time *default-run-time*))
  (let ((pos (make-vec2 0 (+ *earth-radius* *moon-radius*)))
	(velocity (make-vec2 0 0))
	;; The Moon starts on "top" of Earth. And we've attached
	;; thrusters to the bottom of it so that it'll go straight up.
	(accel-vector (v-scale a (make-vec2 0 1)))
	(elapsed-time 0))
    (loop while (and (> *escape-distance* (dist pos *earth-position*))
		     (> max-time elapsed-time))
	  do (progn (incf elapsed-time dt)
		    ;; this is how much it has moved in that time.
		    ;; have to trim in case it collapses back into the Earth.
		    (let ((new-pos (v+ pos (v-scale dt velocity))))
		      (when (not (bodies-intersect pos *moon-radius* *earth-position* *earth-radius*))
			(setf pos new-pos)))
		    (setf pos (v+ pos (v-scale dt velocity)))
		    ;; and here's its new velocity, which is a function
		    ;; of its acceleration (rocket thrusters) and gravitational
		    ;; pull.
		    (let ((total-accel
			    (v-scale
			     dt
			     (v+ accel-vector
				 (v-scale (gravity pos *moon-mass* *earth-position* *earth-mass*)
					  (direction-from pos *earth-position*))))))
		      (setf velocity (v+ velocity total-accel)))))))

(defclass entity ()
  ())
(defgeneric draw (entity))

(defmethod draw ((entity entity))
  (sketch:rect 0 0 10 10))

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
    (sketch:with-pen (sketch:make-pen :fill colour)
      (apply #'sketch:circle
	     (mapcar #'distance-to-pixels) (list (vx pos) (vy pos) radius)))))

(sketch:defsketch spacesim
    ((title "Moon Crash")
     (width *width*)
     (height *height*)
     (earth (make-ball (floor (/ *width* 2))
		       (floor (/ *height* 2))
		       *earth-radius*
		       sketch:+blue+)))
  (sketch:background sketch:+black+)
  (draw earth))
