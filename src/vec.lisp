(in-package mooncrash)

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

(defun vec2-length (v)
  (dist v (make-vec2 0 0)))

(defun vec2-to-unit (v)
  (v-scale (/ 1 (vec2-length v)) v))

(defun direction-from (v1 v2)
  (vec2-to-unit (v- v2 v1)))
