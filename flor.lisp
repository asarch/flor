(ql:quickload :cl-glut)
(ql:quickload :cffi)

(defclass point ()
  ((x :initarg :x :initform 0.0 :accessor x :documentation "x-axis value")
   (y :initarg :y :initform 0.0 :accessor y :documentation "y-axis value")
   (z :initarg :z :initform 0.0 :accessor z :documentation "z-axis value")))

(defclass flor (glut:window)
  ((ang :initform (make-instance 'point))
   (pos :initform (make-instance 'point))
   (init-pos :initform (make-instance 'point))
   (dragging :initform nil))
  (:default-initargs :title "GLUT Flor for Common Lisp" :mode '(:double :rgb :depth)))

(defmethod glut:display-window :before ((window flor))
  (with-slots (ang pos init-pos) window))

(defmethod glut:display ((window flor))
  (gl:clear :color-buffer :depth-buffer)
  (gl:load-identity)
  (with-slots (pos ang) window
    (gl:translate (slot-value pos 'x) (slot-value pos 'y) (slot-value pos 'z))
    (gl:rotate (slot-value ang 'x) 0.0 1.0 0.0)
    (gl:rotate (slot-value ang 'y) 1.0 0.0 0.0))
  ;;(if display-list-id (gl:call-list display-list-id))
  (glut:swap-buffers))

;;--------------------------------------------------------------------
;;
;; *** TODO ***
;;
;;  No se puede hacer esto directamente, tienes que declara
;;  una funcion (de acuerdo con pjb) como:
;;
;;  (cffi:defcallback my-callback …)
;;  (glut:motion-func (cffi:callback my-callback))

(cffi:defcallback motion-func (format t "Hello, world!"))

(glut:motion-func (cffi:callback motion-func))

;(defmethod glut:motion-func ((window flor) x y)
;  (glut:post-redisplay))
;  (with-slots (dragging ang init-ang) window
   ; (cond ((= dragging 1))
   ; (if dragging
;	(progn
;	  ;; $ang->x($ang->x + ($x - $init_ang->x)); 
;	  (setf (slot-value ang 'x) (+ (slot-value ang 'x) (- x (slot-value init-ang 'x))))
;	  (setf (slot-value ang 'y) (+ (slot-value ang 'y) (- y (slot-value init-ang 'y))))
;	  (setf (slot-value init-ang 'x) x)
;	  (setf (slot-value init-ang 'y) y)
;	  (glut:post-redisplay)
;         (glut:post-redisplay)
;	  ))))

;;(glut:motion-func #'(lambda (x y) (format t "Hello, world!" x y)))
;;
;;--------------------------------------------------------------------

(glut:init)
(glut:display-window (make-instance 'flor))
(glut:main-loop)
