(ql:quickload :cl-glu)
(ql:quickload :cl-glut)

(defclass point ()
  ((x :initarg :x :initform 0.0 :accessor x :documentation "x-axis value")
   (y :initarg :y :initform 0.0 :accessor y :documentation "y-axis value")
   (z :initarg :z :initform 0.0 :accessor z :documentation "z-axis value")))

(defconstant +exit+ 0)
(defconstant +fullscreen+ 1)

(defparameter *dragging* nil)
(defparameter *anti-alias* nil)
(defparameter *lighting* nil)
(defparameter *fullscreen* nil)
(defparameter *display-list-id* 0)

(defparameter *pos* (make-instance 'point :z (- 3)))
(defparameter *ang* (make-instance 'point))
(defparameter *init-ang* (make-instance 'point))

(defun axis (size)
  (gl:begin :lines)
  (gl:color 1.0 0.0 0.0) (gl:vertex (- size) 0.0 0.0) (gl:vertex size 0.0 0.0)
  (gl:color 0.0 1.0 0.0) (gl:vertex 0.0 (- size) 0.0) (gl:vertex 0.0 size 0.0)
  (gl:color 0.0 0.0 1.0) (gl:vertex 0.0 0.0 (- size)) (gl:vertex 0.0 0.0 size)
  (gl:end))

(defun init-glscene ()
  (cond ((not (null *anti-alias*)) ;; Antialias
	 (gl:enable :line-smooth)
	 (gl:enable :point-smooth)
	 (gl:enable :blend)
	 (gl:blend-func :src-alpha :one-minus-src-alpha)
	 (gl:hint :line-smooth-hint :dont-care)
	 (gl:line-width 1.5)
	 (gl:enable :depth-test))
	(t ;; No antialias
	 (gl:shade-model :smooth)
	 (gl:clear-color 0.0 0.0 0.0 0.5)
	 (gl:clear-depth 1.0)
	 (gl:enable :depth-test)
	 (gl:depth-func :lequal)
	 (gl:hint :perspective-correction-hint :nicest)))

  (when *lighting* ;; Lighting
    (gl:enable :light0)
    (gl:enable :lighting)
    (gl:enable :auto-normal)
    (gl:enable :normalize)
    (gl:enable :color-material))

  (let (angle coords radian (points 17) (radius 1.0))
    (setf coords (make-array points :fill-pointer 0 :adjustable t))
    (setf angle (/ 360.0 points))
    
    ;; Save coords
    (loop for i from 1 to points do
      (setf radian (/ (* (* angle i) pi) 180))
      (vector-push-extend (make-instance 'point :x (* radius (cos radian)) :y (* radius (sin radian))) coords))
    
    (setf *display-list-id* (gl:new-list *display-list-id* :compile))
    
    (cond ((not (null *display-list-id*))
	   (gl:new-list *display-list-id* :compile)
	   (gl:begin :lines)
	   
	   (loop for i from 1 to points do
	     (loop for j from i to points do
	       (gl:vertex (x (elt coords i)) (y (elt coords i)))
	       (gl:vertex (x (elt coords j)) (y (elt coords j)))))
	   
	   (gl:end)
	   
	   (let ((quadric-object (glu:new-quadric)))	   
	     (cond ((not (null quadric-object))
		    (glu:quadric-draw-style quadric-object :silhouette)
		    (glu:partial-disk quadric-object 0.0 1.0 64 32 0 360)
		    (glu:delete-quadric quadric-object))))
	   
	   (gl:end-list)))))

(defclass window (glut:window)
  ()
  (:default-initargs :title "Flor" :mode '(:double :rgb :depth)))

(defmethod glut:display ((w window))
  (gl:clear :color-buffer-bit :depth-buffer-bit)
  (gl:load-identity)
  (gl:translate (x *pos*) (y *pos*) (z *pos*))
  (gl:rotate (x *ang*) 0.0 1.0 0.0)
  (gl:rotate (y *ang*) 1.0 0.0 0.0)
  
  (if *display-list-id* (gl:call-list *display-list-id*))
  
  (glut:swap-buffers))

(defmethod glut:reshape ((w window) width height)
  (when height ;; Prevents a divition by zero
      (gl:viewport 0 0 width height)
      (gl:matrix-mode :projection)
      (gl:load-identity)
      (glu:perspective 45.0 (/ width height) 0.1 100.0)
      (gl:matrix-mode :modelview)
      (gl:load-identity)))

;; CL-GLUT:IDLE-FUNC already names an ordinary function or a macro.
;;(defmethod glut:idle-func ((w window))
;;  (incf (x *ang*))
;;  (incf (y *ang*))
;;  (glut:post-redisplay))

;; CL-GLUT:KEYBOARD-FUNC already names an ordinary function or a macro.
;;(defmethod glut:keyboard-func ((w window) key x y)
;;  ;; if (key == 120) {exit if (glutGetModifiers() & GLUT_ACTIVE_ALT);
;;  (if (= key 120)
;;      (if (and (glut:get-modifiers) :active-alt)
	  (sb-ext:exit))))

(defmethod glut:special-func ((w window) key x y)
  (let ((delta 01))
    (if (= key :key-f5) (set-fullscreen))
    (if (= key :key-right) (incf (x *pos*) delta))
    (if (= key :key-left) (decf (x *pos*) delta))
    (if (= key :key-up) (incf (y *pos*) delta)))
    (if (= key :key-down) (decf (y *pos*) delta))
    (if (= key :key-end) (incf (z *pos*) delta))
    (if (= key :key-home) (decf (z *pos*) delta)))

(defmethod glut:mouse-func ((w window) button state x y)
  (if (= button :key-left) ;; {
      (if (= state :up) (setf *dragging* nil))
      (if (= state :down)
	  (setf *dragging* t)
	  (setf (x *init-ang*) x)
	  (setf (y *init-ang*) y)))
;;}
  )

(defmethod glut:passive-motion-func ((w window) x y)
  ;; do nothing
  )

(defmethod glut:motion-func ((w window) x y)
  (when dragging
    (incf (x *ang*) (- x (x *init-ang*)))
    (incf (y *ang*) (- y (y *init-ang*)))
    (setf (x *init-ang*) x)
    (setf (y *init-ang*) y)
    (glut:post-redisplay)))

;; CL-GLUT:IDLE-FUNC already names an ordinary function or a macro.
;;(defmethod glut:idle-func ((w window))
;;  (incf (x *ang*))
;;  (incf (y *ang*))
;;  (glut:post-redisplay))

(defmethod glut:create-menu ((w window) value)
  (if (= value :exit) (sb-ext:exit))
  (if (= value +full-screen+) (set-fullscreen)))

(defun show (msg)
(format t "~a~%" msg))

(let ((window (make-instance 'window)))
  (setf glut:*run-main-loop-after-display* nil)
  (show "Doing (glut:init)") (glut:init)
  (show "Doing (init-glscene") (init-glscene)
  (show "Doing (glut:display-window window") (glut:display-window window)
  (show "Doing (glut:add-menu-entry") (glut:add-menu-entry "Full Screen\tF5" +full-screen+)
  (show "Doing (glut:add-menu-entry") (glut:add-menu-entry "Exit\tAlt+x" +exit+)
  (show "Doing (glut:attach-menu") (glut:attach-menu :right-button)
  (show "Doing (glut:main-loop") (glut:main-loop))

;;--------------------------------------------------------------------
;;
;;  Hay dos formas de crear la ventana:
;;
;;  1. (setf window (glut:create-window "GLUT"))
;;  2. (setf window (make-instance 'window :title "GLUT"))
;;
;;  En la primera version si podemos hacer:
;;
;;  (glut:display-window window)
;;
;;  porque esa funcion si reconoce a la ventana creada por GLUT,
;;  pero no podemos hacer:
;;
;;  (defmethod glut:display ((w window)) ...)
;;
;;  ya que no es una instancia de la clase glut:window.
;;
;;  En la segunda version no podemos hacer:
;;
;;  (glut:display-window window)
;;
;;  porque window es una instancia de una clase y no objeto del tipo glut:create-window
;;  pero si podemos hacer:
;;
;;  (defmethod glut:display ((w window)) (glut:swap-buffers))
;;
;;  Se hara de la segunda forma y se deshabilitara el mensaje de error de que
;;  la instancia de la case no es una ventana valida con:
;;
;;  (setf *run-main-loop-after-display* nil)
;;
;;  tal como lo muestra el ejemplo de Gears con CLOS.
;;
;;--------------------------------------------------------------------

;;(glut:init)
;;(glut:display-window (glut:create-window "GLUT"))
;;(glut:display window)
;;(glut:main-loop)

;;(defclass flor (glut:window)
;;  ((ang :initform (make-instance 'point))
;;   (pos :initform (make-instance 'point))
;;   (init-pos :initform (make-instance 'point))
;;   (dragging :initform nil))
;;  (:default-initargs :title "GLUT Flor for Common Lisp" :mode '(:double :rgb :depth)))

;;(defmethod glut:display-window :before ((window flor))
;;  (with-slots (ang pos init-pos) window))

;;(defmethod glut:display ((window flor))
;;  (gl:clear :color-buffer :depth-buffer)
;;  (gl:load-identity)
;;  (with-slots (pos ang) window
;;    (gl:translate (slot-value pos 'x) (slot-value pos 'y) (slot-value pos 'z))
;;    (gl:rotate (slot-value ang 'x) 0.0 1.0 0.0)
;;    (gl:rotate (slot-value ang 'y) 1.0 0.0 0.0))
;;  ;;(if display-list-id (gl:call-list display-list-id))
;;  (glut:swap-buffers))

;;--------------------------------------------------------------------
;;
;; *** TODO ***
;;
;;  Somehow, you cannot actually declare the callback for the mouse
;;  motion a la CLOS way, accordingly with pjb@#lisp.FN you need to
;;  use CFFI for that with:
;;
;;  (cffi:defcallback my-callback …)
;;  (glut:motion-func (cffi:callback my-callback))
;;
;;  For more information about the topic:
;;
;;  CFFI - The Common Foreign Function Interface
;;
;;  https://common-lisp.net/project/cffi/

;;;(cffi:defcallback motion-func (format t "Hello, world!"))

;;;(glut:motion-func (cffi:callback motion-func))

;;(macroexpand '(defmethod glut:motion-func ((window flor) x y)
;;  (format t "Hello!~%")))

;;(defmethod glut:motion-func ((window flor) x y)
;;  (glut:post-redisplay)
;;  (with-slots (dragging ang init-ang) window
;;    (cond ((= dragging 1))
;;	  (if dragging
;;	      (progn
;;		;; $ang->x($ang->x + ($x - $init_ang->x)); 
;;		(setf (slot-value ang 'x) (+ (slot-value ang 'x) (- x (slot-value init-ang 'x))))
;;		(setf (slot-value ang 'y) (+ (slot-value ang 'y) (- y (slot-value init-ang 'y))))
;;		(setf (slot-value init-ang 'x) x)
;;		(setf (slot-value init-ang 'y) y)
;;		(glut:post-redisplay)
;;		(glut:post-redisplay))))))

;;(glut:motion-func #'(lambda (x y) (format t "Coords: (~d, ~d)~%" x y)))

;;(defmethod motion-func ((window flor) x y)
;;  (format t "Coords: (~d, ~d)~%" x y))

;;(glut:init)
;;(glut:display-window (make-instance 'flor))
;;(glut:main-loop)
