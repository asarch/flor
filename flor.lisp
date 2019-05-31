(ql:quickload :cl-glu)
(ql:quickload :cl-glut)

(defclass point ()
  ((x :initarg :x :initform 0.0 :accessor x :documentation "x-axis value")
   (y :initarg :y :initform 0.0 :accessor y :documentation "y-axis value")
   (z :initarg :z :initform 0.0 :accessor z :documentation "z-axis value")))

;; TODO: Set all these parameters as class slots
(defconstant +exit+ 0)
(defconstant +fullscreen+ 1)

(defparameter *dragging* nil)
(defparameter *anti-alias* nil)
(defparameter *lighting* nil)
(defparameter *fullscreen* nil)
(defparameter *display-list-id* nil)

(defparameter *pos* (make-instance 'point :z (- 3.0)))
(defparameter *ang* (make-instance 'point))
(defparameter *init-ang* (make-instance 'point))

(defun axis (size)
  (gl:begin :lines)
  (gl:color 1.0 0.0 0.0) (gl:vertex (- size) 0.0 0.0) (gl:vertex size 0.0 0.0)
  (gl:color 0.0 1.0 0.0) (gl:vertex 0.0 (- size) 0.0) (gl:vertex 0.0 size 0.0)
  (gl:color 0.0 0.0 1.0) (gl:vertex 0.0 0.0 (- size)) (gl:vertex 0.0 0.0 size)
  (gl:end))

(defun set-fullscreen ()
  (cond ((null *fullscreen*)
	 (glut:full-screen)
	 (setf *fullscreen* t))))

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
    
    (setf *display-list-id* (gl:gen-lists 1))
    
    (cond ((> *display-list-id* 0)
	   (gl:new-list *display-list-id* :compile)
	   (gl:begin :lines)
	   
	   (loop for i from 0 to (- points 1) do
	     (loop for j from i to (- points 1) do
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
  
  (if (not (null *display-list-id*))
      (gl:call-list *display-list-id*))
  
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
(defmethod glut:idle ((w window))
  (incf (x *ang*))
  (incf (y *ang*))
  (glut:post-redisplay))

;; CL-GLUT:KEYBOARD-FUNC already names an ordinary function or a macro.
;;(defmethod glut:keyboard ((w window) key x y)
  ;; if (key == 120) {exit if (glutGetModifiers() & GLUT_ACTIVE_ALT);
;;  (if (= key 120)
;;      (if (and (glut:get-modifiers) :active-alt)
;;	  (sb-ext:exit))))

;; CL-GLUT:SPECIAL-FUNC already names an ordinary function or a macro.
(defmethod glut:special ((w window) key x y)
  (let ((delta 0.1))
    ;; In C there is GLUT_KEY_F5, in Common Lisp it is
    ;; :number::key-f5, or just ::key-f5
    ;; caught WARNING: Constant :KEY-F5 conflicts with its asserted type NUMBER.
    ;; See also: The SBCL Manual, Node "Handling of Types"
    (if (eq key :key-f5) (set-fullscreen))
    
    (if (eq key :key-right) (incf (x *pos*) delta))
    
    (if (eq key :key-left) (decf (x *pos*) delta))
    
    (if (eq key :key-up) (incf (y *pos*) delta))
  
    (if (eq key :key-down) (decf (y *pos*) delta))
    
    (if (eq key :key-end) (incf (z *pos*) delta))
    
    (if (eq key :key-home) (decf (z *pos*) delta))))

;; CL-GLUT:MOUSE-FUNC already names an ordinary function or a macro.
(defmethod glut:mouse ((w window) button state x y)
  (cond ((eq button :left-button)
	 (if (eq state :up) (setf *dragging* nil))
	 
	 (cond ((eq state :down)
		(setf *dragging* t)
		(setf (x *init-ang*) x)
		(setf (y *init-ang*) y))))))

;; CL-GLUT:PASSIVE-MOTION-FUNC already names an ordinary function or a macro.
(defmethod glut:passive-motion ((w window) x y)
  ;; do nothing
  )

;; CL-GLUT:MOTION-FUNC already names an ordinary function or a macro.
(defmethod glut:motion ((w window) x y)
  (when *dragging*
    (incf (x *ang*) (- x (x *init-ang*)))
    (incf (y *ang*) (- y (y *init-ang*)))
    (setf (x *init-ang*) x)
    (setf (y *init-ang*) y)
    (glut:post-redisplay)))

(let ((window (make-instance 'window)))
  (setf glut:*run-main-loop-after-display* nil)
  (glut:init)
  (glut:display-window window)
  (init-glscene)

  ;; TODO: You cannot create menus because of this bug:
  ;; https://github.com/3b/cl-opengl/issues/83#issuecomment-497723662
  ;;(show "Doing (glut:create-menu)")
  ;;(glut:create-menu menu-status)
  ;;(glut:add-menu-entry "Full Screen\tF5" +fullscreen+)
  ;;(glut:add-menu-entry "Exit\tAlt+x" +exit+)
  ;;(glut:attach-menu :right-button)
  
  (glut:main-loop))
