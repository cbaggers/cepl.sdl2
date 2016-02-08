(in-package :cepl.sdl2)

(defvar *initd* nil)

(defmethod cepl.host:init ()
  (setf *backend* :sdl)
  (unless *initd*
    (unless (sdl2:init :everything) (error "Failed to initialise SDL"))
    (setf *initd* t)))

(defmethod cepl.host:request-context
    (width height title fullscreen
     no-frame alpha-size depth-size stencil-size
     red-size green-size blue-size buffer-size
     double-buffer hidden resizable)
  "Initializes the backend and returns a list containing: (context window)"
  (setf *backend* :sdl)
  (let ((win (sdl2:create-window
              :title title :w width :h height
              :flags (remove nil `(:shown :opengl
                                          ,(when fullscreen :fullscreen-desktop)
                                          ,(when resizable :resizable)
                                          ,(when no-frame :borderless)
                                          ,(when hidden :hidden))))))
    #+darwin
    (progn
      (setf cl-opengl-bindings::*gl-get-proc-address* #'sdl2::gl-get-proc-address)
      (sdl2:gl-set-attr :context-major-version 4)
      (sdl2:gl-set-attr :context-minor-version 1)
      (sdl2:gl-set-attr :context-profile-mask sdl2-ffi::+SDL-GL-CONTEXT-PROFILE-CORE+))
    (sdl2:gl-set-attr :context-profile-mask 1)
    (sdl2:gl-set-attr :alpha-size alpha-size)
    (sdl2:gl-set-attr :depth-size depth-size)
    (sdl2:gl-set-attr :stencil-size stencil-size)
    (sdl2:gl-set-attr :red-size red-size)
    (sdl2:gl-set-attr :green-size green-size)
    (sdl2:gl-set-attr :blue-size blue-size)
    (sdl2:gl-set-attr :buffer-size buffer-size)
    (sdl2:gl-set-attr :doublebuffer (if double-buffer 1 0))
    (let ((contex (sdl2:gl-create-context win)))
      (sdl2:gl-make-current win contex)
      (list contex win))))

(defmethod cepl.host:shutdown ()
  (sdl2:quit))

(defun sdl-swap (handle)
  (sdl2::sdl-gl-swap-window handle))

(defmethod get-step-func ()
  #'collect-sdl-events)

(defmethod get-swap-func ()
  #'sdl-swap)

(defmethod set-primary-thread-and-run (func &rest args)
  (sdl2:make-this-thread-main (lambda () (apply func args))))

;;----------------------------------------------------------------------
;; event stub

;; {TODO} optimize
(let ((sdl->lisp-time-offset 0))
  (defun set-sdl->lisp-time-offset ()
    (setf sdl->lisp-time-offset (cl:- (get-internal-real-time) (sdl2::get-ticks))))
  (defun sdl->lisp-time (sdl-time)
    (when (= sdl->lisp-time-offset 0)
      (set-sdl->lisp-time-offset))
    (cl:+ sdl-time sdl->lisp-time-offset))
  (defun lisp->sdl-time (lisp-time)
    (when (= sdl->lisp-time-offset 0)
      (set-sdl->lisp-time-offset))
    (cl:- lisp-time sdl->lisp-time-offset)))


(defmacro %case-events ((event &key (method :poll) (timeout nil))
                        &body event-handlers)
  `(let (,(when (symbolp event) `(,event (sdl2:new-event))))
     (loop :until (= 0  (sdl2:next-event ,event ,method ,timeout)) :do
        (case (sdl2::get-event-type ,event)
          ,@(loop :for (type params . forms) :in event-handlers
               :append (let ((type (if (listp type)
                                       type
                                       (list type))))
                         (loop :for typ :in type :collect
                            (sdl2::expand-handler event typ params forms)))
               :into results
               :finally (return (remove nil results)))))
     (sdl2:free-event ,event)))

(defun collect-sdl-events ()
  (%case-events (event)
    (:quit (:timestamp ts)
           (format nil "should quit ~s" (sdl->lisp-time ts)))
    (:windowevent (:timestamp ts :event e :data1 x :data2 y)
                  (format nil "window-event ~s ~s (v! ~s ~s)"
                          (sdl->lisp-time ts) e x y))))
