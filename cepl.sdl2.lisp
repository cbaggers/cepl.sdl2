(in-package :cepl.sdl2)

(defvar *initd* nil)

(defmethod cepl.host:init (&optional (init-flags :everything))
  (unless *initd*
    ;;(unless (sdl2:init :everything) (error "Failed to initialise SDL"))
    (init-sdl2-low-level init-flags)
    (setf *initd* t)))

(defun init-sdl2-low-level (&rest sdl-init-flags)
  (let ((init-flags (autowrap:mask-apply 'sdl2::sdl-init-flags sdl-init-flags)))
    (sdl2::check-rc (sdl2::sdl-init init-flags))))

(defmethod cepl.host:request-context
    (width height title fullscreen
     no-frame alpha-size depth-size stencil-size
     red-size green-size blue-size buffer-size
     double-buffer hidden resizable gl-version)
  "Initializes the backend and returns a list containing: (context window)"
  (let ((win (sdl2:create-window
              :title title :w width :h height
              :flags (remove nil `(:shown :opengl
                                          ,(when fullscreen :fullscreen-desktop)
                                          ,(when resizable :resizable)
                                          ,(when no-frame :borderless)
                                          ,(when hidden :hidden))))))
    (destructuring-bind (&optional major minor)
        (when gl-version (cepl.context:split-float-version gl-version))
      #+windows ; hack to fix CEPL hangup on Windows under SLIME
      (progn
        (sdl2:hide-window win)
        (sdl2:show-window win)
        (when hidden
          (sdl2:hide-window win)))
      #+darwin
      (progn
        (setf cl-opengl-bindings::*gl-get-proc-address* #'sdl2::gl-get-proc-address)
        (sdl2:gl-set-attr :context-major-version (or major 4))
        (sdl2:gl-set-attr :context-minor-version (or minor 1))
        (sdl2:gl-set-attr :context-profile-mask sdl2-ffi::+SDL-GL-CONTEXT-PROFILE-CORE+))
      #-darwin
      (when gl-version
        (sdl2:gl-set-attr :context-major-version major)
        (sdl2:gl-set-attr :context-minor-version minor))
      (handler-case (sdl2:gl-set-attr :context-profile-mask 1)
        (error ()))
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
        (list contex win)))))

(defmethod cepl.host:shutdown ()
  (low-level-quit))

(defun low-level-quit ()
  (sdl2::sdl-quit)
  (setf sdl2::*main-thread-channel* nil)
  (setf sdl2::*lisp-message-event* nil))

(defun sdl-swap (handle)
  (sdl2::sdl-gl-swap-window handle))

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

(defun collect-sdl-events (win)
  (declare (ignore win))
  (%case-events (event)
    (:quit () (cepl.host:shutdown))))

;;----------------------------------------------------------------------
;; tell cepl what to use

(set-step-func #'collect-sdl-events)
(set-swap-func #'sdl-swap)
