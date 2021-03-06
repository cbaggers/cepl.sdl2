(in-package :cepl.sdl2)

(defvar *initd* nil)

;;======================================================================
;; api v2

(defgeneric sdl-init (&rest init-flags)
  (:method (&rest init-flags)
    (let ((flags (or init-flags :everything)))
      (unless (sdl2:was-init :everything)
        (init-sdl2-low-level flags)
        (setf *initd* t)))))

(defun init-sdl2-low-level (&rest sdl-init-flags)
  (let ((init-flags (autowrap:mask-apply 'sdl2::sdl-init-flags sdl-init-flags)))
    (sdl2::check-rc (sdl2::sdl-init init-flags))))

;;----------------------------------------------------------------------

(defun sdl-shutdown ()
  (low-level-quit))

(defun low-level-quit ()
  (sdl2::sdl-quit)
  (setf sdl2::*main-thread-channel* nil)
  (setf sdl2::*lisp-message-event* nil)
  (let ((init-flags (autowrap:mask 'sdl2::sdl-init-flags :everything)))
    (sdl2::check-rc (sdl2::sdl-init init-flags))))

;;----------------------------------------------------------------------

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

(let ((listeners nil))
  ;;
  (defun sdl-register-listener (func)
    (unless (find func listeners)
      (push func listeners)))
  ;;
  (defun sdl-step-v1 (surface)
    (declare (ignore surface))
    (%case-events (event)
      (otherwise () (loop :for listener :in listeners :do (funcall listener event))))))

;;----------------------------------------------------------------------

(defun sdl-swap (handle)
  (sdl2::sdl-gl-swap-window handle))

;;----------------------------------------------------------------------

(defun make-sdl-context (surface version double-buffer
                         alpha-size depth-size stencil-size buffer-size
                         red-size green-size blue-size)
  (setf cl-opengl-bindings::*gl-get-proc-address* #'sdl2::gl-get-proc-address)
  (sdl2:gl-set-attr :alpha-size alpha-size)
  (sdl2:gl-set-attr :depth-size depth-size)
  (when stencil-size
    (sdl2:gl-set-attr :stencil-size stencil-size))
  (sdl2:gl-set-attr :red-size red-size)
  (sdl2:gl-set-attr :green-size green-size)
  (sdl2:gl-set-attr :blue-size blue-size)
  (sdl2:gl-set-attr :buffer-size buffer-size)
  (sdl2:gl-set-attr :doublebuffer (if double-buffer 1 0))
  (sdl2:gl-set-attr :share-with-current-context 0)
  ;;
  (let ((context (if version
                     (create-context-by-version surface version)
                     (search-for-context surface))))
    (assert context ()
            "CEPL.SDL2: Could not find a suitable context for CEPL.
Your machine must support at least GL 3.3")
    context))

(defun make-shared-sdl-context (current-gl-context surface version double-buffer
                                alpha-size depth-size stencil-size buffer-size
                                red-size green-size blue-size)
  (declare (ignorable current-gl-context))
  (setf cl-opengl-bindings::*gl-get-proc-address* #'sdl2::gl-get-proc-address)
  (sdl2:gl-set-attr :alpha-size alpha-size)
  (sdl2:gl-set-attr :depth-size depth-size)
  (when stencil-size
    (sdl2:gl-set-attr :stencil-size stencil-size))
  (sdl2:gl-set-attr :red-size red-size)
  (sdl2:gl-set-attr :green-size green-size)
  (sdl2:gl-set-attr :blue-size blue-size)
  (sdl2:gl-set-attr :buffer-size buffer-size)
  (sdl2:gl-set-attr :doublebuffer (if double-buffer 1 0))
  (sdl2:gl-set-attr :share-with-current-context 1)
  ;;
  (let ((context (if version
                     (create-context-by-version surface version)
                     (search-for-context surface))))
    (assert context ()
            "CEPL.SDL2: Could not find a suitable context for CEPL.
Your machine must support at least GL 3.3")
    (values context surface)))

(defvar *core-context* t)

(defun create-context-by-version (surface version)
  (destructuring-bind (&optional major minor)
      (cepl.context:split-float-version version)
    (sdl2:gl-set-attr :context-profile-mask
                      (if *core-context*
                          sdl2-ffi::+sdl-gl-context-profile-core+
                          sdl2-ffi::+sdl-gl-context-profile-compatibility+))
    (sdl2:gl-set-attr :context-major-version major)
    (sdl2:gl-set-attr :context-minor-version minor)
    (sdl2:gl-create-context surface)))

(defun search-for-context (surface)
  (let ((p (if *core-context*
               sdl2-ffi::+sdl-gl-context-profile-core+
               sdl2-ffi::+sdl-gl-context-profile-compatibility+))
        context)
    (loop :for (major minor core) :in `((4 6 ,p) (4 5 ,p) (4 4 ,p) (4 3 ,p)
                                        (4 2 ,p) (4 1 ,p) (4 0 ,p) (3 3 ,p))
       :until context
       :do (handler-case
               (progn
                 ;; (print (list :> major minor (= core +)))
                 (sdl2:gl-set-attr :context-profile-mask core)
                 (sdl2:gl-set-attr :context-major-version major)
                 (sdl2:gl-set-attr :context-minor-version minor)
                 (setf context (sdl2:gl-create-context surface)))
             (error ())))
    context))


(defun sdl-make-current (context surface)
  (sdl2:gl-make-current surface context))

;;----------------------------------------------------------------------

(defun make-sdl-surface (width height title fullscreen
                         no-frame alpha-size depth-size stencil-size
                         red-size green-size blue-size buffer-size
                         double-buffer hidden resizable)
  (declare (ignore alpha-size depth-size stencil-size buffer-size double-buffer
                   red-size green-size blue-size))
  (let ((surface
         (sdl2:create-window
          :title title :w width :h height
          :flags (remove nil `(:shown :opengl
                                      ,(when fullscreen :fullscreen-desktop)
                                      ,(when resizable :resizable)
                                      ,(when no-frame :borderless)
                                      ,(when hidden :hidden))))))
    #+windows ; hack to fix CEPL hangup on Windows under SLIME
    (progn
      (sdl2:hide-window surface)
      (sdl2:show-window surface))
    (when hidden
      (sdl2:hide-window surface))
    surface))

(defun destroy-sdl-surface (surface)
  (sdl2:destroy-window surface))

(defun sdl-surface-size (win-handle)
  (multiple-value-list (sdl2:get-window-size win-handle)))

(defun sdl-set-surface-size (win-handle width height)
  (sdl2:set-window-size win-handle width height))

(defun sdl-surface-fullscreen-p (surface)
  (not (null (intersection '(:fullscreen-desktop :fullscreen)
                           (sdl2:get-window-flags surface)))))

(defun sdl-set-surface-fullscreen (surface state)
  (sdl2:set-window-fullscreen surface state))

(defun sdl-surface-title (surface)
  (values (sdl2:get-window-title surface)))

(defun sdl-set-surface-title (surface title)
  (sdl2:set-window-title surface title))

;;----------------------------------------------------------------------

(defclass sdl-api (cepl.host:api-2)
  (;;
   (supports-multiple-contexts-p :initform nil)
   ;;
   (supports-multiple-surfaces-p :initform t)
   ;;
   (init-function :initform #'sdl-init)
   ;;
   (shutdown-function :initform #'sdl-shutdown)
   ;;
   (make-surface-function :initform #'make-sdl-surface)
   ;;
   (destroy-surface-function :initform #'destroy-sdl-surface)
   ;;
   (make-context-function :initform #'make-sdl-context)
   ;;
   (step-function :initform #'sdl-step-v1)
   ;;
   (register-event-callback-function :initform #'sdl-register-listener)
   ;;
   (swap-function :initform #'sdl-swap)
   ;;
   (surface-size-function :initform #'sdl-surface-size)
   ;;
   (make-context-current-function :initform #'sdl-make-current)
   ;;
   (set-surface-size-function :initform #'sdl-set-surface-size)
   ;;
   (surface-fullscreen-p-function :initform #'sdl-surface-fullscreen-p)
   ;;
   (set-surface-fullscreen-function :initform #'sdl-set-surface-fullscreen)
   ;;
   (surface-title-function :initform #'sdl-surface-title)
   ;;
   (set-surface-title-function :initform #'sdl-set-surface-title)
   ;;
   (make-gl-context-shared-with-current-context-function
    :initform #'make-shared-sdl-context)))

(register-host 'sdl-api)

;;----------------------------------------------------------------------

(defun vsync ()
  (let ((val (sdl2-ffi.functions:sdl-gl-get-swap-interval)))
    (cond
      ((= val -1) :unsupported)
      ((= val 0) nil)
      ((= val 1) t))))

(defun (setf vsync) (boolean)
  (if (eq :unsupported (vsync))
      (warn "Sorry setting vsync is not supported")
      (sdl2-ffi.functions:sdl-gl-set-swap-interval (if boolean 1 0)))
  boolean)
