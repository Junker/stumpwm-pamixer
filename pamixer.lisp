;;;; pamixer.lisp

(in-package :pamixer)

;; formatters.
(add-screen-mode-line-formatter #\P 'pm-modeline)

(defparameter *step* 5)
(defparameter *allow-boost* nil)
(defparameter *source-allow-boost* nil)

(defparameter *modeline-fmt* "%b(%v)"
  "The default value for displaying pamixer information on the modeline")

(defparameter *formatters-alist*
  '((#\b  ml-bar)
    (#\v  ml-volume)))

(defparameter *mixer-command* "pavucontrol")

(defun run (args &optional (wait-output nil))
  (run-shell-command (concat "pamixer " args) wait-output))

(defun run-sink (args &optional (wait-output nil))
  (run (concat (if *allow-boost* "--allow-boost ") args) wait-output))

(defun run-source (args &optional (wait-output nil))
  (run (concat "--default-source " (if *source-allow-boost* "--allow-boost ") args) wait-output))

(defun volume-up (step)
  (run-sink (format nil "-i ~d" step)))

(defun volume-down (step)
  (run-sink (format nil "-d ~d" step)))

(defun set-volume (value)
  (run-sink (format nil "--set-volume ~d" value)))

(defun get-volume ()
  (parse-integer (run-sink "--get-volume" t) :junk-allowed t))

(defun get-mute ()
  (let ((muted_str (string-trim '(#\Newline) (run-sink "--get-mute" t))))
    (string-equal muted_str "true")))

(defun unmute ()
  (run-sink "-u"))

(defun mute ()
  (run-sink "-m"))

(defun toggle-mute ()
  (run-sink "-t"))

(defun source-volume-up (step)
  (run-source (format nil "-i ~d" step)))

(defun source-volume-down (step)
  (run-source (format nil "-d ~d" step)))

(defun source-set-volume (value)
  (run-source (format nil "--set-volume ~d" value)))

(defun source-get-volume ()
  (parse-integer (run-source "--get-volume" t)) :junk-allowed t)

(defun source-get-mute ()
  (let ((muted_str (string-trim '(#\Newline) (run-source "--get-mute" t))))
    (string-equal muted_str "true")))

(defun source-unmute ()
  (run-source "-u"))

(defun source-mute ()
	(run-source "-m"))

(defun source-toggle-mute ()
	(run-source "-t"))

(defun open-mixer ()
  (run-shell-command *mixer-command*))

(defun ml-bar (volume muted)
  (concat "\["
          (stumpwm:bar (if muted 0 (min 100 volume)) 5 #\X #\=)
          "\]"))

(defun ml-volume (volume muted)
  (if muted "MUT" (format nil "~a\%" volume)))

(defun pm-modeline (ml)
  (declare (ignore ml))
  (format-with-on-click-id
   (format-expand *formatters-alist*
                  *modeline-fmt*
                  (get-volume) (get-mute))
   :ml-pamixer-on-click-change-volume nil))

(defun ml-on-click-change-volume (code id &rest rest)
  (declare (ignore rest))
  (declare (ignore id))
  (let ((button (stumpwm::decode-button-code code)))
    (case button
      ((:left-button)
       (toggle-mute))
      ((:right-button)
       (open-mixer))
      ((:wheel-up)
       (volume-up *step*))
      ((:wheel-down)
       (volume-down *step*))))
  (stumpwm::update-all-mode-lines))

(register-ml-on-click-id :ml-pamixer-on-click-change-volume
                         #'ml-on-click-change-volume)

(defcommand pamixer-volume-up () ()
  "Increase the volume by N points"
  (volume-up *step*))

(defcommand pamixer-volume-down () ()
  "Decrease the volume by N points"
  (volume-down *step*))

(defcommand pamixer-mute () ()
  "Mute"
  (mute))

(defcommand pamixer-unmute () ()
  "Unmute"
  (unmute))

(defcommand pamixer-toggle-mute () ()
  "Toggle Mute"
  (toggle-mute))

(defcommand pamixer-set-volume (value) ((:string "Volume percentage:"))
  "Set volume"
  (set-volume value))

(defcommand pamixer-source-volume-up () ()
  "Increase the volume by N points"
  (source-volume-up *step*))

(defcommand pamixer-source-volume-down () ()
  "Decrease the volume by N points"
  (source-volume-down *step*))

(defcommand pamixer-source-mute () ()
  "Source mute"
  (source-mute))

(defcommand pamixer-source-unmute () ()
  "Source unmute"
  (source-unmute))

(defcommand pamixer-source-toggle-mute () ()
  "Toggle source Mute"
  (source-toggle-mute))

(defcommand pamixer-source-set-volume (value) ((:string "Volume percentage:"))
  "Set source volume"
  (source-set-volume value))
