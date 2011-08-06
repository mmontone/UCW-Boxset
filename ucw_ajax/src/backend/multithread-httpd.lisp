;; -*- lisp -*-

(in-package :it.bese.ucw)

;; TODO the messaging stuff should be part of another lib, probably bordeaux-threads.

;;;
;;; A simple message queue implementation
;;;
(defclass message-queue ()
  ((name
    :initform "A message queue"
    :accessor name-of
    :initarg :name)
   (lock
    :initform (make-lock "message queue")
    :accessor lock-of)
   (messages
    :initform '()
    :accessor messages-of)
   (message-received-condition
    :initform (make-condition-variable)
    :accessor message-received-condition-of)))

(defmethod initialize-instance :after ((self message-queue) &key &allow-other-keys)
  (setf (lock-of self) (make-lock (format nil "Lock for ~S" (name-of self)))))

(defun make-message-queue (&rest initargs)
  (apply #'make-instance 'message-queue initargs))

(defmacro with-lock-held-on-message-queue (queue &body body)
  (rebinding (queue)
    `(with-lock-held ((lock-of ,queue))
      (ucw.backend.dribble "Entering with-lock-held-on-message-queue for queue ~A in thread ~A" ,queue (current-thread))
      (multiple-value-prog1
          ,@body
        (ucw.backend.dribble "Leaving with-lock-held-on-message-queue for queue ~A in thread ~A" ,queue (current-thread))))))

(defgeneric receive-message (message-queue))

(defgeneric send-message (message message-queue))

(defmethod receive-message ((queue message-queue))
  (ucw.backend.dribble "Receiving message from queue ~A in thread ~A" queue (current-thread))
  (with-lock-held-on-message-queue queue
    (multiple-value-prog1
        (loop
          (if (messages-of queue)
              (return (pop (messages-of queue)))
              (progn
                (ucw.backend.dribble "No message in queue ~A, condition-wait in thread ~A" queue (current-thread))
                (condition-wait (message-received-condition-of queue)
                                (lock-of queue)))))
      (ucw.backend.dribble "Returning a received message from queue ~A in thread ~A" queue (current-thread)))))

(defmethod send-message (message (queue message-queue))
  (ucw.backend.dribble "Sending message to queue ~A from thread ~A" queue (current-thread))
  (with-lock-held-on-message-queue queue
    (setf (messages-of queue) (nconc (messages-of queue) (list message)))
    (ucw.backend.dribble "Notifying message-received-condition of queue ~A from thread ~A" queue (current-thread))
    (condition-notify (message-received-condition-of queue)))
  (ucw.backend.dribble "Done sending message to queue ~A from thread ~A" queue (current-thread))
  (values))

(defmacro send (thread &rest message-contents)
  (rebinding (thread)
    (with-unique-names (message)
      `(let ((,message (list ,@message-contents)))
         (ucw.backend.dribble "Sending ~S to ~S from thread ~A" ,message ,thread (current-thread))
         (send-message ,message ,thread)))))

(defmacro receive (thread &body message-match-clauses)
  (with-unique-names (message)
    (rebinding (thread)
      `(let ((,message (receive-message ,thread)))
        (ucw.backend.dribble "~S received ~S" ,thread ,message)
        (list-match-case ,message
          ,@message-match-clauses
          (?_
           (error "Unknown message ~S received by ~S" ?_ ,thread)))))))


(defclass multithread-httpd-backend (httpd-backend)
  ((controller
    :initform nil
    :accessor controller-of)
   (acceptor
    :initform nil
    :accessor acceptor-of)
   (workers
    :initform '()
    :accessor workers-of)
   (available-workers
    :initform '()
    :accessor available-workers-of)
   (worker-count
    :initform 0
    :accessor worker-count-of
    :documentation "The number of workers to
initially create and once the server is running how many are
currently hanging around.")
   (maximum-worker-count
    :initform 32
    :accessor maximum-worker-count-of
    :initarg :maximum-worker-count
    :documentation "The upper limit for the number of workers.")
   (request-timeout
    :initform 90
    :accessor request-timeout-of
    :documentation "Drop requests that can't be served in this time (measured in seconds)."))
  (:documentation "Generic multithreaded backend.

2 main threads:

 * ACCEPTOR sits in a tight loop waiting for incoming
   connections on the backend's socket. When it gets a connection
   it sends the CONNECTION message to control thread with the
   newly created stream.

 * CONTROLLER sits and reacts to various messages:

   1) SHUTDOWN - sends STOP messages to the workers and then
      closes all the sockets and streams.

   2) CONNECTION - Removes a worker from the pool and sends it a
      CONNECTION message. A new worker will be spawned if none
      are available up to MAXIMUM-WORKER-COUNT.  When the worker is
      finished it sends a WORKER-DONE message.

   3) WORKER-DONE - A worker has finished processing a request
      and will be restored to the pool.

   4) ERROR - A worker has encountered an error.

Seperate lists are kept for all workers for use when shutting and
of the available-workers for handling request."))

(defclass pending-request ()
  ((socket :accessor socket :initarg :socket :documentation "The return value of usocket:socket-accept.")
   (request-start-time :accessor request-start-time :initform (get-universal-time)))
  (:documentation "Used to keep track of how old an unallocated request is."))

(defclass thread-with-message-queue-mixin ()
  ((message-queue
    :initform (make-message-queue)
    :accessor message-queue-of)
   (thread
    :accessor thread-of
    :initarg :thread)))

(defprint-object (self thread-with-message-queue-mixin)
  (write-string (thread-name (thread-of self))))

(defmethod send-message (message (self thread-with-message-queue-mixin))
  (send-message message (message-queue-of self)))

(defmethod receive-message ((self thread-with-message-queue-mixin))
  (receive-message (message-queue-of self)))

(defclass httpd-worker (thread-with-message-queue-mixin)
  ())

(defun make-httpd-worker (backend)
  (let ((worker (make-instance 'httpd-worker)))
    (setf (thread-of worker)
          (make-thread (lambda ()
                         (httpd-worker-loop backend worker))
                       :name (format nil "httpd worker ~a" (worker-count-of backend))))
    (push worker (workers-of backend))
    (incf (worker-count-of backend))
    (ucw.backend.info "Spawned new worker thread ~A" worker)
    worker))

(defclass httpd-controller (thread-with-message-queue-mixin)
  ())

(defun make-httpd-controller (backend)
  (assert (not (controller-of backend)))
  (let ((controller (make-instance 'httpd-controller)))
    (setf (controller-of backend) controller)
    (setf (thread-of controller)
          (make-thread (lambda ()
                         (httpd-controller-loop backend))
                       :name "httpd controller"))
    controller))

(defclass httpd-acceptor (thread-with-message-queue-mixin)
  ())

(defun make-httpd-acceptor (backend)
  (assert (not (acceptor-of backend)))
  (let ((acceptor (make-instance 'httpd-acceptor)))
    (setf (acceptor-of backend) acceptor)
    (setf (thread-of acceptor)
          (make-thread (lambda ()
                         (block accepting
                           (handler-bind ((serious-condition
                                           (lambda (error)
                                             (ucw.backend.error "Error in accept thread, starting a new one. The error is~%~A" error)
                                             (when (debug-on-error nil)
                                               (with-simple-restart
                                                   (continue "Exit this thread and start a new acceptor thread")
                                                 (invoke-slime-debugger-if-possible error)))
                                             (setf (acceptor-of backend) nil)
                                             (make-httpd-acceptor backend)
                                             (return-from accepting))))
                             (httpd-accept-loop backend))))
                       :name "httpd acceptor"))
    acceptor))

;;;; The multi thread server

(defun next-available-worker! (backend)
  "Get the next available worker from the backend. If none are
available but we aren't above the max-workers then allocate a new
one."
  (if (available-workers-of backend)
      (pop (available-workers-of backend))
      (when (< (worker-count-of backend)
               (maximum-worker-count-of backend))
        (ucw.backend.dribble "Spawning new worker thread because there weren't enough available")
        (make-httpd-worker backend))))

(defgeneric pending-request-valid-p (backend pending-request))

(defmethod pending-request-valid-p ((backend multithread-httpd-backend)
                                    (pending-request pending-request))
  "Returns T if PENDING-REQUEST is young enough to still be servicable.

This is calculated by comparing the time at which the request
came in with BACKEND's REQUEST-TIMEOUT."
  (< (- (get-universal-time) (request-start-time pending-request))
     (request-timeout-of backend)))

(defun allocate-worker (backend pending-request)
  ;; if we have a worker, tell it to handle the request,
  ;; otherwise requeue the request in the control thread and hopefully
  ;; in the meantime a worker will finish up.
  (if-bind worker (next-available-worker! backend)
    (send worker 'connection pending-request)
    ;; safeguard against a request being requeued too many times.
    (if (pending-request-valid-p backend pending-request)
        (send (controller-of backend) 'connection pending-request) ; requeue
        (progn
          (usocket:socket-close (socket pending-request))
          (ucw.backend.error "Incoming request dropped because no workers were available for ~A seconds."
                             (request-timeout-of backend))))))

(defun httpd-controller-loop (backend)
  (assert (controller-of backend))
  (loop
    (receive (controller-of backend)
      ((shutdown)
       (shutdown-multithreaded-httpd-backend backend)
       (return-from httpd-controller-loop))
      ((start) (return))))
  
  (assert (acceptor-of backend))
  ;; the slot on the backend may get cleared, cache them at start
  (let ((controller (controller-of backend))
        (acceptor (acceptor-of backend)))
    (ucw.backend.debug "Controller is up, starting the acceptor")

    (send acceptor 'start)

    (flet ((control-loop-error (condition)
             (when *debug-on-error*
               (invoke-slime-debugger-if-possible condition))
             (ucw.backend.error "There was an error in the control loop that was ignored because there is no debugger attached:~%~A"
                                condition)
             (continue)))
      (loop
        (handler-bind ((error #'control-loop-error))
          (restart-case
              (receive controller
                ((shutdown)
                 (shutdown-multithreaded-httpd-backend backend)
                 (return-from httpd-controller-loop))

                ((connection ?pending-request)
                 (allocate-worker backend ?pending-request))

                ((worker-done ?worker)
                 (push ?worker (available-workers-of backend)))

                ((unregister-worker ?worker)
                 (ucw.backend.error "Worker ~A is being removed due to an error" ?worker)
                 (setf (workers-of backend) (remove ?worker (workers-of backend)))
                 (decf (worker-count-of backend))))
           (continue ()
             :report "Ignore the error and continue processing."
             nil)
           (kill-control-thread ()
             :report "Return from httpd-controller-loop immediately."
             (return-from httpd-controller-loop))
           (shutdown-backend (&optional (force nil))
             :report "Send the backend a shutdown message."
             (make-thread (lambda ()
                            (shutdown-backend backend :force force))
                          :name "shutdown-backend thread")
             nil)))))))

(defun httpd-worker-loop (backend worker)
  (assert (controller-of backend))
  ;; the slot on the backend may get cleared, cache them at start
  (let ((controller (controller-of backend)))
    (restart-case
         (loop
           (receive worker
             ((stop) (return))
             ((connection ?pending-request)
              (let ((stream-socket (socket ?pending-request)))
                (flet ((serve-one-request ()
                         (unwind-protect
                              (let* ((*request* (read-request backend stream-socket))
                                     (*response* (make-response *request*)))
                                (handle-request backend *request* *response*)
                                (close-request *request*))
                           (ignore-errors
                             (usocket:socket-close stream-socket))))
                       (handle-request-error (condition)
                         (unless (typep condition 'no-handler-for-request)
                           (ucw.backend.error "Error while handling a httpd backend request in worker ~A on socket ~A:~%~A"
                                              worker stream-socket condition))
                         (handle-toplevel-condition nil condition nil)
                         (ucw.backend.error "Should not get here, removing this worker...")
                         (return)))
                  (ucw.backend.dribble "Worker ~A is processing a request" worker)
                  (call-as-backend-request-handler #'serve-one-request
                                                   :error-handler #'handle-request-error)
                  (ucw.backend.dribble "Worker ~A finished processing a request, sending 'worker-done" worker)
                  ;; only send worker-done after everything else is complete
                  ;; (closing the stream) and there haven't been any errors.
                  (send controller 'worker-done worker))))))
     (remove-worker ()
       :report (lambda (stream)
                 (format stream "Stop and remove worker ~A" worker))
       (values)))
  (ucw.backend.dribble "Worker ~A is going away" worker)
  (send controller 'unregister-worker worker)))

(defun httpd-accept-loop (backend)
  (assert (and (acceptor-of backend) (controller-of backend)))
  ;; the slot on the backend may get cleared, cache them at start
  (let ((acceptor (acceptor-of backend))
        (controller (controller-of backend)))
    (loop
      (receive acceptor
        ((start) (return))))

    (ucw.backend.debug "Acceptor is up, starting to accept connections")

    (iter (for stream-socket = (usocket:socket-accept
                                (socket backend) :element-type '(unsigned-byte 8)))
          (while (socket backend))
          (if (or (available-workers-of backend)
                  (< (length (workers-of backend))
                     (maximum-worker-count-of backend)))
              (send controller
                    'connection
                    (make-instance 'pending-request :socket stream-socket))
              (unwind-protect
                   (progn
                     (ucw.backend.warn "No workers available, temporarily not accepting connections...")
                     (let* ((*request* (read-request backend stream-socket))
                            (*response* (make-response *request*)))
                       (send-standard-error-page :http-status-code +http-service-unavailable+
                                                 :title "Server is overloaded"
                                                 :message "The server is overloaded, please try again later"))
                     #+sbcl(sb-unix:nanosleep 0 #.(* 1000 1000 200))
                     #-sbcl(sleep 1))
                (ignore-errors
                  (usocket:socket-close stream-socket)))))))

(defmethod startup-backend ((backend multithread-httpd-backend) &key (initial-worker-count 2) &allow-other-keys)
  (ucw.backend.debug "Spawning controller thread, acceptor thread and the initial workers")

  (let ((ok nil))
    (unwind-protect
         (progn
           (make-httpd-controller backend)
           (make-httpd-acceptor backend)

           (iter (for n :from 0 :below initial-worker-count)
                 (make-httpd-worker backend))
           (setf (available-workers-of backend) (copy-list (workers-of backend)))

           (ucw.backend.debug "Workers are up, starting the control thread")
           (send (controller-of backend) 'start)
           (ucw.backend.debug "Backend successfully started")
           (setf ok t))
      (unless ok
        (ucw.backend.debug "Cleaning up after a failed backend start")
        (shutdown-backend backend :force t))))
  backend)

(defun shutdown-multithreaded-httpd-backend (backend)
  (setf (controller-of backend) nil)
  (iter (for worker :in (workers-of backend))
        (send worker 'stop))
  (setf (workers-of backend) '()
        (available-workers-of backend) '()
        (worker-count-of backend) 0))

(defmethod shutdown-backend ((backend multithread-httpd-backend)
                             &key force &allow-other-keys)
  (macrolet ((kill-thread-and-catch-error (thread)
               (rebinding (thread)
                 `(block kill-worker
                   (handler-bind ((error (lambda (c)
                                           (warn "Error while killing ~S: ~A." ,thread c)
                                           (return-from kill-worker))))
                     (let ((os-thread (thread-of ,thread)))
                       (ucw.backend.dribble "Killing thread ~A; os thread is ~A" ,thread os-thread)
                       (destroy-thread os-thread)))))))
    (ucw.backend.dribble "Shutting down multithread-httpd-backend ~A, force? ~A" backend force)
    (awhen (socket backend)
      (setf (socket backend) nil)
      (ucw.backend.dribble "Closing socket ~A" it)
      (usocket:socket-close it))
    (if force
        (progn
          (iter (for worker :in (workers-of backend))
                (kill-thread-and-catch-error worker))
          (awhen (acceptor-of backend)
            (kill-thread-and-catch-error it))
          (awhen (controller-of backend)
            (kill-thread-and-catch-error it))
          (setf (workers-of backend) '()
                (available-workers-of backend) '()
                (worker-count-of backend) 0
                (controller-of backend) nil
                (acceptor-of backend) nil))
        (progn
          (awhen (acceptor-of backend)
            (kill-thread-and-catch-error it)
            (setf (acceptor-of backend) nil))
          (send (controller-of backend) 'shutdown)))))

;; Copyright (c) 2003-2006 Edward Marco Baringer
;; All rights reserved. 
;; 
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are
;; met:
;; 
;;  - Redistributions of source code must retain the above copyright
;;    notice, this list of conditions and the following disclaimer.
;; 
;;  - Redistributions in binary form must reproduce the above copyright
;;    notice, this list of conditions and the following disclaimer in the
;;    documentation and/or other materials provided with the distribution.
;; 
;;  - Neither the name of Edward Marco Baringer, nor BESE, nor the names
;;    of its contributors may be used to endorse or promote products
;;    derived from this software without specific prior written permission.
;; 
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;; A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT
;; OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;; SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
;; LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
;; OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
