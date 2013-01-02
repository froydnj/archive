;;;; archive.lisp -- common handling for archive files

(in-package :archive)

(defclass archive ()
  ((entry-buffer :initarg :entry-buffer :reader entry-buffer
                 :type (simple-array (unsigned-byte 8) (*)))
   (file-buffer :initform (make-array 8192 :element-type '(unsigned-byte 8))
                :reader file-buffer
                :type (simple-array (unsigned-byte 8) (*)))
   (direction :initarg :direction :reader %archive-direction)
   (bytes-output :initform 0 :accessor bytes-output)
   (open-archive-p :initform t :accessor open-archive-p)
   (skippable-p :initform nil :accessor skippable-p)
   (stream :initarg :stream :reader archive-stream :type stream)))

;; Enable nifty trick of skipping archive data rather than reading it.
#+(or sbcl cmucl (and lispworks unix))
(defmethod initialize-instance :after ((instance archive) &rest initargs)
  (declare (ignore initargs))
  (let ((stream (archive-stream instance)))
    ;; Hopefully this is portable.
    (when (typep stream 'file-stream)
      (let ((stat (stat #+sbcl (sb-impl::fd-stream-fd stream)
                        #+cmucl (system::stream-fd stream)
                        #+(and lispworks unix) (stream::os-file-handle-stream-file-handle stream))))
        (when (and stat (isreg (stat-mode stat)))
          (setf (skippable-p instance) t))))))

(defun initialize-entry-buffer (archive buffer-length)
  "Initialize the ENTRY-BUFFER of ARCHIVE."
  (setf (slot-value archive 'entry-buffer)
        (make-array buffer-length :element-type '(unsigned-byte 8))))

(defun open-archive (archive-type stream &key (direction :input))
  "Return an archive.  STREAM is the underlying Lisp stream for the archive.
STREAM should not be read from or written to anymore."
  (declare (type (member :input :output) direction))
  (make-instance archive-type :stream stream :direction direction))

(defun close-archive (archive)
  "Closes the stream associated with ARCHIVE and the archive itself.
Further operations on the archive are undefined."
  (when (open-archive-p archive)
    (close (archive-stream archive))
    (setf (open-archive-p archive) nil))
  t)

(defun read-entry-block (archive)
  (with-slots (entry-buffer stream) archive
    (let ((nbytes (read-sequence entry-buffer stream)))
      (unless (= nbytes (length entry-buffer))
        (error "Corrupt archive"))
      entry-buffer)))

(defun read-data-block (archive block-length &optional (pad-func #'identity))
  "Read a (SIMPLE-ARRAY (UNSIGNED-BYTE 8) (*)) array of BLOCK-LENGTH
from ARCHIVE.  BLOCK-LENGTH is padded with PAD-FUNC to meet archive
requirements about alignment."
  (let ((length (funcall pad-func block-length)))
    (with-slots (file-buffer stream) archive
      (when (> length (length file-buffer))
        (let ((new-buffer (make-array (* (length file-buffer) 2)
                                      :element-type '(unsigned-byte 8))))
          (setf file-buffer new-buffer)))
      (read-sequence file-buffer stream :end length)
      (subseq file-buffer 0 block-length))))

(defun write-data-block (archive block start &optional end)
  (write-sequence block (archive-stream archive)
                  :start start :end (or end (length block))))

(defmethod read-entry-from-archive :before ((archive archive))
  (unless (eq (%archive-direction archive) :input)
    (error "Attempting to read from a non-input archive")))

(defmethod write-entry-to-archive :before ((archive archive) entry
                                           &key stream)
  (declare (ignore stream))
  (unless (eq (%archive-direction archive) :output)
    (error "Attempting to write to a non-output archive")))

(defmethod write-entry-data ((archive archive) entry stream)
  (cond
    ((eq t stream)
     (with-open-file (filestream (name entry) :direction :input
                                 :element-type '(unsigned-byte 8)
                                 :if-does-not-exist :error)
       (transfer-stream-to-archive archive filestream)))
    ((typep stream 'stream)
     (if (or (equal (stream-element-type stream) '(unsigned-byte 8))
             (equal (stream-element-type stream) '(integer 0 255)))
         (transfer-stream-to-archive archive stream)
         (error "Stream has invalid STREAM-ELEMENT-TYPE ~A"
                (stream-element-type stream))))
    ((eq nil stream)
     ;; do nothing
     )
    (t
     (error "Invalid argument for :STREAM: ~A" stream))))

(defmethod write-entry-data ((archive archive) (entry directory-entry-mixin)
                             stream)
  ;; Directories generally don't have any associated data.
  (values))

(defmethod write-entry-to-archive ((archive archive) entry &key (stream t))
  (with-slots (entry-buffer (archive-stream stream)) archive
    ;; write the entry
    (write-entry-to-buffer entry entry-buffer 0)
    (write-sequence entry-buffer archive-stream)
    ;; write any associated data
    (write-entry-data archive entry stream)
    (values)))

(defmethod write-entry-to-archive :after (archive (entry directory-entry-mixin)
                                          &key stream)
  (declare (ignore stream))
  (let ((dirname (name entry)))
    (mapc
     (lambda (pathname)
       (flet ((relative-pathname (pathname)
                (if (fad:directory-pathname-p pathname)
                    (fad:pathname-as-directory (enough-namestring pathname))
                    (fad:pathname-as-file (enough-namestring pathname)))))
         (let* ((absolute? (and (not (string= "" dirname))
                                (char= #\/ (char dirname 0))))
                (adjusted-pathname (if absolute? pathname
                                       (relative-pathname pathname)))
                (entry (create-entry-from-pathname archive adjusted-pathname)))
           (write-entry-to-archive archive entry))))
     (fad:list-directory dirname))))


;;; providing streamy access for an entry
(defun make-stream-for-entry (archive entry)
  (make-bounded-stream (archive-stream archive) (size entry)))

(defmethod read-entry-from-archive :around (archive)
  (let ((entry (call-next-method)))
    (when entry
      (setf (slot-value entry 'stream)
            (make-stream-for-entry archive entry)))
    entry))

(defun entry-stream (entry)
  "Return a stream connected to the data of ENTRY."
  (slot-value entry 'stream))


;;; doing interesting things with entries

(defun discard-unused-entry-data (archive entry rounding-function)
  (transfer-entry-data-to-stream* archive entry nil rounding-function))

(defun transfer-entry-data-to-stream* (archive entry stream rounding-function)
  (when (data-discarded-p entry)
    ;; by definition, there's nothing left
    (return-from transfer-entry-data-to-stream* (values)))
  (let* ((entry-stream (entry-stream entry))
         (n-bytes-remaining (n-bytes-remaining entry-stream))
         (rounded-size (funcall rounding-function (size entry)))
         (rounded-n-bytes-remaining (- rounded-size
                                       (- (size entry) n-bytes-remaining))))
    (tagbody
       (unless (and (skippable-p archive) (null stream))
         (go :READ-DATA-THROUGH))
     :ATTEMPT-TO-SKIP
       (let ((current-position (file-position (archive-stream archive))))
         (when current-position
           (let ((new-position (file-position (archive-stream archive)
                                              (+ current-position
                                                 rounded-n-bytes-remaining))))
             (when new-position
               (setf rounded-n-bytes-remaining 0)
               (go :CLEANUP)))))
     :SKIP-FAILED
       (setf (skippable-p archive) nil)
     :READ-DATA-THROUGH
       (loop with archive-stream = (archive-stream archive)
          with buffer = (file-buffer archive)
          for bytes-read = (read-sequence buffer archive-stream
                                          :start 0
                                          :end (min (length buffer)
                                                    rounded-n-bytes-remaining))
          do (assert (not (minusp n-bytes-remaining)))
          (decf rounded-n-bytes-remaining bytes-read)
          ;; flush to the other stream
          (when stream
            (write-sequence buffer stream :start 0
                            :end (min n-bytes-remaining bytes-read)))
          (decf n-bytes-remaining bytes-read)
          while (plusp rounded-n-bytes-remaining))
     :CLEANUP
       ;; make sure we didn't overrun the data of the entry
       (assert (zerop rounded-n-bytes-remaining))
       ;; make sure nobody can read from the entry's stream
       (setf (n-bytes-remaining entry-stream) 0)
       ;; indicate that we've already discarded the data
       (setf (data-discarded-p entry) t)
       (values))))

(defun extract-files-from-archive (archive &optional (filter (constantly t)))
  (do-archive-entries (entry archive)
    (if (funcall filter (name entry))
        (extract-entry archive entry)
        (discard-entry archive entry))))

(defun extract-files-from-pathname (pathname &optional (filter (constantly t)))
  (with-open-archive (archive pathname :direction :input)
    (extract-files-from-archive archive filter)))
