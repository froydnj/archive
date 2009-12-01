;;;; macros.lisp -- various internal and external macros

(in-package :archive)

(defun extractor-function-name (entry-name field-name)
  (intern (format nil "~A-READ-~A-FROM-BUFFER" entry-name field-name)))
(defun injector-function-name (entry-name field-name)
  (intern (format nil "~A-WRITE-~A-TO-BUFFER" entry-name field-name)))

(defmacro with-extracted-fields ((entry-class buffer offset &rest fields)
                                 &body body)
  `(let ,(mapcar #'(lambda (field-name)
                     `(,field-name (,(extractor-function-name entry-class field-name) ,buffer ,offset)))
                 fields)
    ,@body))

(defmacro define-octet-header (class-name &rest field-defs)
  (let ((offset 0))                     ; could be integrated in the LOOP?
    (flet ((offset-constant-symbol (name)
             (intern (format nil "+~A-~A-OFFSET+" class-name
                             (string-upcase (symbol-name name)))))
           (length-constant-symbol (name)
             (intern (format nil "+~A-~A-LENGTH+" class-name
                             (string-upcase (symbol-name name)))))
           (keywordify-name (name)
             (intern (string-upcase (symbol-name name))
                     (find-package "KEYWORD"))))
      (loop for (name length kind) in field-defs
            collect `(defconstant ,(offset-constant-symbol name)
                      ,offset) into constant-defs
            collect `(defconstant ,(length-constant-symbol name)
                      ,length) into constant-defs
            collect `(defun ,(extractor-function-name class-name name) (buffer entry-start)
                      (declare (type (simple-array (unsigned-byte 8) (*)) buffer))
                      ,(ecase kind
                        (:string `(read-bytevec-from-buffer buffer :start (+ entry-start ,offset)
                                   :end (+ entry-start ,offset ,length) :nullp nil))
                        (:string-null `(read-bytevec-from-buffer buffer :start (+ entry-start ,offset)
                                        :end (+ entry-start ,offset ,length) :nullp t))
                        (:byte
                         (unless (= length 1)
                           (error ":BYTE fields cannot be longer than 1"))
                         `(aref buffer (+ entry-start ,offset)))
                        (:octnum `(read-number-from-buffer buffer :start (+ entry-start ,offset)
                                   :end (+ entry-start ,offset ,length) :radix 8))
                        (:hexnum `(read-number-from-buffer buffer :start (+ entry-start ,offset)
                                   :end (+ entry-start ,offset ,length) :radix 16)))) into reader-defs
            collect `(defun ,(injector-function-name class-name name) (buffer entry-start thing)
                      (declare (type (simple-array (unsigned-byte 8) (*)) buffer))
                      ,(ecase kind
                        ((:string :string-null)
                         `(dotimes (i (length thing) (values))
                           (setf (aref buffer (+ entry-start ,offset i)) (aref thing i))))
                        (:byte
                         `(setf (aref buffer (+ entry-start ,offset)) thing))
                        (:octnum
                         `(let ((start (+ entry-start ,offset))
                                (end (+ entry-start ,offset ,length)))
                           (write-number-to-buffer thing buffer :start start :end end :radix 8 :nullp t)))
                        (:hexnum
                         `(let ((start (+ entry-start ,offset))
                                (end (+ entry-start ,offset ,length)))
                           (write-number-to-buffer thing buffer :start start :end end :radix 16 :nullp nil))))
                      (values)) into writer-defs
            collect `(,name :initarg ,(keywordify-name name)
                            :accessor ,name) into slot-definitions
            append `(,(keywordify-name name)
                      ,(case kind
                         ((:string :string-null) (make-array 0 :element-type '(unsigned-byte 8)))
                         (t 0))) into default-initargs
            do (incf offset length)
            finally (return
                      `(progn
                        ,@constant-defs
                        ,@reader-defs
                        ,@writer-defs
                        (defconstant ,(intern (format nil "+~A-LENGTH+" class-name)) ,offset)
                        (defclass ,class-name ()
                          ,slot-definitions
                          (:default-initargs ,@default-initargs))))))))

(defmacro with-open-archive ((archive-var pathname
                                          &key (direction :input)
                                          (if-exists nil)
                                          (if-does-not-exist nil)
                                          (archive-type 'tar-archive)) &body body)
  (when (or (eq direction :io) (eq direction :probe))
    (error "Cannot open archives in direction ~A" direction))
  (let ((stream-var (gensym)) (should-close (gensym))
        (gpath (gensym)) (gabort (gensym)))
    `(let ((,archive-var nil) (,gpath ,pathname) (,should-close t)
           (,stream-var nil) (,gabort t))
       (unwind-protect
           (progn
             (when (streamp ,gpath) (setf ,should-close nil))
             (setf ,stream-var
                   (if ,should-close
                       (open ,gpath :direction ,direction
                             ,@(when if-exists
                                 `(:if-exists ,if-exists))
                             ,@(when if-does-not-exist
                                 `(:if-does-not-exist ,if-does-not-exist))
                             :element-type '(unsigned-byte 8))
                       ,gpath))
             (setf ,archive-var (open-archive ',archive-type
                                              ,stream-var
                                              :direction ,direction))
             (multiple-value-prog1
                 (progn ,@body)
               (setf ,gabort nil)))
         (when ,archive-var
           (close-archive ,archive-var)
           (setf ,archive-var nil))
         (when ,should-close
           (close ,stream-var :abort ,gabort))))))

(defmacro do-archive-entries ((entry archive &optional result)
                              &body body)
  "Iterate over the entries in ARCHIVE.  For each entry, ENTRY is bound to
an ARCHIVE-ENTRY representing the entry.  RESULT is used as in DOTIMES."
  (let ((archive-var (gensym)))
    `(let ((,archive-var ,archive))
      (do ((,entry (read-entry-from-archive ,archive-var)
                   (read-entry-from-archive ,archive-var)))
          ((null ,entry) ,result)
        ,@body
        (discard-entry ,archive-var ,entry)))))
