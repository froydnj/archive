(defpackage :archive
  (:use :cl #+(and lispworks (not win32)) :sys)
  (:export ;; types
           #:archive #:tar-archive #:odc-cpio-archive #:svr4-cpio-archive

           ;; creating
           #:open-archive #:close-archive

           ;; entry slot readers
           #:name
           #:entry-stream

           ;; entry tests
           #:entry-directory-p
           #:entry-regular-file-p
           #:entry-symbolic-link-p
           #:entry-character-device-p
           #:entry-block-device-p
           #:entry-fifo-p

           ;; reading archives
           #:read-entry-from-archive
           #:extract-entry
           #:discard-entry

           ;; writing archives
           #:create-entry-from-pathname
           #:write-entry-to-archive #:finalize-archive

           ;; convenience macros
           #:do-archive-entries #:with-open-archive

           ;; external support
           #:*bytevec-to-string-conversion-function*
           #:*string-to-bytevec-conversion-function*))
