;; CoreA CPU Benchmark (Lisp)
;; Measures CPU performance with arithmetic loop

(cffi:define-foreign-library corea
  (:unix "libc.so"))
(cffi:use-foreign-library corea)

(cffi:defcfun "sys_exit" :void (status :int))

(defun cpu-bench ()
  (let ((iterations 1000000)
        (result 0))
    (loop for i from 1 to iterations do
      (incf result (* i 2)))
    (format t "CPU Benchmark Result: ~a~%" result)
    (sys_exit 0)))

(cpu-bench)
