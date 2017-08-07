(eval-when (:compile-toplevel :execute :load-toplevel)
  (ql:quickload :cl-cpp-generator))

(in-package :cl-cpp-generator)

;; https://github.com/google/fuzzer-test-suite

(defparameter *trace-facts*
  `((10 "")))


(defmacro e (&body body)
  `(statements (<< "std::cout" ,@(loop for e in body collect
				      (cond ((stringp e) `(string ,e))
					    (t e))) "std::endl")))

(defmacro er (&body body)
  `(statements (<< "std::cerr" ,@(loop for e in body collect
				      (cond ((stringp e) `(string ,e))
					    (t e))) "std::endl")))



(defun replace-all (string part replacement &key (test #'char=))
"Returns a new string in which all the occurences of the part 
is replaced with replacement."
    (with-output-to-string (out)
      (loop with part-length = (length part)
            for old-pos = 0 then (+ pos part-length)
            for pos = (search part string
                              :start2 old-pos
                              :test test)
            do (write-string string out
                             :start old-pos
                             :end (or pos (length string)))
            when pos do (write-string replacement out)
            while pos))) 


(defun dox (&key brief usage params return)
  `(
    (raw ,(format nil "//! @brief ~a" brief)) (raw "//! ")
    (raw ,(format nil "//! @usage ~a" usage)) (raw "//! ")
    ,@(loop for (name desc) in params collect
	 `(raw ,(format nil "//! @param ~a ~a" name desc)))
    (raw "//! ")
    (raw ,(format nil "//! @return ~a" return))))


(let ((code `(with-compilation-unit
	
		 (include <array>)
	       (include <stdint.h>)
	       (include <stddef.h>)
	       (raw "using namespace std;")
	

	       #+nil ,@(dox :brief ""
		      :usage ""
		      :params '((arguments "")
				)
		      :return "")

	       (function (fuzz_me ((data :type "const uint8_t*")
				   (data_size :type "size_t"))
				  bool)
			 (return (&& (&& (<= 3 data_size)
					 (&& (== (char #\F) (aref data 0))
					     (== (char #\U) (aref data 1))))
				     (&& (== (char #\Z) (aref data 2))
					 (== (char #\Z) (aref data 3))))))
	       (extern-c
		(function  (LLVMFuzzerTestOneInput ((data :type "const uint8_t*")
						    (size :type size_t))
						   int)
			   (funcall fuzz_me data size)
			   (return 0))))))
  (write-source "stage/cl-gen-fuzz/source/main" "cpp" code))


