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
	       (include <complex>)
	       
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
	       ;; http://www.sunshine2k.de/coding/java/TriangleRasterization/TriangleRasterization.html
	       ;; https://gamedev.stackexchange.com/questions/23743/whats-the-most-efficient-way-to-find-barycentric-coordinates
	       ;; https://classes.soe.ucsc.edu/cmps160/Fall10/resources/barycentricInterpolation.pdf
	       ;; http://answers.unity3d.com/questions/383804/calculate-uv-coordinates-of-3d-point-on-plane-of-m.html
	       (function (rasterize_triangle (,@(loop for i in '(a b c) collect
						     `(,i :type complex<float>))
						,@(loop for i below 3 collect
						       `(,(format nil "f~d" i) :type float)
						       )
						(image :type "array<array<float,100>,200>&"))
					     void)
			 (let ((image_max_x :type float :ctor (funcall image.size))
			       (image_max_y :type float :ctor (funcall "image[0].size"))
			       ,@ (loop for typ in '(min max)
				     appending
				       (loop for coord in '(x y) and fun in '(real imag) collect
					    `(,(format nil "~a_~a" typ coord)
					       :ctor (funcall ,typ
							      ,(case typ
								 (min `0s0)
								 (max (format nil "image_max_~a" coord) ))
							      (funcall ,typ
							       (funcall ,typ
									(funcall ,fun a)
									(funcall ,fun b))
							       (funcall ,fun c)
							       )
							      )))))
			   
			   (let ((v0 :ctor (- b a))
				 (v1 :ctor (- c a))
				 
				 ,@ (remove-if #'null
				      (loop for i below 2 appending
					   (loop for j below 2 collect
						(when (<= i j)
						  `(,(format nil "d~d~d" i j) :ctor (funcall real (*
												   ,(format nil "v~d" i)
												   ,(format nil "v~d" j))))))))
				 (denom_ :ctor (/ 1s0
						  (- (* d00 d11)
						     (* d01 d01)))))
			     
			     (for ((y min_y) (<= y max_y) (+= y 1))
				  (for ((x min_x) (<= x max_x) (+= x 1))
				       (let ((p :type complex<float> :ctor (comma-list x y))
					     (v2 :ctor (- p a))
					     ,@ (remove-if #'null
						  (loop for i below 3 appending
						       (loop for j below 3 collect
							    (when (and (<= i j) (or (= i 2) (= j 2)))
							      `(,(format nil "d~d~d" i j) :ctor (funcall real (*
													       ,(format nil "v~d" i)
													       ,(format nil "v~d" j))))))))
					     (v :ctor (* denom_ (- (* d11 d02)
								   (* d01 d12))))
					     (w :ctor (* denom_ (- (* d00 d12)
								   (* d01 d02))))
					     (u :ctor (- 1s0 v w))
					     )
					 (setf (aref image y x)
					       (+ (* u f0)
						  (* v f1)
						  (* w f2)))
					 ))))))
	       (function (lerp ((v0 :type float)
				(v1 :type float)
				(x :type float))
			       float)
			 (return (+ (* (- 1 x) v0)
				    (* x v1))))
	       (function (fuzz_me2 ((data :type "const uint8_t*")
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


;;] http://releases.llvm.org/3.9.0/docs/LibFuzzer.html
