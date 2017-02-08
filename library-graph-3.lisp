(defpackage #:library-graph-3
  (:use #:cl)
  (:export :main))

(in-package #:library-graph-3)

(defparameter *graph-min-size* 100)

(defclass graph ()
  ((edges :reader edges :initform (make-hash-table :test 'equal :size *graph-min-size*))
   (vertices :reader vertices :initform (make-hash-table :test 'eql :size *graph-min-size*))))

(declaim (inline get-edge-weight make-edge))

(defun connect-vertices (edge vertices) 
  (let ((source (car edge))
        (dest (cdr edge))
        (source-v (gethash (car edge) vertices))
        (dest-v (gethash (cdr edge) vertices)))
    (unless (member source dest-v)
      (setf (gethash dest vertices) (cons source dest-v)))
    (unless (member dest source-v) 
      (setf (gethash source vertices) (cons dest source-v)))))

(defmethod add-edge ((graph graph) edge)
  (let ((edges (edges graph))
        (vertices (vertices graph)))
    (if (nth-value 1 (gethash edge edges))
        (incf (gethash edge edges))
        (setf (gethash edge edges) 1)) 
    (connect-vertices edge vertices)))

(defmethod get-edges-from ((graph graph) source)
  (declare (type (integer 0 4294967295) source))
  (mapcar #'(lambda (dest)
              (make-edge source dest))
          (gethash source (vertices graph))))

(defmethod get-edge-weight ((graph graph) edge) 
  (values (gethash edge (edges graph) 0)))

(defun make-edge (source dest)
  (declare (type (integer 0 4294967295) source dest)) 
  (cons source dest))

(defun print-edge (stream edge &optional weight)
  (format stream "~A:~@[~D~]" edge weight))

(defun split-by (delim str)
  (let ((split (position delim str :test #'char=)))
    (list (subseq str 0 split)
          (subseq str (incf split)))))

(defun process-line (line connectors)
  (declare (type (string *) line)) 
  (destructuring-bind (person-id book-id)
      (mapcar #'parse-integer (split-by #\, line))
    (loop for connector in connectors 
       do (funcall connector person-id book-id))))

(defun mark-time (start-time destination current-progress old-percent)
  (let ((percent (truncate (* 100 (/ current-progress destination)))))
    (if (= percent old-percent)
        old-percent
        (let ((elapsed (- (get-internal-real-time) start-time)))
          (format t "~&~D Percent complete. ~Ds elapsed.~%" percent (round (/ elapsed internal-time-units-per-second)))
          percent))))

(defun slurp (stream connectors &optional (start-time (get-internal-real-time)))
  (let ((file-length (file-length stream)))
    (loop for line = (read-line stream nil nil) then (read-line stream nil nil)
       for percent = 0 then (mark-time start-time file-length (file-position stream) percent)
       while line
       do (process-line line connectors)
       finally (return))))

(defun parse-seconds (seconds &optional (minutes 0) (hours 0) (days 0))
  (cond
    ((> hours 23) (parse-seconds seconds minutes (- hours 24) (1+ days)))
    ((> minutes 59) (parse-seconds seconds (- minutes 60) (1+ hours) days))
    ((> seconds 59) (parse-seconds (- seconds 60)))
    (t (list seconds minutes hours days))))

(defun main (in-file &optional (out-file #P"~/library-graph-output.txt"))
  (let ((start-time (get-internal-real-time))
        (connectors *sample-connectors*)
        (graphs *sample-graphs*)
        (queries *sample-queries*))    
    (with-open-file (out-stream out-file :direction :output :if-exists :rename :if-does-not-exist :create)
      (with-open-file (in-stream in-file)
        (slurp in-stream connectors)
        (format t "~&Executing queries and printing results to output file...")
        (loop for query in queries
           do (let ((query-name (query-name query)))
                (format t "~&Executing Query: ~A..." query-name)
                (format out-stream "~&Executing query: ~A~%Results:~%~%" query-name)
                (execute-query query graphs out-stream)))))
    (let ((elapsed (round (/ (get-internal-real-time) internal-time-units-per-second))))
      (format t "~&Total time elapsed: ~A" (parse-seconds elapsed)))))
