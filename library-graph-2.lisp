(defpackage #:library-graph-2
  (:use #:cl))

(in-package #:library-graph-2)

(defparameter *connectors* nil)
(defparameter *graphs* (make-hash-table))
(defparameter *sample-queries* (make-hash-table))

(defun sort-nums (a b)
  (if (> b a)
      (values b a)
      (values a b)))

(defun split-by (delim str)
  (let ((index (position delim str)))
    (list (subseq str 0 index)
          (subseq str (1+ index) (length str)))))

(defun trim-to-fill-pointer (vector)
  (adjust-array vector (fill-pointer vector)))

(defstruct (edge (:conc-name nil) (:constructor make-edge edge-source edge-dest))
  (edge-source -1 :type integer :read-only t)
  (edge-dest -1 :type integer :read-only t) 
  (edge-weight 1 :type integer))

(defun endpoints (edge)
  (list (edge-source edge) (edge-dest edge)))

(defun print-edge (stream edge &key (newline nil))
  (let ((type-format (if (= 0 (edge-type edge))
                         "person ~D bought book ~D"
                         "book ~D bought with book ~D")))
    (format stream "~?, count: ~D ~:[~;~%~]" type-format (endpoints edge) (edge-weight edge) newline)))

(defun print-graph (stream graph)
  (loop for edge across graph do (print-edge stream edge :newline t)))

(defun add-to-graph (graph edge)
  (let ((add-size (ceiling (/ (fill-pointer graph) 2))))
    (vector-push-extend edge graph add-size)))

(defun check-when-present (a b &key (test #'=))
  (if a
      (apply test a b)
      t))

(defun valid-edge-p (edge &key source dest weight) 
  (and (if source (= source (edge-source edge)) t)
       (if dest (= dest (edge-dest edge)) t)
       (if weight (= weight (edge-weight edge)) t)
       edge))

(defun find-connection (graph source dest)
  (loop for edge across graph
     when (valid-edge-p edge :source source :dest dest)
     return edge
     finally (return nil)))

(defun find-connections-from (graph source)
  (loop for edge across graph
     when (valid-edge-p edge :source source)
     collect edge))

(defmacro define-connector (name &rest body)
  `(let ((graph (make-array 10 :element-type 'edge :adjustable t :fill-pointer 0)))
     (setf (gethash ,name *graphs*) graph)
     (push (lambda  (person-id book-id)
             ,@body) *connectors*)))

(defun connect-vertices (v1 v2 graph)
  (let ((edge (find-connection graph v1 v2)))
    (if edge
        (incf (edge-weight edge))
        (add-to-graph graph (make-edge v1 v2)))))

(define-connector person-to-book
    (connect-vertices person-id book-id graph))

(define-connector books-bought-together
    (let ((edges (remove-if #'(lambda (edge) (= book-id (edge-dest edge)))
                            (find-connections-from (gethash 'person-to-book *graphs*) person-id))))
      (loop for edge in edges
         do (connect-vertices book-id (edge-dest edge) graph))))

(defun slurp (text-stream)
  (handler-bind ((end-of-file #'loop-finish))
    (loop for line = (read-line text-stream) then (read-line text-stream)
       do (loop for conn in *connectors*
             do (apply conn (mapcar #'parse-integer (split-by #\, line))))
       finally (return *graphs*))))

(defmacro define-query (query-name graph-name condition-lambda &key (query-table *sample-queries*) (graph-table *graphs*))
  `(let ((graph (gethash ,graph-name ,graph-table)))
     (setf (gethash ,query-name ,query-table)
           (lambda ()
             (loop for edge across graph
                when (apply condition-lambda edge)
                collect (print-edge nil edge))))))

(defun main (in-file &optional (out-file "output.txt") &key (query-table *sample-queries*)) 
  (with-open-file (out-stream out-file :direction :output :if-exists :rename :if-does-not-exist :create)
    (with-open-file (in-stream in-file)
      (let ((results (slurp in-stream)))
        (loop for query being the hash-values in query-table using (hash-key name)
           do (format out-stream "Running query ~A:~%~{~A~%~}~%~%" name (funcall query)))))))
