(in-package #:library-graph-3)

(defstruct (query (:constructor make-query (name fun)))
  (name nil :type symbol :read-only t)
  (fun '() :type (function (hash-table stream) list) :read-only t))

(defun execute-query (query graphs stream) 
  (funcall (query-fun query) graphs stream))

(defparameter *sample-queries*
  (list
   (make-query :book-bought-with-book
               (lambda (graphs stream)
                 (let ((graph (gethash :book-bought-with-book graphs)))
                   (loop for edge being the hash-key of (edges graph) using (hash-value weight)
                      do (format stream "~&Book ~D bought with book ~D ~D times" (car edge) (cdr edge) weight)))))
   
   (make-query :all-data
               (lambda (graphs stream)
                 (loop for graph being the hash-values of graphs using (hash-key graph-name)
                    do (progn
                         (format stream "~&Graph: ~A~%Edges:~%" graph-name)
                         (loop for edge being the hash-key of (edges graph) using (hash-value weight)
                            do (format stream "~&Vertex ~D connects to vertex ~D with weight ~D" (car edge) (cdr edge) weight))
                         (format stream "~&~%Vertices:~%")
                         (loop for vertex being the hash-key of (vertices graph) using (hash-value connects)
                            do (format stream "~&Vertex ~D has connections to vertices: ~{~A ~}"vertex connects))))))))
