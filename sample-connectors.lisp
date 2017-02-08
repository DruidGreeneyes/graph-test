(in-package #:library-graph-3)


;;;;graphs and their connectors
(defparameter *sample-graphs* (make-hash-table :test 'equal))
(defparameter *sample-connectors* nil)

(setf (gethash :test-graph *sample-graphs*) (make-instance 'graph))
(print '(gethash :test-graph *sample-graphs*))
(multiple-value-bind (g f)
    (gethash :test-graph *sample-graphs*)
  (format t "~A, ~A" g f))
(remhash :test-graph *sample-graphs*)

(let ((graph (make-instance 'graph)))
  (setf (gethash :person-buys-book *sample-graphs*) graph)
  (defun connect-person-to-book (person-id book-id)
    (declare (type (integer 0 4294967295) person-id book-id))
    (add-edge graph (make-edge person-id book-id))))
(push #'connect-person-to-book *sample-connectors*)

(let ((graph (make-instance 'graph)))
  (setf (gethash :book-bought-with-book *sample-graphs*) graph)
  (defun connect-book-to-book (book-id-a book-id-b)
    (declare (type (integer 0 4294967295) book-id-a book-id-b))
    (let ((edge (if (> book-id-b book-id-a)
                    (make-edge book-id-b book-id-a)
                    (make-edge book-id-a book-id-b))))
      (add-edge graph edge))))

(let ((graph (or (gethash :person-buys-book *sample-graphs*)
                 (make-instance 'graph))))
  (setf (gethash :person-buys-book *sample-graphs*) graph)
  (defun connect-books-by-person (person-id book-id)
    (declare (type (integer 0 4294967295) person-id book-id))
    (loop for (_ . dest) in (get-edges-from graph person-id)
       unless (= dest book-id)
       do (connect-book-to-book book-id dest))))
(push #'connect-books-by-person *sample-connectors*)


;;;;queries against graphs
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
                            do (format stream "~&Vertex ~D has connections to vertices: ~{~A ~}"vertex connects))))))

   (make-query :person-54
               (lambda (graphs stream)
                 (let* ((graph (gethash :person-buys-book graphs))
                        (edges (edges graph))
                        (books (gethash 54 (vertices graph))))
                   (loop for book in books do
                        (let ((edge (cons 54 book)))
                          (format stream "~&Person ~D bought book ~D, ~D times." (car edge) (cdr edge) (gethash edge edges 0)))))))))
