(in-package #:library-graph-3)

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