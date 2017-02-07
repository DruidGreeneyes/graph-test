;;;; library-graph.lisp
(defpackage #:library-graph
  (:use #:cl))

(in-package #:library-graph)

;;; "library-graph" goes here. Hacks and glory await!

(defparameter *person-buys-book* 0)
(defparameter *book-bought-with-book* 1)
(defparameter *edge-connectors* nil)

(defstruct (edge (:conc-name nil))
  (edge-source -1 :type integer :read-only t)
  (edge-dest -1 :type integer :read-only t)
  (edge-type -1 :type integer :read-only t)
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

(defun valid-edge-p (edge &key (source nil) (dest nil) (e-type nil) (weight nil))
  (let ((tests nil))
    (when source (push (= source (edge-source edge)) tests))
    (when dest (push (= dest (edge-dest edge)) tests))
    (when e-type (push (= e-type (edge-type edge)) tests))
    (when weight (push (= weight (edge-weight edge)) tests))
    (and (not (member nil tests))
         edge)))

(defmacro current-edge () `(aref graph pos))

(defun find-connection (graph source dest e-type &optional (pos 0) (pointer (fill-pointer graph)))
  (if (>= pos pointer)
      nil 
      (or (valid-edge-p (current-edge) :source source :dest dest :e-type e-type) 
          (find-connection graph source dest e-type (1+ pos) pointer))))

(defun find-connections-from (graph source e-type &optional (res nil) (pos 0) (pointer (fill-pointer graph))) 
  (if (>= pos pointer)
      res
      (let ((new-res (if (valid-edge-p (current-edge) :source source :e-type e-type)
                         (cons (current-edge) res)
                         res))) 
        (find-connections-from graph source e-type new-res (1+ pos) pointer))))

(defun connects-book-p (edge book-id &key (endpoint :both))
  (case endpoint
    (:source (= book-id (edge-source edge)))
    (:dest (= book-id (edge-dest edge)))
    (otherwise 
     (or (= book-id (edge-source edge))
         (= book-id (edge-dest edge))))))

(defun split-by (delim str)
  (let ((index (position delim str)))
    (values (subseq str 0 index)
            (subseq str (1+ index) (length str)))))

(defun connect-person-to-book (person-id book-id graph) 
  (let ((edge (find-connection graph person-id book-id *person-buys-book*)))
    (if edge
        (incf (edge-weight edge))
        (let ((new-edge (make-edge :edge-source person-id :edge-dest book-id :edge-type *person-buys-book*)))
          (add-to-graph graph new-edge)))))
(push #'connect-person-to-book *edge-connectors*)

(defun sort-nums (a b)
  (if (> b a)
      (values b a)
      (values a b)))

(defun connect-book-to-book (book-id-a book-id-b graph)
  (multiple-value-bind (source dest)
      (sort-nums book-id-a book-id-b)
    (let ((edge (find-connection graph source dest *book-bought-with-book*)))
      (if edge
          (incf (edge-weight edge))
          (let ((new-edge (make-edge :edge-source source :edge-dest dest :edge-type *book-bought-with-book*))) 
            (add-to-graph graph new-edge))))))

(defun connect-books-by-person (person-id book-id graph)
  (let ((edges (find-connections-from graph person-id *person-buys-book*))) 
    (loop for edge in edges
       unless (= book-id (edge-dest edge))
       do (connect-book-to-book book-id (edge-dest edge) graph))))
(push #'connect-books-by-person *edge-connectors*)

(defun trim-empty-elements (vector)
  (adjust-array vector (fill-pointer vector)))

(defun slurp (text-stream &optional (results (make-array 100 :element-type 'edge :adjustable t :fill-pointer 0)))
  (let ((line (handler-case (read-line text-stream)
                (end-of-file () nil))))
    (if line
        (multiple-value-bind (person-id book-id)
            (split-by #\, line)
          (loop for connector in *edge-connectors*
             do (funcall connector (parse-integer person-id) (parse-integer book-id) results))
          (slurp text-stream results))
        (trim-empty-elements results))))

(defmacro defquery (name condition)
  `(defun ,name (graph)
     (loop for edge across graph
        when ,condition
        collect (print-edge nil edge))))

;;;;sample query(s)

;;everything
(defquery all-edges t)

;;all books bought together
(defquery all-books-together (= *book-bought-with-book* (edge-type edge)))

;;purchase history for person 54
(defquery person-54 (and (= *person-buys-book* (edge-type edge))
                         (= 54 (edge-source edge))))

(defun main (filename &optional (output-filename "output.txt")) 
  (with-open-file (out-stream output-filename :direction :output :if-exists :rename :if-does-not-exist :create)
    (with-open-file (in-stream filename)
      (let ((graph (slurp in-stream))) 
        (format out-stream "All edges in graph: ~%~{~A~%~}~%~%" (all-edges graph))
        (format out-stream "Book co-purchase counts: ~%~{~A~%~}~%~%" (all-books-together graph))
        (format out-stream "Purchase history for person 54: ~%~{~A~%~}~%~%" (person-54 graph))))))

;;(library-graph::main #P"~/Downloads/data.txt" #P"~/Downloads/output.txt" library-graph::*sample-queries*)
