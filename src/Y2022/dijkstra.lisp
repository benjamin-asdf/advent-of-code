(defun make-heap (&key (test #'<))
  (let ((heap (list)))
    (labels ((swap (i j)
               (let ((temp (nth i heap)))
                 (setf (nth i heap) (nth j heap))
                 (setf (nth j heap) temp))))
      (defun push-heap (elem)
        (push elem heap)
        (do* ((n (1- (length heap))
               (floor n 2))
            (parent (floor n 2)
                    (floor n)))
            ((null heap))
          (when (and (> n 0)
                     (funcall test (nth n heap)
                              (nth parent heap)))
            (swap n parent)
            (setq n parent
                  parent (floor n 2)))))
      (defun pop-heap ()
        (pop heap))
      (defun peek ()
        (first heap))
      (defun length-heap ()
        (length heap))
      (defun clear ()
        (setq heap (list)))
      heap)))

(defun hash-table-keys (table)
  (let ((keys (list)))
    (maphash (lambda (key value)
               (push key keys))
             table)
    keys))

(defun dijkstra (graph start-node)
  (let ((distances (make-hash-table))
        (previous (make-hash-table))
        (priority-queue (make-heap :test #'<)))
    (dolist (node (hash-table-keys graph))
      (if (not (eql node start-node))
          (progn
            (setf (gethash node distances) most-positive-word)
            (setf (gethash node previous)
                  nil)
            (push-heap node))))
    (loop
      with distances = distances
      with previous = previous
      with priority-queue = priority-queue
      until (null priority-queue)
      do (let ((current-node (pop-heap priority-queue)))
           (dolist (neighbor (gethash current-node graph))
             (let ((alt (+ (gethash current-node distances)
                           (gethash (car neighbor) (cdr neighbor)))))
               (when (< alt (gethash (car neighbor) distances))
                 (setf (gethash (car neighbor) distances) alt)
                 (setf (gethash (car neighbor) previous) current-node)
                 (push-heap (car neighbor))))))
      finally (return (list :distances distances
                            :previous previous)))))

(defparameter *graph*
  (make-hash-table
   :test #'equal
   :size 10))

;; add some key-value pairs to the hash table
(progn (setf (gethash "A" *graph*) (list '("B" . 5) '("C" . 1)))
       (setf (gethash "B" *graph*) (list '("A" . 5) '("C" . 2) '("D" . 1)))
       (setf (gethash "C" *graph*) (list '("A" . 1) '("B" . 2) '("D" . 4)))
       (setf (gethash "D" *graph*) (list '("B" . 1) '("C" . 4))))


;; call the dijkstra function with the graph and start node "A"
(dijkstra *graph* "A")
;; => (:DISTANCES #<HASH-TABLE :TEST EQUAL :COUNT 4 {1009F497C3}>
;;             :PREVIOUS #<HASH-TABLE :TEST EQUAL :COUNT 4 {1009F4A513}>)


;; get the distances map
(first (dijkstra *graph* "A"))
;; => #<HASH-TABLE :TEST EQUAL :COUNT 4 {1009F497C3}>

;; look up the shortest distance from the start node to node "D"
(gethash "D" (first (dijkstra *graph* "A")))
;; => 2.0

;; get the previous vertices map
(second (dijkstra *graph* "A"))
;; => #<HASH-TABLE :TEST EQUAL :COUNT 4 {1009F4A513}>

;; look up the previous vertex in the shortest path from the start node to node "D"
(gethash "D" (second (dijkstra *graph* "A")))
;; => "B"
