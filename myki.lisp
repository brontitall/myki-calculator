#|
 | Calculate the best myki fare for an ordered set of dates
 | Reference brute force solution. Try every possible combination of money and passes.
 |#

(defun parts (set)
  "Return list of all ways of partitioning list (uses exponential time and space)"
  (loop for i from 1 to (length set)
        for take = (subseq set 0 i)
        for sub = (parts (nthcdr i set))
        nconc (if sub
                (mapcar (lambda (n) (list* take n)) sub)
                (list (list take)))))

(print (parts (list 'a 'b )))

;; todo: try alternating money,pass,money… and pass,money,pass… for each partition
