#|
 | Calculate the best myki fare for an ordered set of dates
 | Reference brute force solution. Try every possible combination of money and passes.
 |
 | Dates are days from reference date; 0 = (monday <= today)
 |#

(defun memoize (fn)
  (let ((cache (make-hash-table :test #'equal)))
    (lambda (&rest args)
      (multiple-value-bind 
        (result exists)
        (gethash args cache)
        (if exists
          result
          (setf (gethash args cache)
                (apply fn args)))))))

(defun fare (set)
  "Calculate best fare for given set of days (max 365), solely using either money or pass"
  (min
    ;; money
    ;; todo: public holiday / weekend daily cap
    (* 554/100 (length set))
    ;; pass
    (let ((days (1+ (- (car (last set)) (first set)))))
      (if (<= days 7)
        ;; seven day pass
        2770/100
        ;; day passes
        (* 311/100 (min 325 (max 28 days)))))))

(defun best-fare (set)
  (loop for i from 1 to (length set)
        minimize (+ (best-fare (nthcdr i set)) (fare (subseq set 0 i)))))

(setf (fdefinition 'fare) (memoize #'fare))
(setf (fdefinition 'best-fare) (memoize #'best-fare))

