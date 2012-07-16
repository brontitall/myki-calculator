#|
 | Calculate the best myki fare for an ordered set of dates
 | Reference brute force solution. Try every possible combination of money and passes.
 |
 | Dates are days from reference date; 0 = (monday <= today)
 | Dates must be within 365 days of each other
 |#

(defun memoize (fn)
  (let ((cache (make-hash-table :test #'equal)))
    (lambda (&rest args)
      (if (eq :g (car args))
        (maphash (lambda (k v) (print (list k v))) cache)
        (multiple-value-bind
          (result exists)
          (gethash args cache)
          (if exists
            result
            (setf (gethash args cache)
                  (apply fn args))))))))

(defun fare (set)
  "Find best fare solely using either money or pass for all given dates"
  ;; todo: public holiday / weekend daily cap
  (let ((money (* 554 (length set)))
        (pass  (let ((days (1+ (- (car (last set)) (first set)))))
                 (if (<= days 7)
                   2770
                   (* 311 (min 325 (max 28 days)))))))
    (if (< money pass)
      (values money :money)
      (values pass :pass))))

(defun best-fare (set)
  (loop with lowest = 0
        with ranges = nil
        for i from (length set) downto 1
        for head = (subseq set 0 i)
        for tail = (nthcdr i set)
        for (tail-fare tail-ranges) = (best-fare tail)
        for head-fare = (fare head)
        for total-fare = (+ head-fare tail-fare)
        if (or (zerop lowest) (< total-fare lowest)) do
          (setq lowest total-fare
                ranges (list* head tail-ranges))
        finally (return (list lowest ranges))))

(setf (fdefinition 'best-fare) (memoize #'best-fare))

(defun run-test (n)
  (print (time (best-fare (loop for i below n collect i)))))
