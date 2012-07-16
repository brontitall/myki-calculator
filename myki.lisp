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
      (multiple-value-bind (result exists)
          (gethash args cache)
        (if exists
          result
          (setf (gethash args cache)
                (apply fn args)))))))

;; todo: MUST take into account public holidays
;; todo: MUST allow choosing zones and concession / full fare

(defun money-fare (day)
  (case (rem day 7)
    ((5 6) 330) ;; weekend daily cap $3.30
    (t 554)))   ;; daily zone 1+2

(defun fare (set)
  "Find best fare solely using either money or pass for all given dates"
  (let ((money (reduce #'+ set :key #'money-fare))
        (pass  (let ((days (1+ (- (car (last set)) (first set)))))
                 (if (<= days 7)
                   2770
                   (* 311 (min 325 (max 28 days)))))))
;; todo: SHOULD have :either key as well if no difference
    (if (< pass money)
      (list pass :pass)
      (list money :money))))

(defun best-fare (set)
  (loop with lowest = 0
        with ranges = nil
        for i from (length set) downto 1
        for head = (subseq set 0 i)
        for tail = (nthcdr i set)
        for (tail-fare tail-ranges) = (best-fare tail)
        for (head-fare head-fare-type) = (fare head)
        for total-fare = (+ head-fare tail-fare)
        if (or (zerop lowest) (< total-fare lowest)) do
          (setq lowest total-fare
                ranges (list* head-fare-type head tail-ranges))
        finally (return (list lowest ranges))))

(setf (fdefinition 'best-fare) (memoize #'best-fare))

(defun ibest-fare (set)
  (car (best-fare set)))
(defun run-tests ()
  (assert (= 101075 (ibest-fare (loop for i from 0 below 365 collect i))))
  (assert (= 101075 (ibest-fare (loop for i from 0 below 325 collect i))))
  (assert (= 46650 (ibest-fare (loop for i from 0 below 150 collect i))))
  (assert (= 2216 (ibest-fare (list 1 2 8 9))))
  (assert (= 1992 (ibest-fare (list 1 5 8 9))))
  (assert (= 0 (ibest-fare nil)))
  (assert (= 554 (ibest-fare (list 3)))))

;(run-tests)

(defun p (&rest n)
  (time (best-fare n)))
