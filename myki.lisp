#|
 | Calculate the best myki fare for an ordered set of dates
 | Reference brute force solution. Try every possible combination of money and passes.
 |
 | Dates are days from reference date; 0 = (saturday <= today)
 | Dates must be within 365 days of each other
 |#

(defun memoize (fn)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
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

(declaim (inline money-fare))
(defun money-fare (day)
  (declare (type (unsigned-byte 9) day) (optimize (speed 3) (safety 0) (debug 0)))
  (case (rem day 7)
    ((0 1) 330) ;; weekend daily cap $3.30
    (t 554)))   ;; daily zone 1+2

(declaim (inline fare))
(defun fare (set)
  "Find best fare solely using either money or pass for all given dates"
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (let ((days (1+ (- (the (unsigned-byte 9) (car (last set))) (the (unsigned-byte 9) (first set))))))
    (declare (type (unsigned-byte 9) days))
    (let ((money (loop for i in set sum (the (unsigned-byte 17) (money-fare i))))
          (pass (if (<= days 7)
                  2770
                  (* 311 (min 325 (max 28 days))))))
      (declare (type (unsigned-byte 17) money pass))
      ;; todo: SHOULD have :either key as well if no difference
      (if (< pass money)
        (values pass :pass)
        (values money :money)))))

(defun best-fare (set)
  (declare (type list set) (optimize (safety 0)))
  (loop with lowest = 0
        with ranges = nil
        for i from (length set) downto 1
        for head = (subseq set 0 i)
        for tail = (nthcdr i set)
        do
        (multiple-value-bind (head-fare head-fare-type)
            (fare head)
          (declare (type (unsigned-byte 17) head-fare)
                   (type keyword head-fare-type))
          (multiple-value-bind (tail-fare tail-ranges)
              (best-fare tail)
            (declare (type (unsigned-byte 17) tail-fare)
                     (type list tail-ranges))
            (let ((total-fare (+ head-fare tail-fare)))
              (declare (type (unsigned-byte 17) total-fare))
              (if (or (zerop lowest) (< total-fare lowest))
                (psetq lowest total-fare
                       ranges (list* head-fare-type head tail-ranges))))))
          finally (return (values lowest ranges))))

(setf (fdefinition 'best-fare) (memoize #'best-fare))

(defun run-tests ()
  (assert (= 101075 (best-fare (loop for i from 0 below 365 collect i))))
  (assert (= 101075 (best-fare (loop for i from 0 below 325 collect i))))
  (assert (= 46650 (best-fare (loop for i from 0 below 150 collect i))))
  (assert (= 2216 (best-fare (list 1 2 8 9))))
  (assert (= 1992 (best-fare (list 1 5 8 9))))
  (assert (= 0 (best-fare nil)))
  (assert (= 554 (best-fare (list 3)))))

;(run-tests)

(defun p (&rest n)
  (time (best-fare n)))

