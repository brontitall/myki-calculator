#|
 | Calculate the best myki fare for an ordered set of dates
 | Reference brute force solution. Try every possible combination of money and passes.
 |
 | Dates are days from reference date; 0 = (saturday <= today)
 | Dates must be within 365 days of each other
 |#

(defparameter *weekend-cap* 330
  "Cost of the myki money weekend daily cap")

(defparameter *long-pass-min-days* 28
  "Minimum length a long pass")

(defparameter *long-pass-max-days* 325
  "Maximum chargable days for a long pass")

(defparameter *weekday-fares*
  '(:full (:zone1 656 :zone2 452 :zone12 1108)
    :concession (:zone1 328 :zone2 226 :zone12 554))
  "Weekday fare values as nested plists

Current 10/8/2012 - http://www.myki.com.au/Fares/Metro-fares")

(defparameter *7-day-pass-fares*
  '(:full ( :zone1 3280 :zone2 2260 :zone12 5540)
    :concession ( :zone1 1640 :zone2 1130 :zone12 2770))
   "7 day pass values as nested plists

 Current 10/8/2012 - http://www.myki.com.au/Fares/Metro-fares")

(defparameter *long-pass-fares*
  '(:full ( :zone1 402 :zone2 268 :zone12 622)
    :concession ( :zone1 201 :zone2 134 :zone12 311))
  "Long pass (>= 28 day) per-day values as nested plists

 Current 10/8/2012 - http://www.myki.com.au/Fares/Metro-fares")

(defun get-cost (set type zone)
  (getf (getf set type) zone))

(defvar *weekday-cost* 
  (get-cost *weekday-fares* :full :zone1)
  "Cost of a myki money weekday daily")

(defvar *7-day-pass-cost*
  (get-cost *7-day-pass-fares* :full :zone1)
  "Cost of a 7-day pass")

(defvar *long-pass-day-cost*
  (get-cost *long-pass-fares* :full :zone1)
  "Daily rate for 28 to 365-day myki pass")

(defmacro with-costs-for (type zone &body body)
  `(let ((*weekday-cost* ,(get-cost *weekday-fares* type zone))
	 (*7-day-pass-cost* ,(get-cost *7-day-pass-fares* type zone))
	 (*long-pass-day-cost* ,(get-cost *long-pass-fares* type zone)))
     (re-memoize)
     ,@body))

(defun long-pass-cost (days)
  (* *long-pass-day-cost*
     (min *long-pass-max-days* 
	  (max *long-pass-min-days*
	       days))))

(defun memoize (fn)
  ;; (declare (optimize (speed 3) (safety 0) (debug 0)))
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
    ((0 1) *weekend-cap*)
    (t *weekday-cost*)))

(declaim (inline fare))
(defun fare (set)
  "Find best fare solely using either money or pass for all given dates"
  ;; (declare (optimize (speed 3) (safety 0) (debug 0)))
  (let ((days (1+ (- (the (unsigned-byte 9) (car (last set))) (the (unsigned-byte 9) (first set))))))
    (declare (type (unsigned-byte 9) days))
    (let ((money (loop for i in set sum (the (unsigned-byte 17) (money-fare i))))
          (pass (if (<= days 7)
		    *7-day-pass-cost*
		    (long-pass-cost days))))
      (declare (type (unsigned-byte 17) money pass))
      ;; todo: SHOULD have :either key as well if no difference
      (if (< pass money)
	  (values pass :pass)
	  (values money :money)))))

(defun best-fare* (set)
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
          (destructuring-bind (tail-fare . tail-ranges)
              (best-fare tail)
            (declare (type (unsigned-byte 17) tail-fare)
                     (type list tail-ranges))
            (let ((total-fare (+ head-fare tail-fare)))
              (declare (type (unsigned-byte 17) total-fare))
              (if (or (zerop lowest) (< total-fare lowest))
                (psetq lowest total-fare
                       ranges (list* head-fare-type head tail-ranges))))))
          finally (return (cons lowest ranges))))

(defun re-memoize ()
  (setf (fdefinition 'best-fare) (memoize #'best-fare*)))
(re-memoize)

(defun ibest-fare (set)
  (car (best-fare set)))

(defun range (n)
  (loop for i from 1 to n collecting i))

(defun weekdays (n) 
  (loop for i from 1 to n 
     when (> (rem i 7) 1) collecting i))

(defun run-tests ()
  (with-costs-for :full :zone1
    (assert (= (ibest-fare (weekdays 25)) (ibest-fare (weekdays 26))))
    (assert (equal '(:pass :pass :pass :pass :money)
		   (remove-if-not 
		    #'(lambda (x) (typep x 'keyword)) 
		    (with-costs-for :full :zone1  (best-fare (weekdays 150)))))))
  (with-costs-for :concession :zone12
    (assert (= 101075 (ibest-fare (loop for i from 0 below 365 collect i))))
    (assert (= 101075 (ibest-fare (loop for i from 0 below 325 collect i))))
    (assert (= 46650 (ibest-fare (loop for i from 0 below 150 collect i))))
    (assert (= 1768 (ibest-fare (list 1 2 8 9))))
    (assert (= 1768 (ibest-fare (list 1 5 8 9))))
    (assert (= 0 (ibest-fare nil)))
    (assert (= 554 (ibest-fare (list 3))))))

;(run-tests)

(defun p (&rest n)
  (time (best-fare n)))

