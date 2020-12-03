(defpackage #:aoc2020.day1
  (:use #:cl)
  (:import-from #:arrows #:-<> #:->>)
  (:local-nicknames (#:tu #:travv0.utils)
                    (#:a #:alexandria)))

(in-package #:aoc2020.day1)

(defun main (&key (part 2))
  (let ((input (->> (a:read-file-into-string "input.txt")
                    str:lines
                    (mapcar #'parse-integer))))
    (reduce #'*
            (ccase part
              (1 (find-nums-that-sum-to 2020 2 input))
              (2 (find-nums-that-sum-to 2020 3 input))))))

(defun find-nums-that-sum-to (sum n nums)
  (-<> nums
       (tu:combinations n)
       (find-if (lambda (ns) (= (reduce #'+ ns) sum)) <>)))
