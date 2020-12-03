(defpackage #:aoc2020.day3
  (:use #:cl)
  (:import-from #:travv0.utils #:fn)
  (:local-nicknames (#:a #:alexandria)))

(in-package #:aoc2020.day3)

(defun main (&key (part 2))
  (let* ((grid (parse-input (a:read-file-into-string "input.txt"))))
    (ccase part
      (1 (count-trees-on-path grid 3 1))
      (2 (let ((slopes '((1 . 1) (3 . 1) (5 . 1) (7 . 1) (1 . 2))))
           (reduce (fn (total (dx . dy))
                     (* total (count-trees-on-path grid dx dy)))
                   slopes
                   :initial-value 1))))))

(defun parse-input (input)
  (let ((lines (str:lines input)))
    (make-array (list (length lines) (length (first lines)))
                :initial-contents lines)))

(defun count-trees-on-path (grid dx dy)
  (loop with count = 0
        for x = 0 then (+ x dx)
        and y = 0 then (+ y dy)
        for square = (get-square grid x y)
        while square
        if (char= square #\#)
          do (incf count)
        finally (return count)))

(defun get-square (grid x y)
  (when (< y (array-dimension grid 0))
    (aref grid y (mod x (array-dimension grid 1)))))
