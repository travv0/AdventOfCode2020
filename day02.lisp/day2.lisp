(defpackage #:aoc2020.day2
  (:use #:cl)
  (:import-from #:bind #:bind)
  (:import-from #:arrows #:->>)
  (:local-nicknames (#:tu #:travv0.utils)
                    (#:a #:alexandria)))

(in-package #:aoc2020.day2)

(defstruct policy min max char)

(defun main (&key (part 2))
  (let ((input (->> (a:read-file-into-string "input.txt")
                    str:lines
                    (mapcar #'parse-line))))
    (ccase part
      (1 (count-if #'password-valid-part1 input))
      (2 (count-if #'password-valid-part2 input)))))

(defun parse-line (line)
  (bind (((min-max char* password) (str:words line))
         ((min max) (mapcar #'parse-integer (str:split #\- min-max)))
         (char (char char* 0)))
    (cons password (make-policy :min min :max max :char char))))

(tu:fn password-valid-part1 ((password . policy))
  (with-slots (char min max) policy
    (let ((count (count-if (a:curry #'char= char)
                           password)))
      (<= min count max))))

(tu:fn password-valid-part2 ((password . policy))
  (with-slots (char min max) policy
    (flet ((index-is-char (index)
             (char= char (char password (1- index)))))
      (a:xor (index-is-char min) (index-is-char max)))))
