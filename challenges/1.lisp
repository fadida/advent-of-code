;; First challenge: Get the max number of calories that one elf is carring

(defconstant *aoc-1-input-file* "../inputs/1")

(defun aoc-1-read-input (path)
  "Reads the challenge input and return a list of
   calories for each elf."
  (let ((elves-calories '())
        (calories '()))
    (with-open-file (in path)
      (loop for line = (read-line in nil nil)
            while line
            do (if (eq (length line) 0)
                   (progn (setq elves-calories (nconc elves-calories (list (copy-seq calories))))
                          (setq calories '()))
                   (push (parse-integer line) calories))))
    elves-calories))

(defun aoc-1-find-elf-with-max-cal (elves-calories)
  "Get the elf number that carries the maximum calories number"
  (loop for cals in elves-calories
        collect (loop for cal in cals
                      summing cal) into elf-list
        finally (return (loop for cal in elf-list
                              maximize cal))))

(defun aoc-1-top3 (&rest elems)
  "Returns the top 3 results form `ELEMS'."
  (let ((sorted (sort elems #'>)))
    (list (first sorted) (second sorted) (third sorted))))

(defun aoc-1-find-top-3-elves (elves-calories)
  "Get the elf number that carries the maximum calories number"
  (loop for cals in elves-calories
        collect (loop for cal in cals
                      summing cal) into elf-list
        finally (return (apply #'+ (apply #'aoc-2-top3 elf-list)))))

(let ((elves-calories (aoc-1-read-input *aoc-1-input-file*)))
  ;; Solve AoC day 1 - Part 1
  (format t "~D," (aoc-1-find-elf-with-max-cal elves-calories))
  ;; Solve AoC day 1 - Part 2
  (format t "~D" (aoc-1-find-top-3-elves elves-calories)))
