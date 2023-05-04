;; Second challenge: Rock Paper Scissors

(defconstant *aoc-2-input-file* "../inputs/2")

(defun aoc-2-convert-to-symbol (move offset)
  "Converts a move to a symbol. The offset is used to convert the move to the correct symbol."
  (let ((move-idx (- (char-code move) (char-code offset))))
    (case move-idx
      (0 'rock)
      (1 'paper)
      (2 'scissors))))

(defun aoc-2-read-input (path)
  "Reads the challenge input and return an association list with the rock-paper-scissors playbook.
Each element of the a-list will look like this `(rock . paper)' where the car is the opponent's
move and the cdr is the move that beats it."
  (let (playbook '())
    (with-open-file (in path)
      (loop for line = (read-line in nil nil)
            while line
            do (let* ((opp (aoc-2-convert-to-symbol (aref line 0) #\A))
                      (my (aoc-2-convert-to-symbol (aref line 2) #\X)))
                 (push (cons opp my) playbook))))
    playbook))

(defun aoc-2-play (opponent-move my-move)
  "Returns the result of a play.
The result is 'WIN, 'TIE or 'LOSE"
  (cond ((eq opponent-move my-move) 'tie)
        ((eq opponent-move 'rock)
         (case my-move
           ('paper 'win)
           ('scissors 'lose)))
        ((eq opponent-move 'paper)
         (case my-move
           ('rock 'lose)
           ('scissors 'win)))
        ((eq opponent-move 'scissors)
         (case my-move
           ('rock 'win)
           ('paper 'lose)))))

(defun aoc-2-play-score (play)
  "Returns the score of a play.
The score is 1 for rock, 2 for paper and 3 for scissors."
  (case play
    ('rock 1)
    ('paper 2)
    ('scissors 3)))

(defun aoc-2-result-score (result)
  "Returns the score of a result.
The score is 6 if the player wins, 3 if it's a tie and 0 if the player loses."
  (case result
    ('win 6)
    ('tie 3)
    ('lose 0)))

(defun aoc-2-calc-turn-score (turn)
  "Calculates the score of a turn.
The score is the sum of the play the player made and the result of the turn.
The result is 6 if the player wins, 3 if it's a tie and 0 if the player loses.
And the play score is 1 for rock, 2 for paper and 3 for scissors."
  (let ((play (cdr turn))
        (result (aoc-2-play (car turn) (cdr turn))))
    (+ (aoc-2-play-score play) (aoc-2-result-score result))))

; Solve the problem - part 1
(let ((playbook (aoc-2-read-input *aoc-2-input-file*)))
  (print (reduce #'+ (mapcar #'aoc-2-calc-turn-score playbook))))

;;; Part 2

(defun aoc-2-get-my-move (result opponent-move)
  "Returns the move that satisfies the result against the opponent's move."
  (let ((parsed-result (case result
                  (#\X 'lose)
                  (#\Y 'tie)
                  (#\Z 'win))))
    (case parsed-result
      ('win (case opponent-move
              ('rock 'paper)
              ('paper 'scissors)
              ('scissors 'rock)))
      ('tie opponent-move)
      ('lose (case opponent-move
               ('rock 'scissors)
               ('paper 'rock)
               ('scissors 'paper))))))

(defun aoc-2-read-input-2 (path)
  "Reads the challenge input and return an association list with the rock-paper-scissors playbook.
Each element of the a-list will look like this `(rock . paper)' where the car is the opponent's
move and the cdr is the move that beats it."
  (let (playbook '())
    (with-open-file (in path)
      (loop for line = (read-line in nil nil)
            while line
            do (let* ((opp (aoc-2-convert-to-symbol (aref line 0) #\A))
                      (my (aoc-2-get-my-move (aref line 2) opp)))
                 (push (cons opp my) playbook))))
    playbook))

; Solve the problem - part 2
(let ((playbook (aoc-2-read-input-2 *aoc-2-input-file*)))
  (print (reduce #'+ (mapcar #'aoc-2-calc-turn-score playbook))))
