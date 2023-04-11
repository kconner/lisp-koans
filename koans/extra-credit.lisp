;;; Copyright 2013 Google Inc.
;;;
;;; Licensed under the Apache License, Version 2.0 (the "License");
;;; you may not use this file except in compliance with the License.
;;; You may obtain a copy of the License at
;;;
;;;     http://www.apache.org/licenses/LICENSE-2.0
;;;
;;; Unless required by applicable law or agreed to in writing, software
;;; distributed under the License is distributed on an "AS IS" BASIS,
;;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;; See the License for the specific language governing permissions and
;;; limitations under the License.

;;; EXTRA CREDIT:
;;;
;;; Create a program that will play the Greed game.
;;; The full rules for the game are in the file extra-credit.txt.
;;;
;;; You already have a DICE-SET class and a score function you can use.
;;; Write a PLAYER class and a GAME class to complete the project.
;;;
;;; This is a free form assignment, so approach it however you desire.

#|
= Playing Greed

Greed is a dice game played among 2 or more players, using 5
six-sided dice.

== Playing Greed

Each player takes a turn consisting of one or more rolls of the dice.
On the first roll of the game, a player rolls all five dice which are
scored according to the following:

  Three 1's => 1000 points
  Three 6's =>  600 points
  Three 5's =>  500 points
  Three 4's =>  400 points
  Three 3's =>  300 points
  Three 2's =>  200 points
  One   1   =>  100 points
  One   5   =>   50 points

A single die can only be counted once in each roll.  For example,
a "5" can only count as part of a triplet (contributing to the 500
points) or as a single 50 points, but not both in the same roll.

Example Scoring

   Throw       Score
   ---------   ------------------
   5 1 3 4 1   50 + 2 * 100 = 250
   1 1 1 3 1   1000 + 100 = 1100
   2 4 4 5 4   400 + 50 = 450

The dice not contributing to the score are called the non-scoring
dice.  "3" and "4" are non-scoring dice in the first example.  "3" is
a non-scoring die in the second, and "2" is a non-score die in the
final example.

After a player rolls and the score is calculated, the scoring dice are
removed and the player has the option of rolling again using only the
non-scoring dice. If all of the thrown dice are scoring, then the
player may roll all 5 dice in the next roll.

The player may continue to roll as long as each roll scores points. If
a roll has zero points, then the player loses not only their turn, but
also accumulated score for that turn. If a player decides to stop
rolling before rolling a zero-point roll, then the accumulated points
for the turn is added to his total score.

== Getting "In The Game"

Before a player is allowed to accumulate points, they must get at
least 300 points in a single turn. Once they have achieved 300 points
in a single turn, the points earned in that turn and each following
turn will be counted toward their total score.

== End Game

Once a player reaches 3000 (or more) points, the game enters the final
round where each of the other players gets one more turn. The winner
is the player with the highest score after the final round.

== References

Greed is described on Wikipedia at
http://en.wikipedia.org/wiki/Greed_(dice_game), however the rules are
a bit different from the rules given here.
|#
  
(defclass greed-game ()
  ((players :initarg :players :accessor game-players)
   (current-player :initarg :current-player :accessor game-current-player)
   (dice-set :initarg :dice-set :accessor game-dice-set)))

(defclass greed-player ()
  ((name :initarg :name :accessor player-name)
   (score :initarg :score :accessor player-score)))

(defclass greed-dice-set ()
  ((dice :initarg :dice :accessor dice-set-dice)))

(defun make-greed-dice-set ()
  (make-instance 'greed-dice-set :dice (make-list 5 :initial-element 0)))

(defun make-greed-player (name)
  (make-instance 'greed-player :name name :score 0))

(defun make-greed-game (players)
  (make-instance 'greed-game :players players
                 :current-player (first players)
                 :dice-set (make-greed-dice-set)))

(defun roll-dice-set (dice-set)
  (setf (dice-set-dice dice-set) (mapcar #'(lambda (&rest unused) (1+ (random 6))) (dice-set-dice dice-set))))

(defun score-dice-set (dice-set)
  (let ((dice-counts (make-array 7))
        (score 0))
    ;; Count the number of times each die value appears.
    (dolist (die (dice-set-dice dice-set))
      (incf (aref dice-counts die)))
    ;; Score the dice.
    (if (>= (aref dice-counts 1) 3) (incf score 1000))
    (if (>= (aref dice-counts 6) 3) (incf score 600))
    (if (>= (aref dice-counts 5) 3) (incf score 500))
    (if (>= (aref dice-counts 4) 3) (incf score 400))
    (if (>= (aref dice-counts 3) 3) (incf score 300))
    (if (>= (aref dice-counts 2) 3) (incf score 200))
    (incf score (* 100 (mod (aref dice-counts 1) 3)))
    (incf score (* 50 (mod (aref dice-counts 5) 3)))
    ;; Print the dice and the score.
    (format t "~{~A ~}~%Score: ~A~%" (dice-set-dice dice-set) score)
    ;; Return the score.
    score))

(defun play-a-game ()
  (let ((game (make-greed-game (list (make-greed-player "Alice")
                                     (make-greed-player "Bob")))))
    ;; Print the players' names to begin.
    (format t "Playing Greed: ~{~A ~}~%" (mapcar #'player-name (game-players game)))
    ;; While no user has triggered the final round, play a turn.
    (loop until (final-round-p game)
          do (play-a-turn game))
    ;; In the final round, each of the other players gets one more turn.
    (dotimes (i (1- (length (game-players game))))
      (play-a-turn game))
    ;; The winner is the player with the highest score after the final round.
    (loop for player in (game-players game)
          maximize (player-score player) into winner-score
          finally (return (find winner-score (game-players game) :key #'player-score)))))

(defun final-round-p (game)
  (some #'(lambda (player) (>= (player-score player) 3000))
        (game-players game)))

(defun play-a-turn (game)
  (let ((player (game-current-player game))
        (dice-set (game-dice-set game)))
    ;; Announce the turn.
    (format t "~A's turn.~%" (player-name player))
    ;; Roll the dice.
    (roll-dice-set dice-set)
    ;; Score the dice.
    (let ((score (score-dice-set dice-set)))
      (setf (player-score player) (+ (player-score player) score))
      ;; Announce the player's score.
      (format t "~A's score: ~A~%" (player-name player) (player-score player))
      (setf (game-current-player game)
            (nth (mod (1+ (position player (game-players game) :test #'eq))
                      (length (game-players game)))
                 (game-players game))))))

(define-test play-greed
             (play-a-game)
             (assert-true t))
