(ns tictactoe.ai-test
  (:require [clojure.test :refer :all]
            [tictactoe.board :refer :all]
            [tictactoe.game :refer :all]
            [tictactoe.ai :refer :all]))

(deftest generate-all-moves
  (testing "Should generate all possible moves for player and board"
    (let [board (empty-board 3)
      	  board-moved (put-cross board 1 1)
      	  player (->Player circle)
      	  possible-moves (generate-all-possible-moves board-moved player)
          possible-boards (map (fn [move] (put-field board-moved (:row move) (:col move) (:mark player))) possible-moves)
      	  fields-for-possible-moves (map (fn [board] (:fields board)) possible-boards)]
  	  (is (= true (some (fn [fields] (= circle (get fields 8))) fields-for-possible-moves)))
  	  (is (= nil (get fields-for-possible-moves 4)))
      (is (= 8 (count possible-boards))))))

(deftest generate-moves-2
	(testing "Fooling arount minimax"
    (let [start-board (empty-board 3)
          board (put-circle (put-cross (put-cross (put-cross (put-circle (put-circle start-board 2 2) 2 1) 2 0) 1 0) 0 2) 0 0)
          player (->Player cross)
          moves (generate-all-possible-moves board player)]
      (doseq [move moves] 
        (let [board-for-move (put-field board (:row move) (:col move) (:mark player))
              game-won (check-if-win? board-for-move (:mark player))]
          (do
            (println (str "game-won " game-won))
            (println move)))))))