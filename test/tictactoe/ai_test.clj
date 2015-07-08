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
    	  possible-boards (generate-all-possible-moves board-moved player)
    	  fields-for-possible-moves (map (fn [board] (:fields board)) possible-boards)]
	  (is (= true (some (fn [fields] (= circle (get fields 8))) fields-for-possible-moves)))
	  (is (= nil (get fields-for-possible-moves 4)))
      (is (= 8 (count possible-boards))))))