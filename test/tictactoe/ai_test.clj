(ns tictactoe.ai-test
  (:require [clojure.test :refer :all]
            [tictactoe.board :refer :all]
            [tictactoe.game :refer :all]
            [tictactoe.ai :refer :all]))

(deftest generate-all-moves
  (testing "Should generate all possible moves for player and board"
    (let [board (empty-board 3)
      	  board-moved (put-cross board 1 1)
      	  player (create-player circle)
      	  possible-moves (generate-all-possible-moves board-moved player)
          possible-boards (map (fn [move] (put-field board-moved (:row move) (:col move) (:mark player))) possible-moves)
      	  fields-for-possible-moves (map (fn [board] (:fields board)) possible-boards)]
  	  (is (= true (some (fn [fields] (= circle (get fields 8))) fields-for-possible-moves)))
  	  (is (= nil (get fields-for-possible-moves 4)))
      (is (= 8 (count possible-boards))))))

(deftest minimax-test
  (testing "Should choose move with score 10 for ai player"
    (let [start-board (empty-board 3)
          ai-player (create-player cross)
          board (put-circle (put-cross (put-cross (put-cross (put-circle (put-circle start-board 2 2) 2 1) 2 0) 1 0) 0 2) 0 0)
          chosen (choose-move board ai-player)
          score (:scores chosen)
          move (:move chosen)]
      (is (= score 10))
      (is (= move (create-movement cross 1 1))))))

(deftest minimax-test-depth
  (testing "Should choose best move"
    (let [start-board (empty-board 3)
          ai-player (create-player cross)
          board (put-circle (put-circle (put-circle (put-cross (put-cross start-board 2 1) 2 0) 2 2) 1 2) 0 1)
          chosen (choose-move board ai-player)
          score (:scores chosen)
          move (:move chosen)]
      (is (= score -7))
      (is (= move (create-movement cross 0 2))))))
