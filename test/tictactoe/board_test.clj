(ns tictactoe.board-test
  (:require [clojure.test :refer :all]
            [tictactoe.board :refer :all]))

(deftest board
  (testing "Board should have correct size"
    (let [board (empty-board 3)
          size (count (keys (:fields board)))]
      (is (= 9 size)))))

(deftest board-is-empty-field
  (testing "Board should have empty field when nothing was put on it"
    (let [board (empty-board 3)]
      (is (= empty-field (field-at board 1 1))))))

(deftest board-put
  (testing "Board should have field changed after putting cross"
    (let [board-after-move (put-cross (empty-board 3) 1 1)]
      (is (= cross (field-at board-after-move 1 1))))))

(deftest board-is-field-empty
  (testing "Board field with no symbol should be empty")
    (let [board (empty-board 3)]
      (is (= true (is-field-empty? board 1 1)))))

(deftest board-win-conditions-rows
  (testing "Check winning conditions in rows")
    (let [board (empty-board 3)
          board-with-winning-row (put-cross (put-cross (put-cross board 0 2) 0 1) 0 0)]
        (is (= true (check-if-win? board-with-winning-row cross)))))

(deftest board-win-conditions-rows-no-win
  (testing "Not inform about win when conditions are not met")
    (let [board (empty-board 3)]
        (is (= false (check-if-win? board cross)))))

(deftest board-win-conditions-cols
  (testing "Check winning conditions in cols")
    (let [board (empty-board 3)
          board-with-winning-col (put-cross (put-cross (put-cross board 2 0) 1 0) 0 0)]
        (is (= true (check-if-win? board-with-winning-col cross)))))

(deftest board-win-conditions-diag-south-east
  (testing "Check winning conditions in diag south east")
    (let [board (empty-board 3)
          board-with-winning-col (put-cross (put-cross (put-cross board 2 2) 1 1) 0 0)]
        (is (= true (check-if-win? board-with-winning-col cross)))))

(deftest board-win-conditions-diag-south-west
  (testing "Check winning conditions in diag south west")
    (let [board (empty-board 3)
          board-with-winning-col (put-cross (put-cross (put-cross board 2 0) 1 1) 0 2)]
        (is (= true (check-if-win? board-with-winning-col cross)))))

(deftest board-all-empty-fields
  (testing "Should return all empty fields as map")
    (let [board (empty-board 3)
          board-after-move (put-cross board 1 1)]
        (is (= 8 (count (all-empty-fields board-after-move))))))

(deftest board-draw
  (testing "Should test for draw on board")
    (let [board (->Board 3 {0 circle 1 cross 2 circle 3 circle 4 cross 5 circle 6 cross 7 circle 8 cross})]
      (is (= true (draw? board)))))

(deftest board-no-draw
  (testing "Should not return true for draw when board is filled but one of players won")
    (let [board (->Board 3 {0 cross 1 cross 2 circle 3 circle 4 cross 5 circle 6 cross 7 circle 8 cross})]
      (is (= false (draw? board)))))
