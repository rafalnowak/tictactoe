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
      (is (= empty-field (get-board-elem board 1 1))))))

(deftest board-put
  (testing "Board should have field changed after putting cross"
    (let [board-after-move (put-cross (empty-board 3) 1 1)]
      (is (= cross (get-board-elem board-after-move 1 1))))))

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