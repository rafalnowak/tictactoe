(ns tictactoe.core-test
  (:require [clojure.test :refer :all]
            [tictactoe.core :refer :all]))

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
    (let [board-after-move (put-cross (empty-board 3) 1 1)
          board (:board board-after-move)]
      (is (= cross (get-board-elem board 1 1))))))
