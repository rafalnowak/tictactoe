(ns tictactoe.core-test
  (:require [clojure.test :refer :all]
            [tictactoe.core :refer :all]))

(deftest board
  (testing "Board should have correct size"
    (let [board (empty-board 3)
          size (count (keys (:fields board)))]
      (is (= 9 size)))))
