(ns tictactoe.core
  (:gen-class))

(require '[clojure.string :as str]
         '[tictactoe.board :as board])

(defrecord Movement [mark row col])

(defn movement 
  [mark movement-string]
  (let [coords (str/split movement-string #" ")
        row (Integer/parseInt (get coords 0))
        col (Integer/parseInt (get coords 1))]
          (->Movement mark row col)))

(defn prompt-move
  []
  (println "Enter coordinates for move (row col): ")
  (let [input (str/trim (read-line))]
    input))

(defn game-loop
  [board]
  (let [move (prompt-move)
        player-movement (movement board/cross move)
        board-after-move (board/put-cross board (:row player-movement) (:col player-movement))]
          (board/print-board board-after-move)
          (recur board-after-move)))

(defn -main
  [& args]
  (let [start-board (board/empty-board 3)]
    (board/print-board start-board)
    (game-loop start-board)))
