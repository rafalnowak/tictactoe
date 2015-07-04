(ns tictactoe.core
  (:gen-class))

(require '[clojure.string :as str]
         '[tictactoe.board :as board])

(defrecord Movement [mark row col])

(defn prompt-move
  []
  (println "Enter coordinates for move (row col): ")
  (let [input (str/trim (read-line))]
    input))

(defn create-movement 
  [mark movement-string]
  (let [coords (str/split movement-string #" ")
        row (Integer/parseInt (get coords 0))
        col (Integer/parseInt (get coords 1))]
          (->Movement mark row col)))

;; TODO: parametrized players symbol and players changing
(defn attemp-move
  [board]
  (let [move-coords (prompt-move)
        player-movement (create-movement board/cross move-coords)
        row (:row player-movement)
        col (:col player-movement)]
          (if (board/is-field-empty? board row col) 
            (board/put-cross board row col)
            (do
              (println "Illegal move, try again") 
              (recur board)))))

(defn game-loop
  [board]
    (let [board-after-move (attemp-move board)]
      (board/print-board board-after-move)
      (recur board-after-move)))

(defn -main
  [& args]
  (let [start-board (board/empty-board 3)]
    (board/print-board start-board)
    (game-loop start-board)))
