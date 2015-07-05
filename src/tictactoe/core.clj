(ns tictactoe.core
  (:gen-class))

(require '[clojure.string :as str]
         '[tictactoe.board :as board])

;; TODO: better ways to create constants, maybe some enums?
(def game-running "RUNNING")
(def x-win "X-WIN")
(def o-win "O-WIN")

(defrecord Movement [mark row col])
(defrecord BoardStatus [board game-status])

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
            (let [board (board/put-cross board row col)]
              (if (board/check-if-win? board board/cross)
                (->BoardStatus board x-win) 
                (->BoardStatus board game-running))) 
            (do
              (println "Illegal move, try again") 
              (recur board)))))

(defn game-loop
  [board]
    (let [board-after-move (attemp-move board)
          board (:board board-after-move)
          status (:game-status board-after-move)]
      (if (= x-win status)
        (do 
          (board/print-board board)
          (println (str "Game over! " status)))
        (do
          (board/print-board board)
          (recur board)))))

(defn -main
  [& args]
  (let [start-board (board/empty-board 3)]
    (board/print-board start-board)
    (game-loop start-board)))
