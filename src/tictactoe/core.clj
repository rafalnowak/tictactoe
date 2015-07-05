(ns tictactoe.core
  (:gen-class))

(require '[clojure.string :as str]
         '[tictactoe.board :as board])

;; TODO: better ways to create constants, maybe some enums?
(def game-running "RUNNING")
(def x-win "X-WIN")
(def o-win "O-WIN")

(defrecord Movement [mark row col])
(defrecord GameState [board game-status player-to-move])
(defrecord Player [mark])

;; TODO: stronger coupling with player so no if will be necessary
(defn next-player
  [player]
  (if (= board/cross (:mark player))
    (->Player board/circle)
    (->Player board/cross)))

;; TODO: stronger coupling with player so no if will be necessary
(defn winning-status-for-player
  [player]
  (if (= board/cross (:mark player))
    x-win
    o-win))

(defn prompt-for-move
  [player]
  (println (str (:mark player) " - enter coordinates for move (row col): "))
  (let [input (str/trim (read-line))]
    input))

(defn create-movement 
  [mark movement-string]
  (let [coords (str/split movement-string #" ")
        row (Integer/parseInt (get coords 0))
        col (Integer/parseInt (get coords 1))]
          (->Movement mark row col)))

(defn attemp-move
  [board player]
  (let [move-coords (prompt-for-move player)
        player-movement (create-movement (:mark player) move-coords)
        row (:row player-movement)
        col (:col player-movement)]
          (if (board/is-field-empty? board row col) 
            (let [board (board/put-field board row col (:mark player))]
              (if (board/check-if-win? board (:mark player))
                (->GameState board (winning-status-for-player player) player) 
                (->GameState board game-running player))) 
            (do
              (println "Illegal move, try again") 
              (recur board player)))))

(defn game-loop
  [board player]
    (let [board-after-move (attemp-move board player)
          board (:board board-after-move)
          status (:game-status board-after-move)]
      (if (or (= x-win status) (= o-win status))
        (do 
          (board/print-board board)
          (println (str "Game over! " status)))
        (do
          (board/print-board board)
          (recur board (next-player player))))))

;; TODO: random starting player
(defn -main
  [& args]
  (let [start-board (board/empty-board 3)
        starting-player (->Player board/circle)]
    (board/print-board start-board)
    (game-loop start-board starting-player)))
