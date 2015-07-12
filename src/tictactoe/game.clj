(ns tictactoe.game
  (:gen-class))

(require '[clojure.string :as str]
         '[tictactoe.board :as board]
         '[tictactoe.game-tech :as game-tech]
         '[tictactoe.ai :as ai])

;; TODO: better ways to create constants, maybe some enums?
(def game-running "RUNNING")
(def x-win "X-WIN")
(def o-win "O-WIN")

(defrecord GameState [board game-status player-to-move])

(declare game-loop attemp-move prompt-for-move create-movement-from-str winning-status-for-player)

;; TODO: random starting player
(defn -main
  [& args]
  (let [start-board (board/empty-board 3)
        starting-player (game-tech/create-player board/circle)]
    (board/print-board start-board)
    (game-loop start-board starting-player)))

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
          (recur board (game-tech/opponent-player player))))))

(defn attemp-move
  [board player]
  (if (= ai/ai-player player)
    (let [msg (println "\nAI is thinking...")
          ai-move (ai/choose-move board player)
          row (:row (:move ai-move))
          col (:col (:move ai-move))
          board-applied (board/put-field board row col (:mark player))]        
        (if (board/check-if-win? board-applied (:mark player))
          (->GameState board-applied (winning-status-for-player player) player) 
          (->GameState board-applied game-running player)))
    (let [move-coords (prompt-for-move player)
          player-movement (create-movement-from-str (:mark player) move-coords)
          row (:row player-movement)
          col (:col player-movement)]
      (if (board/is-field-empty? board row col) 
        (let [board (board/put-field board row col (:mark player))]
          (if (board/check-if-win? board (:mark player))
            (->GameState board (winning-status-for-player player) player) 
            (->GameState board game-running player))) 
        (do
          (println "Illegal move, try again") 
          (recur board player))))))

(defn prompt-for-move
  [player]
  (println (str "\n" (:mark player) " - enter coordinates for move (row col): "))
  (str/trim (read-line)))

(defn create-movement-from-str
  [mark movement-string]
  (let [coords (str/split movement-string #" ")
        row (Integer/parseInt (get coords 0))
        col (Integer/parseInt (get coords 1))]
    (game-tech/create-movement mark row col)))

;; TODO: stronger coupling with player so no if will be necessary
(defn winning-status-for-player
  [player]
  (if (= board/cross (:mark player))
    x-win
    o-win))
