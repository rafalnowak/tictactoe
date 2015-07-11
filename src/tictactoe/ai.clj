(ns tictactoe.ai
  (:gen-class))

(require '[clojure.string :as str]
         '[tictactoe.board :as board]
         '[tictactoe.game :as game])

;; TODO: configurable ai player
(def ai-player (game/create-player board/cross))
(def human-player (game/create-player board/circle))

(defn apply-move-to-board
  [board move]
  (board/put-field board (:row move) (:col move) (:mark move)))

;; TODO: configurable player
(defn score-move
  [board move player]
  (let [board-applied (apply-move-to-board board move)]
    (if (board/check-if-win? board-applied (:mark ai-player))
      10
      (if (board/check-if-win? board-applied (:mark human-player))
        -10
        0))))

(defn generate-all-possible-moves
  [board player]
  (let [empty-fields (board/all-empty-fields board)
        mark (:mark player)]
	  (map 
      (fn [field]
        (let [ind (first field)
              coords (board/index-to-coords board ind)
              row (:row coords)
              col (:col coords)]
          (game/create-movement mark row col)))
      empty-fields)))

(defrecord GameStep [move player board-applied score])

(defn minimax
  [board current-player depth acc]
  (let [possible-moves (generate-all-possible-moves board current-player)]
    (map (fn [move]
      (let [board-applied (apply-move-to-board board move)
            score (score-move board move current-player)]
        (if (game/game-over? board-applied current-player)
          acc
          (minimax board-applied 
            (game/opponent-player current-player) 
            (+ 1 depth) 
            (conj acc (->GameStep move current-player board-applied score))))))
    possible-moves)))
