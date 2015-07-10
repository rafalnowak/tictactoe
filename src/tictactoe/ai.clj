(ns tictactoe.ai
  (:gen-class))

(require '[clojure.string :as str]
         '[tictactoe.board :as board]
         '[tictactoe.game :as game])

;; TODO: configurable ai player
(def ai-player (game/create-player board/cross))

(defn apply-move-to-board
  [board move]
  (board/put-field board (:row move) (:col move) (:mark move)))

(defn score-move
  [board move player]
  (let [board-applied (apply-move-to-board board move)]
    (if (board/check-if-win? board-applied (:mark player))
      10
      (if (board/check-if-win? board-applied (:mark (game/opponent-player player)))
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
