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

(defn minimax
  [board current-player level]
  (let [possible-moves (generate-all-possible-moves board current-player)]
    (doseq [move possible-moves]
      (let [board-applied (apply-move-to-board board move)
            score (score-move board move current-player)
            print-test (do (println (str "Level: " level ", score: " score)) (board/print-board board-applied) (println ""))]
        (if (board/game-over? board-applied current-player)
          ()
          (minimax board-applied (game/opponent-player current-player) (+ 1 level)))))))
