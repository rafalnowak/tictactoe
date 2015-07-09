(ns tictactoe.ai
  (:gen-class))

(require '[clojure.string :as str]
         '[tictactoe.board :as board]
         '[tictactoe.game :as game])

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