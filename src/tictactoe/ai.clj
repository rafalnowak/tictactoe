(ns tictactoe.ai
  (:gen-class))

(require '[tictactoe.board :as board]
         '[tictactoe.game-tech :as game-tech])

;; TODO: configurable ai player
(def ai-player (game-tech/create-player board/cross))
(def human-player (game-tech/create-player board/circle))

(defrecord GameStep [move player score nodes])

(declare minimax-tree minimax-tree-with-depth generate-all-possible-moves score-move apply-move-to-board)

;; TODO - returns list of nodes instead of real tree - instead it should return real tree with proper root
(defn minimax-tree
  [board current-player]
  (minimax-tree-with-depth board current-player 0))

(defn minimax-tree-with-depth
  [board current-player depth]
  (let [possible-moves (generate-all-possible-moves board current-player)]
    (map (fn [move]
      (let [board-applied (apply-move-to-board board move)
            score (score-move board move current-player depth)
            opponent (game-tech/opponent-player current-player)]
        (if (game-tech/game-over? board-applied current-player)
          (->GameStep move opponent score [])
          (->GameStep move opponent score (minimax-tree-with-depth board-applied opponent (+ 1 depth))))))
    possible-moves)))

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
          (game-tech/create-movement mark row col)))
      empty-fields)))

;; TODO: configurable player
(defn score-move
  [board move player depth]
  (let [board-applied (apply-move-to-board board move)]
    (if (board/check-if-win? board-applied (:mark ai-player))
      (- 10 depth)
      (if (board/check-if-win? board-applied (:mark human-player))
        (- depth 10)
        0))))

(defn apply-move-to-board
  [board move]
  (board/put-field board (:row move) (:col move) (:mark move)))

(declare gather-moves-with-scores gather-scores-iter choose-move reeval-moves-with-scores)

(defrecord MoveWithScores [move player scores])

(defn gather-moves-with-scores
  [tree-node]
  (if (empty? (:nodes tree-node))
    (->MoveWithScores (:move tree-node) (:player tree-node) (list (:score tree-node)))
    (->MoveWithScores (:move tree-node) (:player tree-node) (gather-scores-iter tree-node []))))

(defn gather-scores-iter
  [tree-node acc]
  (if (empty? (:nodes tree-node))
    (conj acc (:score tree-node))
    (concat 
      (conj acc (:score tree-node))
      (flatten (map (fn [n] (gather-scores-iter n acc)) (:nodes tree-node))))))

(defn choose-move
  [board ai-player]
  (let [tree (minimax-tree board ai-player)
        moves-scores (map gather-moves-with-scores tree)
        moves-reeval (map reeval-moves-with-scores moves-scores)
        max-score (apply max (map (fn [move] (:scores move)) moves-reeval))]
    (first (filter (fn [move] (= max-score (:scores move))) moves-reeval))))

;; TODO: condition for end game
(defn reeval-moves-with-scores
  [move-scores]
  (let [player (:player move-scores)
        move (:move move-scores)
        scores (:scores move-scores)]
    (if (= ai-player player)
      (->MoveWithScores move player (apply max scores))
      (->MoveWithScores move player (apply min scores)))))

