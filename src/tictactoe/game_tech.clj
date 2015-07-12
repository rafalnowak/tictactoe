(ns tictactoe.game-tech
  (:gen-class))

(require '[tictactoe.board :as board])

;; TODO: player in separate module
;; and protocol for player for better ai integration
(defrecord Player [mark])

(defn create-player
  [mark]
  (->Player mark))

(defrecord Movement [mark row col])

(defn create-movement
  [mark row col]
  (->Movement mark row col))

;; TODO: stronger coupling with player so no if will be necessary
(defn opponent-player
  [player]
  (if (= board/cross (:mark player))
    (->Player board/circle)
    (->Player board/cross)))

(defn game-over?
  [board player]
  (or
    (board/draw? board)
    (board/check-if-win? board (:mark player))))
