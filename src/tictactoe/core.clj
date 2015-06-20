(ns tictactoe.core
  (:gen-class))

(def cross "X")
(def circle "O")
(def empty-field " ")

(defrecord Board [size fields])
(defrecord MoveResult [board moved])

(defn empty-board [size]
  (let [empty-fields (zipmap (range 0 (* size size)) (repeat (* size size) empty-field))]
    (->Board size empty-fields)))

(defn board-fields-count [board]
  (let [size (:size board)]
    (* size size)))

(defn coords-to-index [board row col]
  (+ col (* row (:size board))))
(defn get-board-elem [board row col]
  (get (:fields board) (coords-to-index board row col)))


(defn is-empty-field [board row col]
  (let [index (coords-to-index board row col)]
    (= empty-field (get (:fields board) index))))

(defn put-field [board row col value]
  (->Board (:size board) (assoc (:fields board) (coords-to-index board row col) value)))
(defn put-cross [board row col]
  (put-field board row col cross))
(defn put-circle [board row col]
  (put-field board row col circle))

(defn -main
  [& args]
  (let [start-board (empty-board 3)]
    (println start-board)))
