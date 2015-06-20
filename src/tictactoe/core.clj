(ns tictactoe.core
  (:gen-class))

(def cross "X")
(def circle "O")
(def empty-field " ")

(defrecord Board [size fields])

(defn empty-board [size] (let [empty-fields (zipmap (range 0 (* size size)) (repeat (* size size) empty-field))]
                           (->Board size empty-fields)))

(defn board-fields-count [board] (let [size (:size board)]
                                   (* size size)))

(defn coords-to-index [board row col] (+ col (* row (:size board))))
(defn get-board-elem [board row col] (get (:fields board) (coords-to-index board row col)))

(defn put-field [board row col value] (->Board (:size board) (assoc (:fields board) (coords-to-index board row col) value)))
(defn put-cross [board row col] (put-field board row col cross))
(defn put-circle [board row col] (put-field board row col circle))

(defn -main
  [& args]
  (println "Hello, World!"))
