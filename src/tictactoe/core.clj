(ns tictactoe.core
  (:gen-class))

(require '[clojure.string :as str])

(def cross "X")
(def circle "O")
(def empty-field " ")

(defrecord Board [size fields])
(defrecord Movement [mark row col])

(defn empty-board 
  [size]
  (let [empty-fields (zipmap (range 0 (* size size)) (repeat (* size size) empty-field))]
    (->Board size empty-fields)))

(defn coords-to-index 
  [board row col]
  (+ col (* row (:size board))))

(defn get-board-elem 
  [board row col]
  (get (:fields board) (coords-to-index board row col)))

(defn is-field-empty? 
  [board row col]
  (let [index (coords-to-index board row col)]
    (= empty-field (get (:fields board) index))))

(defn update-fields-in-board 
  [board row col value]
  (assoc (:fields board) (coords-to-index board row col) value))

(defn update-board 
  [board row col value]
  (->Board (:size board) (update-fields-in-board board row col value)))

(defn put-field 
  [board row col value]
    (update-board board row col value))

(defn put-cross 
  [board row col]
  (put-field board row col cross))

(defn put-circle 
  [board row col]
  (put-field board row col circle))

(defn print-board 
  [board]
  (let [keys (reverse (keys (:fields board)))]
    (doseq [row (partition (:size board) keys)]
      (let [values (map (fn [f] (get (:fields board) f)) row)
            row-printable (clojure.string/join " | " values)]
        (println row-printable)))))

(defn movement 
  [mark movement-string]
  (let [coords (str/split movement-string #" ")
        row (Integer/parseInt (get coords 0))
        col (Integer/parseInt (get coords 1))]
          (->Movement mark row col)))

(defn prompt-move
  []
  (println "Enter coordinates for move (row col): ")
  (let [input (str/trim (read-line))]
    input))

(defn game-loop
  [board]
  (let [move (prompt-move)
        player-movement (movement cross move)
        board-after-move (put-cross board (:row player-movement) (:col player-movement))]
          (print-board board-after-move)
          (recur board-after-move)))

(defn -main
  [& args]
  (let [start-board (empty-board 3)]
    (print-board start-board)
    (game-loop start-board)))
