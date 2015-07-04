(ns tictactoe.board
  (:gen-class))

(def cross "X")
(def circle "O")
(def empty-field " ")

(defrecord Board [size fields])

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
      (let [values (map (fn [r] (get (:fields board) r)) row)
            row-printable (clojure.string/join " | " values)]
        (println row-printable)))))

(defn fields-coords-by-rows
  [board]
  (let [rows (range 0 (:size board))
        cols (range 0 (:size board))]
    (map (fn [row] (map (fn [col] [row col]) cols)) rows)))

;; returns nil if no row matches predicate (strange clojure library behaviour?)
;; TODO: check cols and diagonals
(defn check-if-win?
  [board mark]
  (let [coords (fields-coords-by-rows board)]
    (some 
      (fn 
        [row] 
        (every? 
          (fn 
            [field] 
            (let [row (nth field 0)
                  col (nth field 1)]
              (= mark (get-board-elem board row col)))
            ) row)) 
      coords)))

