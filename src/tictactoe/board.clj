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

(defn index-to-coords
  [board i]
  (let [row (quot i (:size board))
        col (mod i (:size board))]
    {:row row :col col}))

(defn field-at
  [board row col]
  (get (:fields board) (coords-to-index board row col)))

(defn is-field-empty? 
  [board row col]
  (let [index (coords-to-index board row col)]
    (= empty-field (get (:fields board) index))))

;; TODO: ugly hack with into {}, is there a nicer way?
(defn all-empty-fields
  [board]
  (let [fields (:fields board)]
    (into {} (filter #(= (second %) empty-field) fields))))

(defn update-field-in-board 
  [board row col value]
  (assoc (:fields board) (coords-to-index board row col) value))

(defn update-board 
  [board row col value]
  (->Board (:size board) (update-field-in-board board row col value)))

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
  (let [keys (sort (keys (:fields board)))]
    (doseq [row (partition (:size board) keys)]
      (let [values (map (fn [r] (get (:fields board) r)) row)
            row-printable (clojure.string/join " | " values)]
        (println row-printable)))))

(defn generate-coords
  [board coords-generating-function]
  (let [indexes (range 0 (:size board))]
    (map (fn [row] (map (partial coords-generating-function row) indexes)) indexes)))

(defn rows-coords
  [board]
  (generate-coords board (fn [row col] [row col])))

(defn columns-coords
  [board]
  (generate-coords board (fn [row col] [col row])))

(defn diagonals-coords
  [board]
  (let [indexes (range 0 (:size board))
        size (:size board)
        diagSE (map (fn [i] [i i]) indexes)
        diagSW (map (fn [i] [i (- (- size 1) i)]) indexes)]
    [diagSE diagSW]))

(defn all-fields-are-same?
  [mark fields-seq]
  (every? (fn [field] (= mark field)) fields-seq))

(defn map-coords-to-values
  [board coords] 
  (map (fn [field] 
    (let [row (nth field 0) 
          col (nth field 1)] 
      (field-at board row col))) coords))

;; returns nil if no row matches predicate when using some (strange clojure library behaviour?)
;; using empty? and filter instead for more consistent result
(defn win-condition-for
  [mark fields]
  (not (empty? (filter (partial all-fields-are-same? mark) fields))))

(defn check-if-win?
  [board mark]
  (let [all-rows (rows-coords board)
        all-cols (columns-coords board)
        diagonals (diagonals-coords board)
        fields-rows (map (partial map-coords-to-values board) all-rows)
        fields-cols (map (partial map-coords-to-values board) all-cols)
        fields-diagonals (map (partial map-coords-to-values board) diagonals)]
    (or 
      (win-condition-for mark fields-rows)
      (win-condition-for mark fields-cols)
      (win-condition-for mark fields-diagonals))))

(defn draw?
  [board]
  (let [empty-fields (all-empty-fields board)]
    (and 
      (= 0 (count empty-fields))
      (= false (check-if-win? board cross))
      (= false (check-if-win? board circle)))))

