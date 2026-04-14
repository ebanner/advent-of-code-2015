(def GRID-SIZE 1000)


(defmacro reduce* [[acc init x coll] & body]
  `(reduce (fn [~acc ~x] ~@body) ~init ~coll))


(defn get-action [line]
  (let [tokens (clojure.string/split line #" ")]

    (cond
      (= (first tokens) "toggle") :TOGGLE
      (= (subvec tokens 0 2) ["turn" "on"]) :TURN-ON
      (= (subvec tokens 0 2) ["turn" "off"]) :TURN-OFF)))


(defn parse-point [point-str]
  (let [[i j] (clojure.string/split point-str #",")]
    (mapv Integer/parseInt [i j])))


(defn get-rectangle [line]
  (let [tokens (clojure.string/split line #" ")
        N (count tokens)]

    [(parse-point (tokens (- N 3)))
     (parse-point (last tokens))]))


(defn parse [line]
  (let [action (get-action line)
        rectangle (get-rectangle line)]

    [action rectangle]))


(defn get-instructions []
  (let [lines (clojure.string/split-lines (slurp "input"))]

    (mapv parse lines)))


(defn get-grid [N]
  (->>
   (for [_ (range N)] (into [] (repeat N 0)))
   (into [])))


(defn turn-on [grid [[i1 j1] [i2 j2]]]
  (reduce* [grid grid
            [i j] (for [i (range i1 (inc i2)) j (range j1 (inc j2))] [i j])]

    (update-in grid [i j] inc)))


(defn turn-off [grid [[i1 j1] [i2 j2]]]
  (reduce* [grid grid
            [i j] (for [i (range i1 (inc i2)) j (range j1 (inc j2))] [i j])]

    (update-in grid [i j] #(max (dec %) 0))))


(defn toggle [grid [[i1 j1] [i2 j2]]]
  (reduce* [grid grid
            [i j] (for [i (range i1 (inc i2)) j (range j1 (inc j2))] [i j])]

    (update-in grid [i j] (comp inc inc))))


(defn do-instruction [[action rectangle] grid]
  (cond
    (= action :TURN-ON) (turn-on grid rectangle)
    (= action :TURN-OFF) (turn-off grid rectangle)
    (= action :TOGGLE) (toggle grid rectangle)))


(defn count-grid [grid]
  (let [N (count grid)
        M (count (first grid))]

    (reduce +
      (for [i (range N) j (range M)]
        (get-in grid [i j])))))


(defn -main [& args]
  (let [instructions (get-instructions)
        grid (-> (get-grid GRID-SIZE) atom)]

    (doseq [instruction instructions]
      (swap! grid #(do-instruction instruction %)))

    (count-grid @grid)))


(println (-main))
