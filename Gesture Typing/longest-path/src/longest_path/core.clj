(ns longest-path.core
  "Solution to first problem in https://github.com/norvig/pytudes/blob/master/ipynb/Gesture%20Typing.ipynb"
  (:gen-class)
  (:require [clojure.string :as s]))

(def coordinates
  (let [parse (fn [row chars] (into {} (map-indexed #(do [%2 [row %1]]) chars)))]
    (merge
     (parse 1 [\Q \W \E \R \T \Y \U \I \O \P])
     (parse 2 [nil \A \S \D \F \G \H \J \K \L])
     (parse 3 [nil nil \Z \X \C \V \B \N \M]))))

(defn iterate-pairs [coll]
  (when (and coll (next coll))
    (cons [(first coll) (second coll)] (iterate-pairs (next coll)))))

(defn path-length-pair [[left right]]
  (let [[xleft yleft] (coordinates left)
        [xright yright] (coordinates right)]
    (+
     (Math/abs (- xright xleft))
     (Math/abs (- yright yleft)))))

(defn path-length-word [word]
  (->> word
      s/upper-case
      seq
      iterate-pairs
      (map path-length-pair)
      (apply +)))

(def wordlist
  (with-open [rdr (clojure.java.io/reader "/Users/tim/dev/etudes/Gesture Typing/TWL06.txt")]
    (vec (line-seq rdr))))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (->> wordlist
       (map #(vec [% (path-length-word %)]))
       (sort-by second >)
       (take 15)))
