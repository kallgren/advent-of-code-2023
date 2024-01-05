(ns aoc.utils
  (:require
   [clojure.string :as str]))

(defn read-input-lines [f]
  (-> (slurp f)
      (str/split-lines)))

(defn contains-value? [coll v]
  (some #(= % v) coll))

(defn get-char [s i]
  (subs s i (inc i)))

(defn get-numbers [s]
  (->> (re-seq #"\d+" s)
       (map #(Integer. %))))

(defn filter-above [coll threshold] ;; TODO: Gotta be a built in way
  (filter #(> % threshold) coll))
