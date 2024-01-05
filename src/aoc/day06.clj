(ns aoc.day06
  (:require
   [aoc.utils :refer [filter-above get-numbers read-input-lines]]
   [clojure.string :as str]
   [clojure.test :refer [deftest is]]))

(def input "resources/day06.txt")

(defn calc-distance [hold-time limit-time]
  (let [moving-time (- limit-time hold-time)]
    (* moving-time hold-time)))

(defn get-possibilities [time]
  (->> (range 1 (inc time))
       (map #(calc-distance % time))))

(defn part-1 [fpath]
  (let [[line-1 line-2] (read-input-lines fpath)
        times (get-numbers line-1)
        distances (get-numbers line-2)]
    (->> times
         (map-indexed (fn [idx itm]
                        (filter-above (get-possibilities itm) (nth distances idx))))
         (map count)
         (reduce *))))

(defn part-2 [fpath]
  (let [[line-1 line-2] (read-input-lines fpath)
        times [(Long. (str/join (re-seq #"\d+" line-1)))] ;; TODO: find way to piggyback, or make part-2 handle one num instead
        distances [(Long. (str/join (re-seq #"\d+" line-2)))]]
    (->> times
         (map-indexed #(filter-above (get-possibilities %2) (nth distances %)))
         (map count)
         (reduce *)))) ;; unused


;; TODO: Use hashmap for performance

(println (part-1 input))
(println (part-2 input))

(def test-input
  ["Time:      7  15   30"
   "Distance:  9  40  200"])

(deftest part-1-test
  (with-redefs [read-input-lines (fn [_] test-input)]
    (is (= 288 (part-1 "dummy")))))

(deftest part-2-test
  (with-redefs [read-input-lines (fn [_] test-input)]
    (is (= 71503 (part-2 "dummy")))))

(comment
  (get-numbers "Time:      7   15    30")
  (get-possibilities 4)
  (filter-above (get-possibilities 4) 2)
  (str/join ["12" "3"])
  (Long. "207139412091014")
  :rcf)