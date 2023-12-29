(ns aoc.day06
  (:require
   [aoc.utils :refer [read-input-lines]]
   [clojure.test :refer [deftest is]]))

(def input "resources/day06.txt")

(defn get-nums [s]
  (->> (re-seq #"\d+" s)
       (map #(Integer. %))))

(defn calc-distance [hold-time limit-time]
  (let [moving-time (- limit-time hold-time)]
    (* moving-time hold-time)))

(defn get-possibilities [time]
  (->> (range 1 (inc time))
       (map #(calc-distance % time))))

(defn get-winning [possibilities target]
  (filter #(> % target) possibilities))

;; (->> (read-input-lines input)
;;      #(count (first %))
;;      (reduce *))

(defn part-1 [fpath]
  (let [[line-1 line-2] (read-input-lines fpath)
        times (get-nums line-1)
        distances (get-nums line-2)]
    (->> times
         (map-indexed #(get-winning (get-possibilities %2) (nth distances %)))
         (map count)
         (reduce *))))

;; (defn part-1 [fpath]
;;   (let [[t d] (read-input-lines fpath)
;;         times (get-nums t)
;;         distances (get-nums d)]
;;     (->> (process times distances)
;;          (reduce *))))

;;; WHYYY NO WORK
;; (defn part-1 [fpath]
;;   (->> (read-input-lines fpath)
;;        #(process (get-nums (first %)) (get-nums (last %)))
;;        (reduce *)))

(println (part-1 input))

(def test-input
  ["Time:      7  15   30"
   "Distance:  9  40  200"])

(deftest part-1-test
  (with-redefs [read-input-lines (fn [_] test-input)]
    (is (= 288 (part-1 "dummy")))))

(comment
  (get-nums "Time:      7   15    30")
  (get-possibilities 4)
  (get-winning (get-possibilities 4) 2)
  :rcf)