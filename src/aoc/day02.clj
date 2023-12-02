(ns aoc.day02
  (:require
   [aoc.utils :refer [read-input-lines]]
   [clojure.string :as str]
   [clojure.test :refer [deftest is]]))

(def input "resources/day02.txt")

(def limits
  {:red 12
   :green 13
   :blue 14})

(defn get-max [s color]
  (->> (re-seq (re-pattern (str "(\\d+) " color)) s)
       (map #(Integer. (second %)))
       (apply max)))

(defn get-maximums-map [s]
  (let [[_ id] (re-matches #"Game (\d+).*" s)
        red (get-max s "red")
        green (get-max s "green")
        blue (get-max s "blue")]
    {:id (Integer. id) :red red :green green :blue blue}))

(defn within-limits? [{:keys [red green blue]}]
  (and (<= red (:red limits)) (<= green (:green limits)) (<= blue (:blue limits))))

(defn get-power [{:keys [red green blue]}]
  (* red green blue))

(defn part-1 [fpath]
  (->> (read-input-lines fpath)
       (map get-maximums-map)
       (filter within-limits?)
       (map :id)
       (apply +)))

(defn part-2 [fpath]
  (->> (read-input-lines fpath)
       (map get-maximums-map)
       (map get-power)
       (apply +)))

(println (part-1 input))
(println (part-2 input))

(def test-input
  ["Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green"
   "Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue"
   "Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red"
   "Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red"
   "Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"])

(deftest get-maximums-map-test
  (is (= {:id 1 :blue 6 :red 4 :green 2}   (get-maximums-map (nth test-input 0))))
  (is (= {:id 2 :blue 4 :red 1 :green 3}   (get-maximums-map (nth test-input 1))))
  (is (= {:id 3 :blue 6 :red 20 :green 13} (get-maximums-map (nth test-input 2))))
  (is (= {:id 4 :blue 15 :red 14 :green 3} (get-maximums-map (nth test-input 3))))
  (is (= {:id 5 :blue 2 :red 6 :green 3}   (get-maximums-map (nth test-input 4)))))

(deftest part-1-test
  (with-redefs [read-input-lines (fn [_] test-input)]
    (is (= 8 (part-1 "dummy")))))

(deftest part-2-test
  (with-redefs [read-input-lines (fn [_] test-input)]
    (is (= 2286 (part-2 "dummy")))))

(comment
;; (defn part-1 [fpath]
;;   (->> (read-input-lines fpath)
;;        (map #(str/split % #"Game (\d+): "))
;;        (map #(zipmap (re-groups %) (str/split (second %) #"; ")))))
;;     ;;    (map (fn [reveals] (map #(str/replace % #"(\d+) red|(\d+) green|(\d+) blue") reveals)))))

  (->>  ["Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green"]
        (map #(str/split % #"Game (\d+): "))
        (map #(zipmap (first %) (str/split (second %) #"; "))))

  (str/replace
   "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green"
   #"Game (\d+): [(\d+) red|(\d+) green|(\d+) blue]"
   #(str (nth % 1)))

  (re-seq
   #"Game (\d+): (\d+) red|(\d+) green|(\d+) blue"
   "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green")

  (defn new-get [s]
    (let [[_ game-id rest] (re-seq #"Game (\d+): (.+)" s)
          turns (str/split "; " rest)]
      {game-id {:blue nil}}))

  (new-get    "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green")

  (def res (re-seq #"\d+ blue" "30 blue; 4 blue"))
  (re-seq #"\d+" (str res))

  (get-maximums-map "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green")

  (re-matches #"Game (\d+):.*" "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green")

  (re-seq
   #"(\d+) red|(\d+) green|(\d+) blue"
   "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green")

  (re-matches
   #"(\d+) red|(\d+) green|(\d+) blue"
   "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green")

  (str/split "Game: 2" #": ")
  :rcf)