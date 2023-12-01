(ns aoc.day01
  (:require
   [aoc.utils :refer [read-input-lines]]
   [clojure.string :as str]
   [clojure.test :refer [deftest is]]))

(def input "resources/day01.txt")

(def alpha-digit-map
  ;; Trailing letter to ensure the very last
  ;; alphabetical digit is not missed
  {"one"   "1e"
   "two"   "2o"
   "three" "3e"
   "four"  "4"
   "five"  "5e"
   "six"   "6"
   "seven" "7n"
   "eight" "8t"
   "nine"  "9e"})

(defn convert-alpha-digits [s]
  (let [pattern (re-pattern (str/join "|" (keys alpha-digit-map)))]
    (-> s
        (str/replace pattern alpha-digit-map)
        (str/replace pattern alpha-digit-map))))

(defn get-calibration-val [s]
  (let [digits (re-seq #"\d" s)]
    (when (seq digits)
      (Integer. (str (first digits) (last digits))))))

(defn part-1 [fpath]
  (->> (read-input-lines fpath)
       (map get-calibration-val)
       (apply +)))

(defn part-2 [fpath]
  (->> (read-input-lines fpath)
       (map convert-alpha-digits)
       (map get-calibration-val)
       (apply +)))

(println (part-1 input)) ;; 54927
(println (part-2 input)) ;; 54581

(deftest convert-alpha-digits-test
  (is (= "2o19e"       (convert-alpha-digits "two1nine")))
  (is (= "82o3e"       (convert-alpha-digits "eightwothree")))
  (is (= "abc1e23exyz" (convert-alpha-digits "abcone2threexyz")))
  (is (= "x21e34"      (convert-alpha-digits "xtwone3four")))
  (is (= "49e8t7n2"    (convert-alpha-digits "4nineeightseven2")))
  (is (= "z18t234"     (convert-alpha-digits "zoneight234")))
  (is (= "7pqrst6teen" (convert-alpha-digits "7pqrstsixteen"))))

(deftest get-calibration-val-test
  (is (= 12 (get-calibration-val "1abc2")))
  (is (= 38 (get-calibration-val "pqr3stu8vwx")))
  (is (= 15 (get-calibration-val "a1b2c3d4e5f")))
  (is (= 77 (get-calibration-val "treb7uchet"))))

(deftest part-1-test
  (with-redefs [read-input-lines (fn [_] ["1abc2" "pqr3stu8vwx" "a1b2c3d4e5f" "treb7uchet"])]
    (is (= 142 (part-1 "dummy")))))

(deftest part-2-test
  (with-redefs [read-input-lines (fn [_] ["two1nine" "eightwothree" "abcone2threexyz" "xtwone3four" "4nineeightseven2" "zoneight234" "7pqrstsixteen"])]
    (is (= 281 (part-2 "dummy")))))

(deftest part-2-extra-test
  ;; Make sure the very last alphabetical digit is not missed
  (with-redefs [read-input-lines (fn [_] ["two1nineight" "eighttwone"])]
    (is (= (+ 28 81) (part-2 "dummy")))))

(comment
  (re-seq #"\d" "ad9d20ja")
  (get-calibration-val "1abc2")
  (Integer. "8a")
  (nth "asd" 1)
  (keys alpha-digit-map)
  (str/join "|" '("asd" "bas"))
  \d
  'a
  \a
  (= \a 'a)
  (= 'a 'a')
  :rcf)
