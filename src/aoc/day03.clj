(ns aoc.day03
  (:require
   [aoc.utils :refer [get-char read-input-lines]]
   [clojure.string :as str]
   [clojure.test :refer [deftest is]]))

(def input "resources/day03.txt")

(def symbol-regex #"[!--/:-?{-~^_`@\[\]]") ;; poor

(defn has-symbols? [s]
  (boolean (re-seq symbol-regex s)))

(defn has-surrounding-symbols? [number lines x y]
  (let [line-above (nth lines (dec y) nil)
        line-below (nth lines (inc y) nil)
        this-line (nth lines y)
        x-min (if (= x 0) 0 (dec x))
        line-len (count (first lines))
        x-right (+ x 1 (count number))
        x-max (if (>= x-right line-len) line-len x-right)
        adjacent-above (if line-above (subs line-above x-min x-max) "")
        adjacent-below (if line-below (subs line-below x-min x-max) "")
        adjacent-sides (subs this-line x-min x-max)]
    (some true? (map has-symbols? [adjacent-above adjacent-below adjacent-sides]))))

(defn extract-number-acc [s start x current]
  (if (< x (count s))
    (let [char (get-char s x)]
      (if (Character/isDigit (first char))
        (recur s start (inc x) (str current char))
        current))
    current))

(defn extract-number [s start]
  (extract-number-acc s start start ""))

(defn get-part-numbers-acc [s y lines x numbers]
  (if (< x (count s))
    (let [char (get-char s x)]
      (if (Character/isDigit (first char))
        (let [number (extract-number s x)
              should-be-added (has-surrounding-symbols? number lines x y)
              new-numbers (if should-be-added (conj numbers (Integer. number)) numbers)
              new-x (+ x (count number))]
          (recur s y lines new-x new-numbers))
        (recur s y lines (inc x) numbers)))
    numbers))

(defn get-part-numbers [s y lines]
  (get-part-numbers-acc s y lines 0 []))

(defn part-1 [fpath]
  ;; for each line
  ;; find first digit
  ;; step forward and remember whole number and its index
  ;; when number has ended, do symbol regex on lines above below and on the sides
  ;; if match, keep the number, else drop it
  ;; continue the rest of line same way
  ;; continue the other lines
  (let [lines (read-input-lines fpath)]
    (->> lines
         (map-indexed #(get-part-numbers %2 % lines))
         (apply concat)
         (apply +))))

(println (part-1 input))

(def test-input
  ["467..114.."
   "...*......"
   "..35..633."
   "......#..."
   "617*......"
   ".....+.58."
   "..592....."
   "......755."
   "...$.*...."
   ".664.598.."])

(deftest get-part-numbers-test
  (is (= [467]    (get-part-numbers (nth test-input 0) 0 test-input)))
  (is (= [35 633] (get-part-numbers (nth test-input 2) 2 test-input))))

(deftest part-1-test
  (with-redefs [read-input-lines (fn [_] test-input)]
    (is (= 4361 (part-1 "dummy")))))

(comment
  (re-seq symbol-regex "@!\"#€%&/()=?")
  (has-surrounding-symbols? "192" ["..23x..*192"] 8 0)
  (map has-symbols? ["*19"])
  (extract-number "*192" 1)
  (get-part-numbers "..23x..*192" 0 ["..23x..*192"])

  (map #(re-seq symbol-regex (str %)) "..*23*")

  (re-seq symbol-regex "..*23*")

  (str/index-of "..*23*" \*)

  (re-seq symbol-regex (nth test-input 1))

  ;; (->> test-input
  ;;      (map get-symbol-indices)
  ;;      (zipmap (range (count test-input))))

  (boolean (re-seq symbol-regex ".....a2*"))

  (for [c "tes2t"]
    (when (Character/isDigit c)
      (println c)))
  :rcf)