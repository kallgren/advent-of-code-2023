(ns aoc.day03
  (:require
   [aoc.utils :refer [contains-value? get-char read-input-lines]]
   [clojure.string :as str]
   [clojure.test :refer [deftest is]]))

(def input "resources/day03.txt")

(def symbol-regex #"[!--/:-?{-~^_`@\[\]]") ;; poor

(defn has-symbols? [s]
  (boolean (re-seq symbol-regex s)))

(defn has-asterisk? [s]
  (boolean (re-find #"\*" s)))

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

(defn get-surrounding-gears [number lines x y]
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
    (some true? (map has-asterisk? [adjacent-above adjacent-below adjacent-sides]))))

(map #(if (= (first %) [0 1] (first %) (inc (second %))) %) {[0 1] 12})

(defn get-gear-ratios-acc [s y lines x numbers gear-map]
  (if (< x (count s))
    (let [char (get-char s x)]
      (if (Character/isDigit (first char))
        (let [number (extract-number s x)
              surrounding-gears (get-surrounding-gears number lines x y) ;; [[][][]]
              ;; for each gear, add this number to that key in gear-map
              new-gear-map gear-map ;; add this number to all those gears
              new-numbers (if should-be-added (conj numbers (Integer. number)) numbers)
              new-x (+ x (count number))]
          (recur s y lines new-x new-numbers new-gear-map))
        (recur s y lines (inc x) numbers new-gear-map)))
    numbers))

(defn get-gear-ratios [s y lines]
  (get-gear-ratios-acc s y lines 0 [] {}))


(comment
  (defn get-symbol-indices [s]
    (->> s
         (map #(re-seq symbol-regex (str %)))
         (map-indexed #(when %2 %))
         (remove nil?)))

  (defn get-symbol-index-map [lines]
    (->> lines
         (map get-symbol-indices)
      ;;  (map-indexed #(vec [%2 %]))
         (zipmap (range (count lines)))))

  (defn has-symbol-neighbour? [[x y] symbol-index-map]
    (let [has-upper-left (contains-value? (dec x) (get symbol-index-map (dec y) '()))
          has-upper (contains-value? x (get symbol-index-map (dec y) '()))
          has-upper-right (contains-value? (inc x) (get symbol-index-map (dec y) '()))
          has-left (contains-value? (dec x) (get symbol-index-map y '()))
          has-right (contains-value? (inc x) (get symbol-index-map y '()))
          has-lower-left (contains-value? (dec x) (get symbol-index-map (inc y) '()))
          has-lower (contains-value? x (get symbol-index-map (inc y) '()))
          has-lower-right (contains-value? (inc x) (get symbol-index-map (inc y) '()))]
      (or has-upper-left has-upper has-upper-right has-left has-right has-lower-left has-lower has-lower-right)))

  (defn process-number [s symbol-index-map line-no i current is-part]
    (let [c (subs s i (inc i))]
      (if (Character/isDigit (first c))
        (let [has-symbol-neighbour (has-symbol-neighbour? [i line-no] symbol-index-map)
              new-is-part (or has-symbol-neighbour is-part)]
          (if (<= i (count s))
            (recur s symbol-index-map line-no (inc i) (str current c) new-is-part)
            (when is-part
              current)))
        (when is-part
          current))))

  (process-number "...32...11" {0 2} 0 3 "" false)

  (Character/isDigit \.)

;; (defn get-number-bounds [s bounds]
;;   (for [c s]
;;     (when (Character/isDigit c)
;;       (process-number (subs s i) (str c)))))

;; (get-number-bounds "...23....429.." [])
  :rcf)

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

(defn part-2 [fpath]
  (let [lines (read-input-lines fpath)]
    (->> lines
         (map-indexed #(get-part-numbers %2 % lines))
         (apply concat)
         (apply +))))

(println (part-1 input))
(println (part-2 input))

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

(def expected-index-map
  {0 '()
   1 '(3)
   2 '()
   3 '(6)
   4 '(3)
   5 '(5)
   6 '()
   7 '()
   8 '(3 5)
   9 '()})

(get-symbol-index-map test-input)

(deftest get-symbol-indices-test
  (is (= '(3) (get-symbol-indices (nth test-input 1))))
  (is (= '(6) (get-symbol-indices (nth test-input 3))))
  (is (= '(3 5) (get-symbol-indices (nth test-input 8)))))

(deftest get-symbol-index-map-test
  (is (= expected-index-map (get-symbol-index-map test-input))))

(deftest get-part-numbers-test
  (is (= [467]    (get-part-numbers (nth test-input 0) 0 test-input)))
  (is (= [35 633] (get-part-numbers (nth test-input 2) 2 test-input))))

(deftest part-1-test
  (with-redefs [read-input-lines (fn [_] test-input)]
    (is (= 4361 (part-1 "dummy")))))

(deftest part-2-test
  (with-redefs [read-input-lines (fn [_] test-input)]
    (is (= 467835 (part-2 "dummy")))))

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

  (->> test-input
       (map get-symbol-indices)
       (zipmap (range (count test-input))))

  (boolean (re-seq symbol-regex ".....a2*"))

  (for [c "tes2t"]
    (when (Character/isDigit c)
      (println c)))
  :rcf)