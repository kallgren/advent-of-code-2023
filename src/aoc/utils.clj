(ns aoc.utils
  (:require
   [clojure.string :as str]))

(defn read-input-lines [f]
  (-> (slurp f)
      (str/split-lines)))

(defn get-char [s i]
  (subs s i (inc i)))
