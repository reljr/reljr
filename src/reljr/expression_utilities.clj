(ns reljr.expression-utilities
  (:require [clojure.set :as set]
            [clojure.pprint :as pp]))

(defn and-raw-cnfs [cnf1 cnf2]
  (set/union cnf1 cnf2))

(defn or-raw-cnfs [cnf1 cnf2]
  (into #{}
        (for [c1 cnf1
              c2 cnf2]
          (set/union c1 c2))))

(defn not-cnf-atom [atom]
  (if (and (vector? atom) (= (first atom) 'not))
    (second atom)
    ['not atom]))

(defn not-raw-cnf-clause [clause]
  (into #{} (map (fn [a] #{(not-cnf-atom a)})) clause))

(defn not-raw-cnf [cnf]
  (transduce (map not-raw-cnf-clause) (completing or-raw-cnfs) #{} cnf))

(defn cnf-normalize-boolexpr-raw [expression]
  (cond
    (not (vector? expression)) #{#{expression}}
    :otherwise
    (case (first expression)
      and (transduce (map cnf-normalize-boolexpr-raw)
                     (completing and-raw-cnfs)
                     #{}
                     (rest expression))
      or (transduce (map cnf-normalize-boolexpr-raw)
                    (completing or-raw-cnfs)
                    #{#{}}
                    (rest expression))
      not (not-raw-cnf (cnf-normalize-boolexpr-raw (second expression)))
      #{#{expression}})))

(defn cnf-normalize-boolexpr [expression]
  (into ['and] (map #(into ['or] %)) (cnf-normalize-boolexpr-raw expression)))

(defn boolexpr-columns-used [e]
  (cond
    (keyword? e) #{e}
    (vector? e) (reduce set/union (map boolexpr-columns-used (rest e)))
    :otherwise #{}))
