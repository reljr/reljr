(ns reljr.expression-utilities
  (:require [clojure.set :as set]
            [clojure.pprint :as pp]
            [fipp.engine :as fipp]))

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

(defn propogate-column-metadata [expression]
  (->>
   (case (:type expression)
     :relation (::columns expression) ; Assume the preprocessor already set this
     (:selection :order-by) (::columns (:sub expression))
     :projection (into [] (map second) (:columns expression))
     :rename-relation (mapv #(keyword (:name expression) (name %)) (::columns (:sub expression)))
     :rename-column (mapv #(if (= %1 (:old expression)) (:new expression) %1) (::columns (:sub expression)))
     :group-by (into (:group-cols expression) (map first (:aggregation expression)))
     (:union :subtraction :intersection) (::columns (:left expression))
     :division (let [{:keys [left right]} expression]
                 (vec (set/difference (set left) (set right))))
     (:inner-join :cross-product) (into (::columns (:left expression)) (::columns (:right expression)))
     :natural-join
     (let [lcols (::columns (:left expression))
           l-groups (group-by name lcols)
           rcols (::columns (:right expression))
           r-groups (group-by name rcols)
           dropped-groups (for [[g1 cs1] l-groups
                                [g2 cs2] r-groups
                                :when (= g1 g2)]
                            (set cs2))
           cols (reduce set/difference
                        (into #{} (concat lcols rcols))
                        dropped-groups)]
       (vec cols)))
   (assoc expression ::columns)))

(defn raexpression-walker [expression prefn postfn]
  (let [expression (prefn expression)]
    (letfn [(recursor [expression]
              (raexpression-walker expression prefn postfn))
            (unary-recur [expression]
                         (postfn (update expression :sub recursor)))
            (binary-recur [expression]
                          (let [c (count expression)]
                            (-> expression
                                transient
                                (assoc! :left (recursor (:left expression)))
                                (assoc! :right (recursor (:right expression)))
                                persistent!
                                postfn)))]
      (case (:type expression)
        :relation (postfn (prefn expression))
        (:projection :selection :rename-relation
                     :rename-column :order-by :group-by)
        (unary-recur expression)
        (:union :subtraction :intersection :division
                :cross-product :inner-join :natural-join)
        (binary-recur expression)))))

(defn pprint-raexpression* [expression]
  (case (:type expression)
    :relation [:span "Relation: " (str (:relation expression)) :break]
    [:align
     (str (name (:type expression))) ":"
     :break
     [:nest
      (for [[k v] expression]
        (case k
          (:predicate :name :old :new :group-columns :relation)
          [:span (str (name k)) ": " (str v) :break]
          :columns
          [:span (str (name k)) ": "
           [:align (for [[a b] v] [:span (str a) " -> " (str b) :break])]]
          :orderings
          [:span (str (name k)) ": "
           [:align (for [[a b] v] [:span (str a) " " (str b) :break])]]
          :aggregation
          [:span (str (name k)) ": "
           [:align (for [[a b] v] [:span (str a) " <- " (str b) :break])]]
          nil))
      (for [v (keep expression [:sub :left :right])
            :when v]
        [:span "- " (pprint-raexpression* v)])]]))

(defn pprint-raexpression [expression]
  (fipp/pprint-document (pprint-raexpression* expression) {:width 80}))
