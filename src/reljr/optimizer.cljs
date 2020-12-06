(ns reljr.optimizer
  (:require [clojure.set :as set]
            [reljr.expression-utilities :as eutils]))

(defn optimize-up-selection [e]
  (if (= (:type (:sub e)) :selection)
    (assoc e
           :sub (:sub (:sub e))
           :predicate ['and (:predicate e) (:predicate (:sub e))])
    e))

(defn optimize-up-join [e]
  (let [[l lp] (if (= :selection (:type (:left e)))
                 [(:sub (:left e)) (:predicate (:left e))]
                 [(:left e) ['and]])
        [r rp] (if (= :selection (:type (:right e)))
                 [(:sub (:right e)) (:predicate (:right e))]
                 [(:right e) ['and]])
        cross (eutils/propogate-column-metadata
               {:type :cross-product
                :left l
                :right r})]
    (eutils/propogate-column-metadata
     {:type :selection
      :predicate ['and (or (:relation e) ['and]) lp rp]
      :sub cross})))

(defn optimize-up [expression]
  (letfn [(postwalker [e]
            (case (:type e)
              :selection (optimize-up-selection e)
              (:inner-join :cross-product) (optimize-up-join e)
              e))]
    (eutils/raexpression-walker expression identity postwalker)))

(defn cnf-selections [expression]
  (letfn [(prewalker [e]
            (if (= :selection (:type e))
              (update e :predicate eutils/cnf-normalize-boolexpr)
              e))]
    (eutils/raexpression-walker expression prewalker identity)))

(defn- pick-condition-set [test-groups-by-sub picked remaining]
  (let [conds (for [c remaining
                    :let [g (seq (test-groups-by-sub c))]
                    :when g]
                (reduce (fn [[ls lt] [rs rt]]
                          (if (< (count (set/difference ls picked))
                                 (count (set/difference rs picked)))
                            [ls lt]
                            [rs rt]))
                        g))]
    (when (seq conds)
      (reduce (fn [[ls lt] [rs rt]]
                (if (< (count (set/difference ls picked))
                       (count (set/difference rs picked)))
                  [ls lt]
                  [rs rt]))
              conds))))

(defn- clean-test-groups [test-groups-by-sub group-keys]
  (reduce (fn [test-groups-by-sub s]
            (update test-groups-by-sub s dissoc group-keys))
          test-groups-by-sub
          group-keys))

(defn- regroup-test-groups-by-sub [test-groups subs]
  (into {} (map (fn [sub] [sub (into {} (filter (fn [[s t]] (s sub)) test-groups))]))
        subs))

(defn- get-subs [expression]
  (into #{} ((fn rec [e]
               (if (= :cross-product (:type e))
                 (concat (rec (:left e)) (rec (:right e)))
                 [e]))
             (:sub expression))))

(defn- compute-columns-to-subs [subs]
  (into {} (mapcat (fn [{cs ::eutils/columns :as sub}]
                     (for [c cs] [c sub])))
        subs))

(defn normalize-join [expression]
  (let [tests (rest (:predicate expression))
        subs (get-subs expression)
        col-sub-map (compute-columns-to-subs subs)
        test-groups (->> tests
                         (group-by (fn [e] (into #{}
                                                (map col-sub-map)
                                                (eutils/boolexpr-columns-used e))))
                         (into {} (map (fn [[k v]] [(set k) v]))))
        zero-tests (test-groups #{})
        one-tests (into {} (comp (filter (fn [[ks v]] (= (count ks) 1)))
                                 (map (fn [[ks v]] [(first ks) v])))
                        test-groups)
        sub-map (into {}
                      (map (fn [s]
                             (if-let [test (one-tests s)]
                               [s {:type :selection
                                   :sub s
                                   :predicate (apply vector 'and test)
                                   ::eutils/columns (::eutils/columns s)}]
                               [s s])))
                      subs)
        test-groups (reduce dissoc test-groups
                            (cons #{} (filter #(= 1 (count %)) (keys test-groups))))
        test-groups-by-sub (regroup-test-groups-by-sub test-groups subs)
        [initial-set initial-cond] (pick-condition-set test-groups-by-sub #{} subs)]
    (if initial-set
      (loop [test-groups-by-sub (clean-test-groups test-groups-by-sub initial-set)
             remaining (set/difference subs initial-set)
             picked initial-set
             expr {:type :inner-join :relation (apply vector 'and initial-cond)
                   :left (reduce (fn [l r] {:type :cross-product
                                           :left l :right r})
                                 (sub-map (first initial-set))
                                 (map sub-map (rest (butlast initial-set))))
                   :right (sub-map (last initial-set))}]
        (if (seq remaining)
          (if-let [[sub-set cond] (pick-condition-set test-groups-by-sub picked remaining)]
            (let [new-subs (set/difference sub-set picked)
                  now-picked (set/union picked sub-set)
                  all-conds (into {} (mapcat (fn [s]
                                               (for [cond (test-groups-by-sub s)
                                                     :when (set/subset? (first cond) now-picked)]
                                                 cond)))
                                  now-picked)
                  full-cond (apply vector 'and (mapcat second all-conds))]
              (recur (reduce clean-test-groups test-groups-by-sub (map first all-conds))
                     (set/difference remaining sub-set)
                     now-picked
                     {:type :inner-join :relation full-cond
                      :left (reduce (fn [l r] {:type :cross-product
                                              :left l :right r})
                                    expr
                                    (map sub-map (rest new-subs)))
                      :right (sub-map (first new-subs))}))
            (reduce (fn [l r] {:type :cross-product
                              :left l :right r})
                    expr
                    (map sub-map remaining)))
          expr))
      (reduce (fn [l r] {:type :cross-product
                        :left l :right r})
              (map sub-map subs)))))

(defn normalize-joins [expression]
  (letfn [(prewalker [e]
            (if (and (= :selection (:type e))
                     (= :cross-product (:type (:sub e))))
              (normalize-join e)
              e))]
    (eutils/raexpression-walker expression prewalker eutils/propogate-column-metadata)))

(defn optimize [expression tables]
  (-> expression optimize-up cnf-selections normalize-joins))
