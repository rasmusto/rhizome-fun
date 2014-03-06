(ns rhizome-fun.core
  (:require [rhizome.viz :refer [view-image dot->image view-graph save-graph save-image]]
            [rhizome.dot :refer [graph->dot]]))

(def alpha-nodes (mapv (comp keyword str)
                       "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"))

(def alpha-nodes-lower (mapv (comp keyword str)
                             "abcdefghijklmnopqrstuvwxyz"))

(def alpha-nodes-vowel (mapv (comp keyword str)
                             "aeiouAEIOU"))

(defn random-from [nodes]
  (let [n (rand-int (count nodes))
        pick (nodes n)
        [a b] (split-at n nodes)]
    [pick (vec (concat a (drop 1 b)))]))

(defn pick-nodes [nodes]
  (loop [picks [], nodes nodes, n (rand-int (count nodes))]
    (cond (zero? n) picks
          :else
          (let [[pick remaining] (random-from nodes)]
            (recur (conj picks pick) remaining (dec n))))))

(defn make-graph [nodes]
  (for [node nodes
        :let [picks (pick-nodes nodes)]]
    [node picks]))

(defn random-graph [nodes & {:keys [node->cluster
                                    cluster->descriptor]
                             :or {node->cluster (fn [n] (rand-int (count nodes)))
                                  cluster->descriptor (fn [n] {:label (str n)})}}]
  (let [nodes (pick-nodes (vec nodes))
        g (into {} (make-graph nodes))
        dot
        (graph->dot
          (keys g)
          g
          :node->descriptor (fn [n] {:label n})
          :node->cluster node->cluster
          :cluster->descriptor cluster->descriptor
          ; :edge->descriptor nil
          :options {:rankdir :BT
                    :dpi 100
                    :node {:fontname "Arial"}}
          :vertical? false
          :filename "test.png")]
    dot))

(def view-dot (comp view-image dot->image))

(comment
  (random-from alpha-nodes)
  (pick-nodes alpha-nodes)

  (require '[clojure.pprint :refer [pprint]])
  (pprint (make-graph (pick-nodes alpha-nodes-lower)))

  ; FIXME deal with clusters containing a single cluster
  (view-dot (random-graph (vec (range 26))))

  (view-dot
    (random-graph (take 20 alpha-nodes)
                  :cluster (fn [n] (cond ((set alpha-nodes-vowel) n) :vowel
                                         ((set [:b :c :d :f :B :C :D :F]) n) :bcdf
                                         :else :other))
                  :cluster->descriptor {:vowel {:label :vowel :rank :same}
                                        :bcdf  {:label :bcdf  :rank :same}
                                        :other {:label :other :rank :same}}))


  )
