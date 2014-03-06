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

(comment
  (random-from alpha-nodes)
  (pick-nodes alpha-nodes))

(defn pick-nodes [nodes]
  (loop [picks [], nodes nodes, n (rand-int (count nodes))]
    (cond (zero? n) picks
          :else
          (let [[pick remaining] (random-from nodes)]
            (recur (conj picks pick) remaining (dec n))))))

(comment
  (require '[clojure.pprint :refer [pprint]])
  (pprint (make-graph (pick-nodes alpha-nodes-lower))))

(defn make-graph [nodes]
  (for [node nodes
        :let [picks (pick-nodes nodes)]]
    [node picks]))

(let [nodes (pick-nodes (vec (take 10 alpha-nodes)))
      ; TODO analyze word adjacency
      g (into {} (make-graph nodes))
      cluster (fn [n] (rand-int (count nodes)))
      cluster (fn [n] (cond ((set alpha-nodes-vowel) n) :vowel
                            ((set [:b :c :d :f :B :C :D :F]) n) :bcdf
                            :else :other))
      dot
      (graph->dot
        (keys g)
        g
        :node->descriptor (fn [n] {:label n})
        ;; TODO node options allow for assigning rank?
        :node->cluster cluster
        :cluster->descriptor {
                              :vowel {:label :vowel :rank :same}
                              :bcdf  {:label :bcdf  :rank :same}
                              :other {:label :other :rank :same}
                              }
        ; :edge->descriptor (fn [a b]
        ;                     (condp = [a b]
        ;                       [a :a] {:label b}
        ;                       [a :b] {:label b}
        ;                       [a :c] {:label b}
        ;                       [a :d] {:label b}
        ;                       [a :e] {:label b}
        ;                       [a :f] {:label b}
        ;                       [a :g] {:label b}
        ;                       [a :h] {:label b}
        ;                       [a :i] {:label b}
        ;                       [a :j] {:label b}
        ;                       [a :k] {:label b}
        ;                       [a :l] {:label b}
        ;                       [a :m] {:label b}
        ;                       [a b] {:label b}
        ;                       ))
        :options {:rankdir :BT
                  :dpi 300
                  :node {:fontname "Arial"}
                  :edge {:test "foo"}
                  }
        :vertical? false
        :filename "test.png")
      ]
  (-> dot dot->image (save-image "test.png")))
