(ns urm.core
  (:refer-clojure :exclude [inc pop])
  (:require [clojure.pprint :refer [pprint]]
            [clojure.math.numeric-tower :as math]))

(declare decode-pair code-pair)

(defn inc [register jump-to]
  [:inc register jump-to])

(defn deb [register jump-to branch-on-zero]
  [:deb register jump-to branch-on-zero])

(defn end []
  [:end])

(defn copy [from to goto]
  [:copy from to goto])

(defn push [from to goto]
  [:push from to goto])

(defn pop [from to goto branch]
  [:pop from to goto branch])

(defn apply-statement [[instruction & args :as statement] state]
  (case instruction
    :inc  (let [[register jump-to] args
                current (get-in state [:registers register] 0)]
            (-> state
                (assoc-in [:registers register] (+ 1 current))
                (assoc :position jump-to)))
    :deb  (let [[register jump-to branch-on-zero] args
                current (get-in state [:registers register] 0)
                branch? (== current 0)]
            (-> state
                (assoc-in [:registers register] (if branch? current (dec current)))
                (assoc :position (if branch? branch-on-zero jump-to))))
    :copy (let [[from to exit] args
                from-value (get-in state [:registers from] 0) ]
            (-> state
                (assoc-in [:registers to] from-value)
                (assoc :position exit)))
    :push (let [[from to exit] args
                from-value (get-in state [:registers from] 0)
                to-value (get-in state [:registers to] 0)]
            (-> state
                (assoc-in [:registers to] (code-pair from-value to-value))
                (assoc-in [:registers from] 0)
                (assoc :position exit)))
    :pop  (let [[from to halt exit] args
                from-value (get-in state [:registers from] 0)
                exit? (== 0 from-value)]
            (if exit?
              (assoc state :position exit)
              (let [[h t] (decode-pair from-value)]
                (-> state
                    (assoc-in [:registers from] t)
                    (assoc-in [:registers to] h)
                    (assoc :position halt)))))
    :end state))

(defn next-state [{:keys [position program] :as state}]
  (let [next-statement (nth program position)]
    (apply-statement next-statement state)))

(defn run [state]
  (let [next (next-state state)]
    (println next)
    (if (= next state)
      state
      (recur next))))

(defn urm->fn [statements]
  (fn [& args]
    (let [final-state (run {:program statements
                            :position 0
                            :registers (zipmap (drop 1 (range)) args)})]
      (get-in final-state [:registers 0] 0))))

(defn eval-urm [statements args]
  (apply (urm->fn statements) args))

(defn zero [register]
  [(deb register 0 1)
   (end)])

(defn divides? [n div]
  (== 0 (rem n div)))

(defn factors-of-2
  ([n] (factors-of-2 n 0))
  ([n so-far]
   (if (divides? n 2)
     (recur (/ n 2)
            (+ 1 so-far))
     so-far)))

(defn code-pair [x y]
  (* (math/expt 2 x)
     (+ (* 2 y) 1)))

(defn decode-pair [n]
  (let [x (factors-of-2 n)
        y (/ (dec (/ n (math/expt 2 x)))
             2)]
    [x y]))

(defn code-pair* [x y]
  (dec (code-pair x y)))

(defn uncode-pair* [n]
  (decode-pair (+ n 1)))

(defn code-list [[h & t :as number-list]]
  (if (empty? number-list)
    0
    (code-pair h (code-list t))))

(defn decode-list [code]
  (if (== code 0)
    '()
    (let [[h code'] (decode-pair code)]
      (cons h
            (decode-list code')))))

(defn code-instruction [[instruction register jump-to branch-on-zero]]
  (case instruction
    :inc (code-pair (* 2 register) jump-to)
    :deb (code-pair (+ (* 2 register) 1)
               (code-pair* jump-to branch-on-zero))
    :end 0))

(defn decode-instruction [code]
  (if (== code 0)
    (end)
    (let [[y z] (decode-pair code)]
      (if (even? y)
        (inc (/ y 2) z)
        (let [[j k] (uncode-pair* z)]
          (deb (/ (dec y) 2)
               j
               k))))))

(defn code-program [instructions]
  (code-list (map code-instruction instructions)))

(defn decode-program [code]
  (map decode-instruction (decode-list code)))

(def program 1)
(def registers 2)
(def position 3)
(def current-instruction 4)
(def current-instruction-type 5)
(def current-register 6)
(def s 7)
(def t 8)
(def z 9)

(def uurm
  [(copy program t 1)
   (pop t current-instruction 2 15)
   (deb position 1 3)
   (pop current-instruction current-instruction-type 4 15)
   (pop registers current-register 5 5)
   (deb current-instruction-type 6 8)
   (deb current-instruction-type 7 9)
   (push current-register s 4)
   (inc current-register 10)
   (inc current-instruction 11)
   (copy current-instruction position 12)
   (pop current-instruction position 13 13)
   (push current-register registers 14)
   (deb current-register 12 10)
   (pop s current-register 12 0)
   (pop registers 0 16 16)
   (end)])

(def add [(deb 2 1 2)
          (inc 0 0)
          (deb 1 3 4)
          (inc 0 2)
          (end)])

(defn instruction->edge [[instruction register a b c]]
  (case instruction
    :inc [a]
    :deb [a b]
    :copy [b]
    :push [b]
    :pop [b c]
    :end []))

(defn label-for-line [urm l]
  (if (= l -1 )
    "α"
    (let [instruction (nth urm l)
          [type register second-register] instruction
          second-register (if (or  (= type :copy)
                                   (= type :push)
                                   (= type :pop)
                                   )
                            second-register
                            "")
          ]
      (cond (= type :end)
            "Ω"
            :else
            (str "(" (name  type) " " register second-register ")")))))


;; (defn draw-urm [urm]
;;   (let [adjacents (merge {-1 [0]} (zipmap (range) (map instruction->edge urm)))]
;;     (rhizome.viz/view-graph (keys adjacents) adjacents
;;                             :node->descriptor (fn [n] {:label (label-for-line urm n)
;;                                                        :shape :circle})
;;                             :edge->descriptor (fn [l r]
;;                                                 )
;;                             :options {:ratio 0.75
;;                                       :size 10}

;;                             )))

;; (defn save-urm [urm filename]
;;   (let [adjacents (merge {-1 [0]} (zipmap (range) (map instruction->edge urm)))]
;;     (rhizome.viz/save-graph (keys adjacents) adjacents
;;                             :node->descriptor (fn [n] {:label (label-for-line urm n)
;;                                                        :shape :circle})
;;                             :edge->descriptor (fn [l r]
;;                                                 )
;;                             :options {:ratio 0.75
;;                                       :size 800}
;;                             :filename filename)))



(defn -main []
  (eval-urm uurm
                  [(code-program add)
                   (code-list [0 2 3])
                   ]))
