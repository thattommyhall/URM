(ns urm.core
  (:refer-clojure :exclude [inc pop])
  (:require [clojure.pprint :refer [pprint]]
            [clojure.math.numeric-tower :as math]))

(declare un<<>> <<>>)

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
                (assoc-in [:registers to] (<<>> from-value to-value))
                (assoc-in [:registers from] 0)
                (assoc :position exit)))
    :pop  (let [[from to halt exit] args
                from-value (get-in state [:registers from] 0)
                exit? (== 0 from-value)]
            (if exit?
              (assoc state :position exit)
              (let [[h t] (un<<>> from-value)]
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

(defn <<>> [x y]
  (* (math/expt 2 x)
     (+ (* 2 y) 1)))

(defn un<<>> [n]
  (let [x (factors-of-2 n)
        y (/ (dec (/ n (math/expt 2 x)))
             2)]
    [x y]))

(defn <> [x y]
  (dec (<<>> x y)))

(defn un<> [n]
  (un<<>> (+ n 1)))

(defn code-list [[h & t :as number-list]]
  (if (empty? number-list)
    0
    (<<>> h (code-list t))))

(defn decode-list [code]
  (if (== code 0)
    '()
    (let [[h code'] (un<<>> code)]
      (cons h
            (decode-list code')))))

(defn code-instruction [[instruction register jump-to branch-on-zero]]
  (case instruction
    :inc (<<>> (* 2 register) jump-to)
    :deb (<<>> (+ (* 2 register) 1)
               (<> jump-to branch-on-zero))
    :end 0))

(defn decode-instruction [code]
  (if (== code 0)
    (end)
    (let [[y z] (un<<>> code)]
      (if (even? y)
        (inc (/ y 2) z)
        (let [[j k] (un<> z)]
          (deb (/ (dec y) 2)
               j
               k))))))

(defn code-program [instructions]
  (code-list (map code-instruction instructions)))

(defn decode-program [code]
  (map decode-instruction (decode-list code)))

(def p 1)
(def a 2)
(def pc 3)
(def n 4)
(def c 5)
(def r 6)
(def s 7)
(def t 8)
(def z 9)

(def uurm
  [(copy p t 1)                         ; 0
   (pop t n 2 15)                       ; 1
   (deb pc 1 3)                         ; 2
   (pop n c 4 15)                       ; 3
   (pop a r 5 5)                        ; 4
   (deb c 6 8)                          ; 5
   (deb c 7 9)                          ; 6
   (push r s 4)                         ; 7
   (inc r 10)                           ; 8
   (inc n 11)                           ; 9
   (copy n pc 12)                       ; 10
   (pop n pc 13 13)                     ; 11
   (push r a 14)                        ; 12
   (deb r 12 10)                        ; 13
   (pop s r 12 0)                       ; 14
   (pop a 0 16 16)                      ; 15
   (end)])                              ; 16



(defn -main []
  (eval-urm uurm
            [(code-program [(inc 0 1)
                            (end)])
             (code-list [0 0 0 0])
             0]))
