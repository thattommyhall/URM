(ns urm.core)

(defn inc [register jump-to]
  [:inc register jump-to])

(defn deb [register jump-to branch-on-zero]
  [:deb register jump-to branch-on-zero])

(defn end []
  [:end])

(defn inc-fn [register jump-to]
  (fn [{:keys [position registers] :as state}]
    (let [current (get-in state [:registers register] 0)]
      (-> state
          (assoc-in [:registers register] (+ 1 current))
          (assoc :position jump-to)))))

(defn deb-fn [register jump-to branch-on-zero]
  (fn [{:keys [position registers] :as state}]
    (let [current (get-in state [:registers register] 0)
          branch? (= current 0)]
      (-> state
          (assoc-in [:registers register] (if branch? current (dec current)))
          (assoc :position (if branch? branch-on-zero jump-to))))))

(defn end-fn [] identity)

(defn next-state [{:keys [position program] :as state}]
  (let [next-fn (nth program position)]
    (next-fn state)))

(defn run [state]
  (let [next (next-state state)]
    (if (= next state)
      (get-in state [:registers 0])
      (recur next))))

(def lookup { :deb deb-fn :inc inc-fn :end end-fn })

(defn functionise [[instruction & args]]
  (apply (lookup instruction) args))

(defn urm->fn [statements]
  (let [program (map functionise statements)]
    (fn [& args]
      (run {:program program
            :position 0
            :registers (zipmap (range) args)}))))

(defn eval-urm [statements args]
  (apply (urm->fn statements) args))

(defn inc-by [n]
  (fn [[instruction register jump-to branch-on-zero :as statement]]
    (case instruction
      :deb (deb register (+ jump-to n) (+ branch-on-zero n))
      :inc (inc register (+ jump-to n))
      :end (end))))

(defn increment-instruction-numbers-by [urm n]
  (map (inc-by n) urm))

(defn zero [register]
  [(deb register 0 1)
   (end)])

(defn comp-urm
  ([urm1 urm2 & urms]
   (comp-urm urm1
             (apply comp-urm urm2 urms)))
  ([urm1 urm2]
   (let [length1 (dec (count urm1))
         start-of-program (take length1 urm1)
         rest-of-program (increment-instruction-numbers-by urm2 length1)
         ]
     (concat start-of-program
             rest-of-program))))

(defn pow [a n]
  (bigint (Math/pow a n)))

(defn divides? [n div]
  (= 0 (rem n div)))

(defn factors-of-2
  ([n] (factors-of-2 n 0))
  ([n so-far]
   (if (divides? n 2)
     (recur (/ n 2)
            (+ so-far 1))
     so-far)))

(defn <<>> [x y]
  (* (pow 2 x)
     (+ (* 2 y) 1)))

(defn un<<>> [n]
  (let [x (factors-of-2 n)
        y (/ (dec (/ n (pow 2 x)))
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
  (if (= code 0)
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
  (if (= code 0)
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
