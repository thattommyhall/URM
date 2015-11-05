(ns urm.core)

(defn Inc [register jump-to]
  (fn [{:keys [position registers] :as state}]
    (let [current (get-in state [:registers register] 0)]
      (-> state
          (assoc-in [:registers register] (inc current))
          (assoc :position jump-to)))))

(defn Deb [register jump-to branch-on-zero]
  (fn [{:keys [position registers] :as state}]
    (let [current (get-in state [:registers register] 0)
          branch? (= current 0)]
      (-> state
          (assoc-in [:registers register] (if branch? current (dec current)))
          (assoc :position (if branch? branch-on-zero jump-to))))))

(defn End [] identity)

(defn next-state [{:keys [position program] :as state}]
  (let [next-fn (nth program position)]
    (next-fn state)))

(defn run [state]
  (let [next (next-state state)]
    (if (= next state)
      (get-in state [:registers 0])
      (recur next))))

(def lookup { 'Deb Deb 'Inc Inc 'End End })

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
