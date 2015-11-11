(ns urm.core-test
  (:refer-clojure :exclude [inc])
  (:require [expectations :refer :all]
            [urm.core :refer :all]))

(expect 1
        (eval-urm [(inc 0 1)
                   (end)]
                  []))

(def add [(deb 2 1 2)
          (inc 0 0)
          (deb 1 3 4)
          (inc 0 2)
          (end)])

(expect []
        (comp-urm []
                  []))

(expect 3
        ((urm->fn add) 1 2))

(expect [(deb 0 0 1)
         (deb 1 1 2)
         (end)]
        (comp-urm (zero 0)
                  (zero 1)))

(expect [(deb 0 0 1)
         (deb 1 1 2)
         (deb 2 2 3)
         (end)]
        (comp-urm (zero 0)
                  (zero 1)
                  (zero 2)))

(expect (<<>> 0 13)
        (<> 2 3))

(expect 8
        (code-list [3]))

(expect 34
        (code-list [1 3]))

(expect 276
        (code-list [2 1 3]))

(expect 18
        (code-instruction (deb 0 0 2)))

(expect (end)
        (decode-instruction 0))

(expect (deb 0 0 2)
        (decode-instruction 18))

(expect 3
        (factors-of-2 8))

(expect [0 13]
        (un<<>> 27))

(expect [2 3]
        (un<> 27))

(expect 786432
        (code-program [(deb 0 0 2)
                       (end)]))

(expect [(deb 0 0 2)
         (end)]
        (decode-program 786432))

(expect {1 4
         2 0
         3 5}
        (:registers (run {:program (zero 2)
                             :position 0
                             :registers {1 4
                                         2 5
                                         3 5}})))

(expect {1 4
         2 4
         9 0}
 (:registers (run {:program (copy 1 2)
                   :position 0
                   :registers {1 4
                               2 5
                               9 0}})))

(expect { 1 0
          2 (<<>> 0 13)
          9 0}

 (:registers (run {:program (push 1 2)
                   :position 0
                   :registers {1 0
                               2 13
                               9 0}})))

(expect { 1 11
          2 3
          9 0}

 (:registers (run {:program (pop 1 2)
                   :position 0
                   :registers {1 (<<>> 3 11)
                               2 0
                               9 0}})))

;; (expect 2
;;         (eval-urm full-urm
;;                   [(code-program add)
;;                    (code-list [1 1])
;;                    0]))
