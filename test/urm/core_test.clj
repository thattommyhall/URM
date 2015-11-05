(ns urm.core-test
  (:require [expectations :refer :all]
            [urm.core :refer :all]))

(expect 2
        (eval-urm [(inc 0 1)
                   (end)]
                  [1]))

(def add (urm->fn [(deb 1 1 2)
                   (inc 0 0)
                   (end)]))

(expect 3
        (add 1 2))

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
