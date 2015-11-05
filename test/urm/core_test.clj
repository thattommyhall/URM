(ns urm.core-test
  (:require [expectations :refer :all]
            [urm.core :refer :all]))

(expect 2
        (eval-urm '[(Inc 0 1)
                    (End)]
                  [1]))

(def add (urm->fn '[(Deb 1 1 2)
                    (Inc 0 0)
                    (End)]))

(expect 3
        (add 1 2))
