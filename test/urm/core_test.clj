(ns urm.core-test
  (:require [expectations :refer :all]
            [urm.core :refer :all]))

(expect 2
        ((URM (Inc 0 1)
              End)
         1))

(def add (URM (Deb 1 1 2)
              (Inc 0 0)
              End))

(expect 3
        (add 1 2))
