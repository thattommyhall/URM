(ns urm.core-test
  (:refer-clojure :exclude [inc pop])
  (:require [expectations :refer :all]
            [urm.core :refer :all]))

(expect 1
        (eval-urm [(inc 0 1)
                   (end)]
                  []))

(expect 3
        ((urm->fn add) 1 2))

(expect (encode-pair [0 13])
        (encode-pair* [2 3]))

(expect 8
        (encode-list [3]))

(expect 34
        (encode-list [1 3]))

(expect 276
        (encode-list [2 1 3]))

(expect 18
        (encode-instruction (deb 0 0 2)))

(expect (end)
        (decode-instruction 0))

(expect (deb 0 0 2)
        (decode-instruction 18))

(expect (deb 0 0 2)
        (decode-instruction (encode-instruction (deb 0 0 2))))

(expect 3
        (factors-of-2 8))

(expect [0 13]
        (decode-pair 27))

(expect 28
        (encode-pair (decode-pair (encode-pair [2 3]))))

(expect 71680
        (encode-pair (decode-pair (encode-pair [11 17]))))

(expect [2 1 3]
        (decode-list (encode-list (decode-list 276))))

(expect [2 3]
        (uncode-pair* 27))

(expect [2 3]
        (uncode-pair* (encode-pair* [2 3])))

(expect 27
        (encode-pair* (uncode-pair* (encode-pair* [2 3]))))

(expect 786432
        (encode-program [(deb 0 0 2)
                       (end)]))

(expect [(deb 0 0 2)
         (end)]
        (decode-program 786432))

(expect [(deb 0 0 2)
         (end)]
        (decode-program (encode-program (decode-program 786432))))

(expect  786432
        (encode-program (decode-program (encode-program [(deb 0 0 2)
                                                         (end)]))))

(expect {1 4
         2 0
         3 5}

        (:registers (run {:program (zero 2)
                          :position 0
                          :registers {1 4
                                      2 5
                                      3 5}})))

(expect {1 4
         2 4}

        (:registers (run {:program [(copy 1 2 1)
                                    (end)]
                          :position 0
                          :registers {1 4
                                      2 5
                                      }})))

(expect {1 0
         2 (encode-list [4 5 6])
         9 0}

        (:registers (run {:program [(push 1 2 1)
                                    (end)]
                          :position 0
                          :registers {1 4
                                      2 (encode-list [5 6])
                                      9 0}})))

(expect {1 11
         2 3
         9 0}

        (:registers (run {:program   [(pop 1 2 1 1)
                                      (end)]
                          :position  0
                          :registers {1 (encode-pair [3 11])
                                      2 0
                                      9 0}})))

(expect {1 0
         2 0
         3 1}

        (:registers (run {:program [(pop 1 2 2 1)
                                    (inc 3 2)
                                    (end)]
                          :position 0
                          :registers {1 0
                                      2 0
                                      }})))

(expect 1
        (eval-urm uurm
                  [(encode-program [(inc 0 1)
                                  (end)])
                   (encode-list [0])
                   ]))

(expect 2
        (eval-urm uurm
                  [(encode-program add)
                   (encode-list [0 1 1])
                   ]))
