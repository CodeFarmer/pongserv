(ns pongserv.core-test
  (:require [clojure.test :refer [deftest is testing]])
  (:use [pongserv.core]))

(deftest rects-not-colliding
  (is (false? (rects-collide? [0  0 10 10]
                              [20 0 10 10])))
  (is (false? (rects-collide? [0  0 10 10]
                              [0 20 10 10]))))

(deftest rects-colliding
  (is (true?  (rects-collide? [0 0 10 10]
                              [5 0 10 10])))
  (is (true? (rects-collide?  [0 0 10 10]
                              [0 5 10 10]))))
