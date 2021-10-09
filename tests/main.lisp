(defpackage outside/tests/main
  (:use :cl
        :outside
        :rove))
(in-package :outside/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :random)' in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
    (ok (= 1 1))))

