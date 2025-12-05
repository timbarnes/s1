(load "scheme/test-harness.scm")

(displayln "         =====Basic Tests=====")
(load "scheme/basic_tests.scm")

(displayln "         =====Advanced Tests=====")
(load "scheme/advanced_tests.scm")

(display "          === All tests completed ===")
(newline)
(if (null? **failed-tests**)
    (displayln "===  All" **counter** "tests passed!  ===")
    (begin
        (displayln "Failing tests: ")
        (displayln "==============")
        (map displayln **failed-tests**)))
