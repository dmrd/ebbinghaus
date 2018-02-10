(ns ebbinghaus.test-runner
  (:require
   [doo.runner :refer-macros [doo-tests]]
   [ebbinghaus.core-test]
   [ebbinghaus.common-test]))

(enable-console-print!)

(doo-tests 'ebbinghaus.core-test
           'ebbinghaus.common-test)
