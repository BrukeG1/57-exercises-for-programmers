#!/bin/bash

# run tests, quick check 100 times for quick check props
runhaskell -isrc -itest test/Spec.hs -a100
