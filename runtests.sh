#!/bin/bash

# run tests, quick check 100 times for quick check props
runhaskell -isrc -it t/Spec.hs -a100
