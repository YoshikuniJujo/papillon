runhaskell -isrc -Wall src/papillon test/peg/arith.papillon > tmp/arith.hs && \
runhaskell -isrc -Wall tmp/arith.hs
cp test/peg/arith.papillon tmp/arith_th.hs
runhaskell -isrc -Wall tmp/arith_th.hs

runhaskell -isrc -Wall src/papillon test/peg/test.papillon > tmp/test.hs && \
runhaskell -isrc -Wall tmp/test.hs
cp test/peg/test.papillon tmp/test_th.hs
runhaskell -isrc -Wall tmp/test_th.hs

runhaskell -isrc -Wall src/papillon test/peg/testChar.papillon > tmp/testChar.hs && \
runhaskell -isrc -Wall tmp/testChar.hs
cp test/peg/testChar.papillon tmp/testChar_th.hs
runhaskell -isrc -Wall tmp/testChar_th.hs
