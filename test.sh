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

runhaskell -isrc -Wall src/papillon test/peg/otherTypes.papillon > tmp/otherTypes.hs && \
runhaskell -isrc -Wall tmp/otherTypes.hs
cp test/peg/otherTypes.papillon tmp/otherTypes_th.hs
runhaskell -isrc -Wall tmp/otherTypes_th.hs

runhaskell -isrc -Wall src/papillon test/peg/testNotAfter.papillon > tmp/testNotAfter.hs && \
runhaskell -isrc -Wall tmp/testNotAfter.hs
cp test/peg/testNotAfter.papillon tmp/testNotAfter_th.hs
runhaskell -isrc -Wall tmp/testNotAfter_th.hs

runhaskell -isrc -Wall src/papillon test/peg/parseError.papillon > tmp/parseError.hs && \
runhaskell -isrc -Wall tmp/parseError.hs
cp test/peg/parseError.papillon tmp/parseError_th.hs
runhaskell -isrc -Wall tmp/parseError_th.hs

runhaskell -isrc -Wall src/papillon test/peg/testZoi.papillon > tmp/testZoi.hs && \
runhaskell -isrc -Wall tmp/testZoi.hs
cp test/peg/testZoi.papillon tmp/testZoi_th.hs
runhaskell -isrc -Wall tmp/testZoi_th.hs

runhaskell -isrc -Wall src/papillon test/peg/testList.papillon > tmp/testList.hs && \
runhaskell -isrc -Wall tmp/testList.hs
cp test/peg/testList.papillon tmp/testList_th.hs
runhaskell -isrc -Wall tmp/testList_th.hs
