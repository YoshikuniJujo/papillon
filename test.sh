echo
echo simple.papillon
runhaskell -isrc -Wall bin/papillon test/peg/simple.papillon > tmp/simple.hs && \
runhaskell -isrc -Wall tmp/simple.hs
cp test/peg/simple.papillon tmp/simple_th.hs
runhaskell -isrc -Wall tmp/simple_th.hs

echo
echo arith.papillon
runhaskell -isrc -Wall bin/papillon test/peg/arith.papillon > tmp/arith.hs && \
runhaskell -isrc -Wall tmp/arith.hs
cp test/peg/arith.papillon tmp/arith_th.hs
runhaskell -isrc -Wall tmp/arith_th.hs

echo
echo test.papillon
runhaskell -isrc -Wall bin/papillon test/peg/test.papillon > tmp/test.hs && \
runhaskell -isrc -Wall tmp/test.hs
cp test/peg/test.papillon tmp/test_th.hs
runhaskell -isrc -Wall tmp/test_th.hs

echo
echo testChar.papillon
runhaskell -isrc -Wall bin/papillon test/peg/testChar.papillon > tmp/testChar.hs && \
runhaskell -isrc -Wall tmp/testChar.hs
cp test/peg/testChar.papillon tmp/testChar_th.hs
runhaskell -isrc -Wall tmp/testChar_th.hs

echo
echo otherTypes.papillon
runhaskell -isrc -Wall bin/papillon test/peg/otherTypes.papillon > tmp/otherTypes.hs && \
runhaskell -isrc -Wall tmp/otherTypes.hs
cp test/peg/otherTypes.papillon tmp/otherTypes_th.hs
runhaskell -isrc -Wall tmp/otherTypes_th.hs

echo
echo testNotAfter.papillon
runhaskell -isrc -Wall bin/papillon test/peg/testNotAfter.papillon > tmp/testNotAfter.hs && \
runhaskell -isrc -Wall tmp/testNotAfter.hs
cp test/peg/testNotAfter.papillon tmp/testNotAfter_th.hs
runhaskell -isrc -Wall tmp/testNotAfter_th.hs

echo
echo parseError.papillon
runhaskell -isrc -Wall bin/papillon test/peg/parseError.papillon > tmp/parseError.hs && \
runhaskell -isrc -Wall tmp/parseError.hs
cp test/peg/parseError.papillon tmp/parseError_th.hs
runhaskell -isrc -Wall tmp/parseError_th.hs

echo
echo testZoi.papillon
runhaskell -isrc -Wall bin/papillon test/peg/testZoi.papillon > tmp/testZoi.hs && \
runhaskell -isrc -Wall tmp/testZoi.hs
cp test/peg/testZoi.papillon tmp/testZoi_th.hs
runhaskell -isrc -Wall tmp/testZoi_th.hs

echo
echo testList.papillon
runhaskell -isrc -Wall bin/papillon test/peg/testList.papillon > tmp/testList.hs && \
runhaskell -isrc -Wall tmp/testList.hs
cp test/peg/testList.papillon tmp/testList_th.hs
runhaskell -isrc -Wall tmp/testList_th.hs

echo
echo testParen.papillon
runhaskell -isrc -Wall bin/papillon test/peg/testParen.papillon > tmp/testParen.hs && \
runhaskell -isrc -Wall tmp/testParen.hs
cp test/peg/testParen.papillon tmp/testParen_th.hs
runhaskell -isrc -Wall tmp/testParen_th.hs

echo
echo testType.papillon
runhaskell -isrc -Wall bin/papillon test/peg/testType.papillon > tmp/testType.hs && \
runhaskell -isrc -Wall tmp/testType.hs
cp test/peg/testType.papillon tmp/testType_th.hs
runhaskell -isrc -Wall tmp/testType_th.hs

echo
echo testOp.papillon
runhaskell -isrc -Wall bin/papillon test/peg/testOp.papillon > tmp/testOp.hs && \
runhaskell -isrc -Wall tmp/testOp.hs
cp test/peg/testOp.papillon tmp/testOp_th.hs
runhaskell -isrc -Wall tmp/testOp_th.hs

echo
echo testListLit.papillon
runhaskell -isrc -Wall bin/papillon test/peg/testListLit.papillon > tmp/testListLit.hs && \
runhaskell -isrc -Wall tmp/testListLit.hs
cp test/peg/testListLit.papillon tmp/testListLit_th.hs
runhaskell -isrc -Wall tmp/testListLit_th.hs

echo
echo testPatOp.papillon
runhaskell -isrc -Wall bin/papillon test/peg/testPatOp.papillon > tmp/testPatOp.hs && \
runhaskell -isrc -Wall tmp/testPatOp.hs
cp test/peg/testPatOp.papillon tmp/testPatOp_th.hs
runhaskell -isrc -Wall tmp/testPatOp_th.hs
