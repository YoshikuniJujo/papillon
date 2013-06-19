runhaskell -isrc src/papillon test/peg/arith.papillon > tmp/arith.hs && \
runhaskell -isrc tmp/arith.hs
cp test/peg/arith.papillon tmp/arith_th.hs
runhaskell -isrc tmp/arith_th.hs
