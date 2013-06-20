runhaskell -isrc src/papillon src/Text/Papillon/Parser.peg > tmp/Parser.hs
#papillon src/Text/Papillon/Parser.peg > tmp/Parser.hs
mv tmp/Parser.hs src/Text/Papillon/Parser.hs
