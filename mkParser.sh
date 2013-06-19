runhaskell -isrc src/papillon.hs src/Text/Papillon/Parser.peg > tmp/Parser.hs
mv tmp/Parser.hs src/Text/Papillon/Parser.hs
