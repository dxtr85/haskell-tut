-- file: ch05/Setup.hs
#!/usr/bin/env runhaskell
import Distribution.Simple
main = defaultMain


-- Steps to create haskell package:
-- 1. Create .cabal file
-- 2. Create Setup.hs file
-- 3. Execute: runghc Setup configure
-- 4. Execute: runghc Setup build
-- 5. Execute: runghc Setup install
