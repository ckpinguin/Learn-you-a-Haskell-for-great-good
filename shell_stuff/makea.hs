{-# LANGUAGE OverloadedStrings #-}

import Shelly

main = shelly $ do
  appendfile "a.hs" "main = print \"hello\""
  run "ghc" ["a.hs"]
  out <- run "./a" []
  out <- run "rm" ["a.hs"]
  echo out
