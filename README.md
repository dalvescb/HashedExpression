# HashedExpression [![Build Status](https://travis-ci.org/dalvescb/HashedExpression.svg?branch=master)](https://travis-ci.org/dalvescb/HashedExpression)

## Style Guide
- hindent
    - Install: `stack install hindent`
    - `hindent --indent-size 4 $FILE_PATH$`    

## Check for non-exhaustive patterns:
- `stack clean`
- `stack build --fast --ghc-options -Wincomplete-patterns`
