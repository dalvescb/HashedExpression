# HashedExpression

## Style Guide
- hindent
    - Install: `stack install hindent`
    - `hindent --indent-size 4 $FILE_PATH$`    

## Testing
We use the following packages for testing:
- QuickCheck
- Hspec

Tests should be stored in `test/`.

In order to execute tests, execute the following from the main directory:
`stack test` or `stack build --test`