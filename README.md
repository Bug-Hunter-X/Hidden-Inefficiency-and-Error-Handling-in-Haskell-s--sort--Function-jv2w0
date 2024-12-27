# Haskell Sort Bug

This repository demonstrates a subtle bug related to Haskell's built-in `sort` function. The `sort` function, while generally efficient, can exhibit performance issues with very large lists or lists containing elements expensive to compare. Additionally, it lacks explicit error handling for incomparable elements.

The `bug.hs` file contains the buggy code. The `bugSolution.hs` provides improved solutions.