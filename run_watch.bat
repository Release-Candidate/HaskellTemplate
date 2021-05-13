:: SPDX-License-Identifier: MIT
:: Copyright (C) 2021 Roland Csaszar
::
:: Project:  TestHaskell
:: File:     run_watch.bat
:: Date:     13.May.2021
:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

:: Watches for file changes to rebuild and rerun the tests.
stack test --fast --haddock-deps --file-watch
