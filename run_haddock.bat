:: SPDX-License-Identifier: MIT
:: Copyright (C) 2021 Roland Csaszar
::
:: Project:  TestHaskell
:: File:     run_haddock.bat
:: Date:     18.May.2021
:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

:: Run Haddock to generate the library's reference dcumentation to doc/html.
stack clean
stack haddock --haddock-arguments="--odir=docs/html --html --built-in-themes"
