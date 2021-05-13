:: SPDX-License-Identifier: MIT
:: Copyright (C) 2021 Roland Csaszar
::
:: Project:  TestHaskell
:: File:     make_sh_executable.bat
:: Date:     13.May.2021
:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

:: Add the executable bit to '.sh' shell scripts, as this is a problem on
:: Windows filessystems.

"C:\Program Files\git\usr\bin\find"  ./ -name "*.sh" -exec git add --chmod=+x {} ;
