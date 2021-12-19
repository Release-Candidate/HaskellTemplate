:: SPDX-License-Identifier: MIT
:: Copyright (C) 2021 Roland Csaszar
::
:: Project:  TestHaskell
:: File:     run_code.bat
:: Date:     13.May.2021
:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

:: Run VS Code or Code Insiders

cd %~dp0
echo %cd%

:: Code:
:: stack exec "%LOCALAPPDATA%\Programs\Microsoft VS Code\bin\code"

:: Code:
stack exec "%APPDATA%\Code-Haskell\bin\code" TestHaskell.code-workspace
