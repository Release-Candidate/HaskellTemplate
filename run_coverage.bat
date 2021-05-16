:: SPDX-License-Identifier: MIT
:: Copyright (C) 2021 Roland Csaszar
::
:: Project:  TestHaskell
:: File:     run_coverage.bat
:: Date:     16.May.2021
:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

@echo off

for /f "delims=" %%l in ('stack path --local-hpc-root') do (
    set RESULT=%%l
    call :TRIM RESULT
)

@echo on
stack clean
stack test --coverage
stack exec -- hpc-lcov --file %RESULT%\TestHaskell\TestHaskell-test\TestHaskell-test.tix

@echo off
GOTO :EOF

:: trim spaces off the strings
:TRIM
SetLocal EnableDelayedExpansion
Call :TRIMHELPER %%%1%%
EndLocal & set %1=%helper_tmp%
GOTO :EOF

:TRIMHELPER
set helper_tmp=%*
GOTO :EOF
