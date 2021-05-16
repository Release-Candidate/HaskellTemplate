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

for /f "delims=" %%l in ('stack path --dist-dir') do (
    set MIX_DIR=%%l
    call :TRIM MIX_DIR
)

@echo on
stack clean
stack test --coverage
stack exec -- hpc-codecov --verbose -o coverage.json %RESULT%\TestHaskell\TestHaskell-test\TestHaskell-test.tix -m %MIX_DIR%\hpc

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
