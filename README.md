# TestHaskell

This is a Haskell Github template repository and Stack template file. The whole repository is contained in the Stack template [BigTemplate.hsfiles](./BigTemplate.hsfiles).

To use it, see section [Usage](#usage).

[Below is actually text for usage in the project later, not needed for this template.]

Downloads and releases can be found in the section [Download](#download).

Detailed documentation can be found at [Read the Docs](https://haskelltemplate.readthedocs.io/en/latest/).

[![Haskell badge](https://img.shields.io/badge/uses-Haskell-brightgreen?style=flat)](https://www.haskell.org/)
[![HSpec badge](https://img.shields.io/badge/uses-Hspec-brightgreen?style=flat)](https://hspec.github.io/)
[![GitHub License badge](https://img.shields.io/github/license/Release-Candidate/HaskellTemplate)](https://github.com/Release-Candidate/HaskellTemplate/blob/main/LICENSE)
[![Documentation Status](https://readthedocs.org/projects/haskelltemplate/badge/?version=latest)](https://haskelltemplate.readthedocs.io/en/latest/?badge=latest)
[more badges ...](#badges)

- [TestHaskell](#testhaskell)
  - [Usage](#usage)
    - [Haskell and Stack](#haskell-and-stack)
    - [MkDocs and Pipenv](#mkdocs-and-pipenv)
    - [What is What?](#what-is-what)
      - [Scripts](#scripts)
      - [GitHub Workflows & Issue Templates](#github-workflows--issue-templates)
      - [MkDocs documentation](#mkdocs-documentation)
      - [Haskell Source](#haskell-source)
      - [Visual Studio Code](#visual-studio-code)
  - [Download](#download)
  - [Badges](#badges)
    - [GitHub Workflows](#github-workflows)
    - [CodeCov Coverage Report](#codecov-coverage-report)

## Usage

You can either use this GitHub template repository as a GitHub template for a Haskell project and manually change all occurrences of `TestHaskell`, `Release-Candidate` and my name. Or use [BigTemplate.hsfiles](./BigTemplate.hsfiles) to generate a local project from this repository, using `stack new`:

```shell
stack new PROJECT_NAME https://raw.githubusercontent.com/Release-Candidate/HaskellTemplate/main/BigTemplate.hsfiles
```

### Haskell and Stack

You need [Stack](https://docs.haskellstack.org/en/stable/README/) to install everything else (Stack installs the GHC, the Haskell compiler too). Install that using your distributions package manager, Homebrew on Macs or Chocolatey on Windows or use another way - see [Stack - Howto Install](https://docs.haskellstack.org/en/stable/README/#how-to-install).

To generate a new project named `PROJECT_NAME` using this template, you have to call `stack new` with it:

```shell
stack new PROJECT_NAME https://raw.githubusercontent.com/Release-Candidate/HaskellTemplate/main/BigTemplate.hsfiles
```

Enter the directory `PROJECT_NAME` and execute

```shell
./install_build_tools
```

or, on Windows:

```shell
install_build_tools.bat
```

to locally install all needed Haskell build tools. Now calling

```shell
stack build
```

should successfully build the template project.

### MkDocs and Pipenv

To generate the documentation using MkDocs, a virtual Python environment is needed.
So, first you need to install Python, if you don't have it installed already - either from your distributions repository, using the XCode or [Homebrew](https://brew.sh/) version, or getting it from [Python.org](https://www.python.org/downloads/).

See

- [Using Python on Windows](https://docs.python.org/3/using/windows.html)
- [Using Python on a Macintosh](https://docs.python.org/3/using/mac.html)
- [Using Python on Unix Platforms](https://docs.python.org/3/using/unix.html)

In the file `Pipfile` there is a stanza saying

```ini
[requires]
python_version = "3.9"
```

That's just because I used 3.9 when generating that documentation, and Pipenv is picky about the version mentioned there. Just edit that to match your installed Python version.

Install `pipenv` using the package manager pip

```shell
pip install pipenv
```

Now you're ready to download and install the needed packages using pipenv

```shell
pipenv install --dev
```

After that you can use MkDocs.

Call

```shell
pipenv run mkdocs serve
```

in the root directory (`PROJECT_NAME`) and connect to the running webserver at [http://127.0.0.1:8000](http://127.0.0.1:8000).
This preview shows changes in realtime, so any changes to the markdown files in `docs` you
see as preview as soon as you save the file. The generated HTML files are saved in the directory
`sites`.

### What is What?

A list of all files in this template and what they do or configure.

- [README.md](./README.md) - The file that you are reading right now.
- [LICENSE](./LICENSE) - The project'S license, MIT.
- [CHANGELOG.md](./CHANGELOG.md) - The project's changelog.

#### Scripts

- [make_template_file.sh](./make_template_file.sh) - Unix-ish only: generate the template file [BigTemplate.hsfiles](./BigTemplate.hsfiles) from the files in this repository.
- [install_build_tools.bat](./install_build_tools.bat) - Locally installs all needed build tools using `stack install`.
- [install_build_tools.sh](./install_build_tools.sh) - Locally installs all needed build tools using `stack install`.
- [run_watch.bat](./run_watch.bat) - Watches for source code changes to continuously rebuild and run tests. Calls:

  ```shell
  stack test --fast --haddock-deps --file-watch
  ```

- [run_watch.sh](./run_watch.sh) - Watches for source code changes to continuously rebuild and run tests. Calls:

  ```shell
  stack test --fast --haddock-deps --file-watch
  ```

- [run_coverage.bat](./run_coverage.bat) - Generate a LCov coverage report `./lcov.info`, by calling

  ```shell
  stack clean
  stack test --coverage
  stack exec -- hpc-lcov --file %RESULT%\TestHaskell\TestHaskell-test\TestHaskell-test.tix
  ```

- [run_coverage.sh](./run_coverage.sh) - Generate a LCov coverage report `./lcov.info`, by calling

  ```shell
  stack clean
  stack test --coverage
  stack exec -- hpc-lcov --file ${TIX_FILE}
  ```

- [run_codecov_coverage.bat](./run_codecov_coverage.bat) - Generates a CodeCov coverage report `./coverage.json`, by calling

  ```shell
  stack clean
  stack test --coverage
  stack exec -- hpc-codecov --verbose -o coverage.json %RESULT%\TestHaskell\TestHaskell-test\TestHaskell-test.tix -m %MIX_DIR%\hpc
  ```

- [run_codecov_coverage.sh](./run_codecov_coverage.sh) - Generates a CodeCov coverage report `./coverage.json`, by calling

  ```shell
  stack clean
  stack test --coverage
  stack exec -- hpc-codecov --verbose -o coverage.json ${TIX} -m ${MIX_DIR}\hpc
  ```

- [run_haddock.bat](./run_haddock.bat) - Runs Haddock to generate the source documentation in `docs/html/`. Calls

  ```shell
  stack clean

  stack haddock --haddock-arguments="--odir=docs/html --html --built-in-themes"

  ```

- [run_haddock.sh](./run_haddock.sh) - Runs Haddock to generate the source documentation in `docs/html/`. Calls

  ```shell
  stack clean

  stack haddock --haddock-arguments="--odir=docs/html --html --built-in-themes"

  ```

- [run_hoogle.bat](./run_hoogle.bat) - Runs Hoogle to create the local search index. Calls:

  ```shell
  stack hoogle -- generate --local
  stack hoogle -- server --local --port=8080
  ```

- [run_hoogle.sh](./run_hoogle.sh)- Runs Hoogle to create the local search index. Calls:

  ```shell
  stack hoogle -- generate --local
  stack hoogle -- server --local --port=8080
  ```

- [run_code.bat](./run_code.bat) - Script to start Visual Studio Code (Insiders) with the Stack environment, so we don't need to add Stack's local `bin` directory to the user PATH. Calls

  ```shell
  stack exec "%LOCALAPPDATA%\Programs\Microsoft VS Code Insiders\bin\code-insiders" TestHaskell.code-workspace
  ```

- [run_code.sh](./run_code.sh) - Script to start Visual Studio Code (Insiders) with the Stack environment, so we don't need to add Stack's local `bin` directory to the user PATH.

  ```shell
  stack exec code-insiders TestHaskell.code-workspace
  ```

- [make_sh_executable.bat](./make_sh_executable.bat) - Windows only, recursively sets the executable bit of all shell scripts with a suffix of `.sh`.

- [./scripts/get_changelog.sh](./scripts/get_changelog.sh) - Script used by the GitHub workflow [create_packages.yml](./.github/workflows/create_packages.yml), to extract the latest part out of the changelog, the file [CHANGELOG.md](./CHANGELOG.md).

#### GitHub Workflows & Issue Templates

Directory [`.github/ISSUE_TEMPLATE`](./github/ISSUE_TEMPLATE/):

- [bug_report.md](./.github/ISSUE_TEMPLATE/bug_report.md) - Bug report template for GitHub
- [feature_request.md](./.github/ISSUE_TEMPLATE/feature_request.md) - Feature request template for GitHub

Directory [`.github/workflows/`](./.github/workflows/):

- [create_packages.yml](./.github/workflows/create_packages.yml) - A GitHub workflow to build the executable(s), install them to `./bin` and create a GitHub release (like [Latest Release at GitHub](https://github.com/Release-Candidate/HaskellTemplate/releases/latest)). This workflow runs on all 3 GitHub OSes, Linux, Mac OS X and Windows, it is started after tagging the source with a release tag of the form `v.?.?.?` or manually creating a release using GitHubs web-frontend.
- [linux_test.yml](./.github/workflows/linux_test.yml) - Run tests and coverage tests on Linux in 2 jobs, upload the coverage results to Codecov.
- [osx_test.yml](./.github/workflows/osx_test.yml) - Run tests and coverage tests on Mac OS X in 2 jobs, upload the coverage results to Codecov.
- [windows_test.yml](./.github/workflows/windows_test.yml) - Run tests and coverage tests on Windows in 2 jobs, upload the coverage results to Codecov.

#### MkDocs documentation

- [Pipfile](./Pipfile) - Packages nedded by MkDocs to install using `pipenv` and the package `mkdocs` itself.
- [mkdocs.yml](./mkdocs.yml) - The configuration file for MkDocs, contains the website's index:

  ```YML
  nav:
  - Home: index.md
  - Project Links:
      - "Downloads": https://github.com/Release-Candidate/TestHaskell/releases/latest
      - "GitHub Project Page": "https://github.com/Release-Candidate/TestHaskell"
      - "Report a Bug or a Feature Request": "https://github.com/Release-Candidate/TestHaskell/issues/new/choose"
      - "Issue Tracker at GitHub": "https://github.com/Release-Candidate/TestHaskell/issues"
  - "Installation & Usage":
      - "Installation & Usage": usage.md
      - License: license.md
  - "Reference Documentation":
      - "Library Reference": html/index.html
  - Contributing:
      - Contributing: contributing.md
  ```

- [.readthedocs.yaml](./.readthedocs.yaml) - The configuration for [Read the Docs](https://readthedocs.org/), to host the generated documentation.

Directory [docs](./docs):

- [requirements.txt](docs/requirements.txt) - Packages (plugins for MkDocs) that have to be installed by Read the Docs to generate the documentation.
- [index.md](./docs/index.md) - The documentation's home page.
- [contributing.md](./docs/contributing.md) - Usage information.
- [contributing.md](./docs/contributing.md) - Information on how to contribute to the project.

Directory [docs/html/](./docs/html/): contains the documentation generated by Haddock using the script [run_haddock.sh](./run_haddock.sh)/[run_haddock.bat](./run_haddock.bat). Sadly I haven't found a way to generate that documentation using `mkdocs build`, which is what Read the Docs calls to build it. So for now it is included in the source repository (but not the Stack template file).

#### Haskell Source

Stack, Cabal and HPack configuration:

- [stack.yaml](./stack.yaml) - Stack configuration file, mainly the resolver (LTS) configuration and some extra packages.
- [stack.yaml.lock](./stack.yaml.lock) - Stack lockfile containing package/LTS versions.
- [package.yaml](./package.yaml) - The main Stack/HPack configuration file.
- [Setup.hs](./Setup.hs) - Cabal `Setup.hs` file.
- [hie.yaml](./hie.yaml) - The Hie configuration file, generated using

  ```shell
  stack exec gen-hie > hie.yaml
  ```

The Haskell source consists of three parts, the library in the directory `src`, the command line program in the directory `app` and the tests in the directory `test`.

- [./src/Lib.hs](./src/Lib.hs) - The library, Fibonacci and golden ratio calculations (you certainly have never seen anything similar!)
- [./app/Main.hs](./app/Main.hs) - The command line program, does nothing else but parse the command line arguments and call the library functions.
- [./test/Spec.hs](./test/Spec.hs) - The entry point of the tests.
- [./test/FibonacciSpec.hs](./test/FibonacciSpec.hs) - Fibonacci test cases, automatically discovered by Hspec
- [./test/GoldenRatioSpec.hs](./test/GoldenRatioSpec.hs) - Golden ratio test cases, automatically discovered by Hspec.

#### Visual Studio Code

- [run_code.bat](./run_code.bat) - Script to start Visual Studio Code (Insiders) with the Stack environment, so we don't need to add Stack's local `bin` directory to the user PATH. Calls

  ```shell
  stack exec "%LOCALAPPDATA%\Programs\Microsoft VS Code Insiders\bin\code-insiders" TestHaskell.code-workspace
  ```

- [run_code.sh](./run_code.sh) - Script to start Visual Studio Code (Insiders) with the Stack environment, so we don't need to add Stack's local `bin` directory to the user PATH.

  ```shell
  stack exec code-insiders TestHaskell.code-workspace
  ```

- [TestHaskell.code-workspace](./TestHaskell.code-workspace) - The Visual Studio Code workspace file.
- [.vscode/](./.vscode/) - Directory containing additional Visual Studio Code configuration.

## Download

That's actually just a placeholder for real text, in this template repo you only get to download a Fibonacci and golden ration command line program for Linux, OS X and Windows.

Download the latest release: [Latest Release at GitHub](https://github.com/Release-Candidate/HaskellTemplate/releases/latest)

Changelog of all versions: [CHANGELOG.md](./CHANGELOG.md)

All releases: [All Releases at GitHub](https://github.com/Release-Candidate/HaskellTemplate/releases)

## Badges

### GitHub Workflows

[![Tests Mac OS X latest](https://github.com/Release-Candidate/HaskellTemplate/actions/workflows/osx_test.yml/badge.svg)](https://github.com/Release-Candidate/HaskellTemplate/actions/workflows/osx_test.yml)
[![Tests Ubuntu 20.04](https://github.com/Release-Candidate/HaskellTemplate/actions/workflows/linux_test.yml/badge.svg)](https://github.com/Release-Candidate/HaskellTemplate/actions/workflows/linux_test.yml)
[![Tests Windows 2019](https://github.com/Release-Candidate/HaskellTemplate/actions/workflows/windows_test.yml/badge.svg)](https://github.com/Release-Candidate/HaskellTemplate/actions/workflows/windows_test.yml)

### CodeCov Coverage Report

[![codecov](https://codecov.io/gh/Release-Candidate/HaskellTemplate/branch/main/graph/badge.svg?token=L988V53VRA)](https://codecov.io/gh/Release-Candidate/HaskellTemplate)
