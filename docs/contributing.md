# Contributing

Any help is welcome!

If you encounter a problem using TestHaskell, a task it not as easy as you'd like it to be or you'd like something added to it: open an issue at GitHub, see section [Report Issues](#report-issues-bugs-and-feature-requests).

- [Contributing](#contributing)
  - [Report Issues (Bugs and Feature Requests)](#report-issues-bugs-and-feature-requests)
  - [Forking the Repository](#forking-the-repository)
    - [Github Documentation on Collaborating with Issues and Pull Requests](#github-documentation-on-collaborating-with-issues-and-pull-requests)
  - [Developing TestHaskell](#developing-testhaskell)
    - [Changing and Generating Documentation](#changing-and-generating-documentation)
      - [Installing Dependencies](#installing-dependencies)
      - [MkDocs Files](#mkdocs-files)
      - [Haddock](#haddock)
      - [Read the Docs Configuration](#read-the-docs-configuration)
      - [GitHub Documentation](#github-documentation)
    - [Haskell Source](#haskell-source)
      - [Pipenv and MkDocs](#pipenv-and-mkdocs)
      - [Haskell Stack](#haskell-stack)
      - [Source Code and Tests](#source-code-and-tests)
        - [Library Sources](#library-sources)
        - [Executable Sources](#executable-sources)
        - [Test Sources](#test-sources)
      - [Build](#build)
  - [CodeCov Configuration](#codecov-configuration)
  - [GitHub Workflows](#github-workflows)
  - [GitHub Issue Templates](#github-issue-templates)
  - [What is What? - List of all Files](#what-is-what---list-of-all-files)
    - [Scripts](#scripts)
    - [GitHub Workflows & Issue Templates](#github-workflows--issue-templates)
    - [MkDocs documentation](#mkdocs-documentation)
    - [Haskell Source](#haskell-source-1)
    - [Visual Studio Code](#visual-studio-code)

## Report Issues (Bugs and Feature Requests)

Please help making TestHaskell better by filing bug reports and feature requests.

File a bug report at [Github](https://github.com/Release-Candidate/TestHaskell/issues/new?assignees=&labels=&template=bug_report.md&title=).
Add a feature request at [Github](https://github.com/Release-Candidate/TestHaskell/issues/new?assignees=&labels=&template=feature_request.md&title=).
Take a look at the [Issue Tracker at GitHub](https://github.com/Release-Candidate/TestHaskell/issues)

## Forking the Repository

If you'd like to contribute directly, e.g. better the documentation, add another language or write some source code: fork TestHaskell by clicking the `Fork` button in the upper right corner of the GitHub project website. Check out your fork of TestHaskell using the URL from the `Code` button of your fork on Github. The URL should be something like github.com/YOUR_USERNAME/TestHaskell.git.

Details about how to fork a repository on Github are [here](https://docs.github.com/en/github/getting-started-with-github/fork-a-repo).

Make your changes, push them to your forked repository and make a pull-request (e.g. using the Pull request-button above and right of GitHubs source file view).

See [GitHub on Pull-Requests](https://docs.github.com/en/github/collaborating-with-issues-and-pull-requests/proposing-changes-to-your-work-with-pull-requests) and another [How-To](https://github.com/MarcDiethelm/contributing/blob/master/README.md).

### Github Documentation on Collaborating with Issues and Pull Requests

See GitHub's documentation about how to contribute for details: [Collaborating with issues and pull requests](https://docs.github.com/en/github/collaborating-with-issues-and-pull-requests).

## Developing TestHaskell

### Changing and Generating Documentation

#### Installing Dependencies

To generate the documentation using MkDocs, a virtual Python environment is needed. First you need to install Python, if you don't have it installed already - either from your distributions repository, using the XCode or [Homebrew](https://brew.sh/) version, or getting it from [Python.org](https://www.python.org/downloads/).

See

- [Using Python on Windows](https://docs.python.org/3/using/windows.html)
- [Using Python on a Macintosh](https://docs.python.org/3/using/mac.html)
- [Using Python on Unix Platforms](https://docs.python.org/3/using/unix.html)

In the file [`Pipfile`](https://github.com/Release-Candidate/TestHaskell/blob/main/Pipfile) there is a stanza saying

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

in the root directory of TestHaskell and connect to the running webserver at [http://127.0.0.1:8000](http://127.0.0.1:8000).
This preview shows changes in realtime, so any changes to the markdown files in `docs` you see as preview as soon as you save the file. The generated HTML files are saved in the directory `sites`.

#### MkDocs Files

- `mkdocs.yml` - the MkDocs configuration, specially the configuration of the navigation sidebar in `nav` which you may need to edit

```yml
  nav:
    - Home: index.md
    - Project Links:
        - "Downloads": https://github.com/Release-Candidate/TestHaskell/releases/latest
        - "GitHub Project Page": "https://github.com/Release-Candidate/TestHaskell"
        - "Report a Bug or a Feature Request": "https://github.com/Release-Candidate/TestHaskell/issues/new/choose"
        - "Issue Tracker at GitHub": "https://github.com/Release-Candidate/TestHaskell/issues"
    - "Installation & Usage":
        - "Installation & Usage": usage.md
        - "License": license.md
    - "API Documentation":
        - "Library Reference": html/index.html
    - Contributing:
        - Contributing: contributing.md
```

- `docs/` - the markdown files that are used to generate the
   HTML sites in the directory `sites/`
- `docs/html` - the Haddock generated API reference files (as HTML). See [Haddock](#haddock)

#### Haddock

[Haddock](https://www.haskell.org/haddock/) is a Haskell code documentation tool that generates linked HTML documentation of your modules, functions and types. Is is generated using

```shell
stack stack haddock
```

In the project root there are the two scripts

- [run_haddock.bat](https://github.com/Release-Candidate/HaskellTemplate/blob/main/run_haddock.bat)
- [run_haddock.sh](https://github.com/Release-Candidate/HaskellTemplate/blob/main/run_haddock.sh)
which let Haddock generate the API documentation in the directory `docs/html` using the command

```shell
stack haddock --haddock-arguments="--odir=docs/html --html --built-in-themes"
```

!!! Warning
    The directory `docs/html` and its content need to be checked in to Git, because I didn't find a way to let Read the Docs generate the Haddock documentation when building the documentation. If you are not using Read the Docs to host your Documentation, this may not matter for you.

#### Read the Docs Configuration

- `.readthedocs.yaml` the configuration for Read the Docs
- `docs/requirements.txt` the packages needed by MkDocs
   when generating the documentation at Read the Docs.
   Locally needed packages are configured in `Pipfile`

Read the Docs automatically generates the MkDocs documentation after each `git push`.

#### GitHub Documentation

The Markdown documentation for GitHub are the files [README.md](https://github.com/Release-Candidate/TestHaskell/blob/main/README.md) and [CHANGELOG.md](https://github.com/Release-Candidate/TestHaskell/blob/main/CHANGELOG.md) in the project root directory.

### Haskell Source

Before you can use the configured Tools of this project, you have to download and install the needed tools.

#### Pipenv and MkDocs

To generate the documentation using MkDocs (see [Changing and Generating Documentation](#changing-and-generating-documentation)), a virtual Python environment is needed. So, first you need to install Python, if you don't have it installed already - either from your distributions repository, using the XCode or [Homebrew](https://brew.sh/) version, or getting it from [Python.org](https://www.python.org/downloads/).

In the file [`Pipfile`](https://github.com/Release-Candidate/TestHaskell/blob/main/Pipfile) there is a stanza saying

```ini
[requires]
python_version = "3.9"
```

  That's just because I used 3.9 when generating that
documentation, and Pipenv is picky about the version mentioned
there. Just edit that to match your installed
Python version.
Install `pipenv` using the package
manager pip

```shell
pip install pipenv
```

Now you're ready to download and install the needed packages using pipenv

```shell
pipenv install --dev
```

After that you should be able to use the executable `mkdocs` in the local virtual Python environment in your project root using `pipenv run`:

```shell
pipenv run mkdocs --version
```

#### Haskell Stack

TestHaskell uses [Stack](https://docs.haskellstack.org/en/stable/README/) as a build tool and to manage dependencies. You can install it using your distribution's package manager, [Homebrew](https://formulae.brew.sh/formula/haskell-stack) on Macs or [Chocolatey](https://community.chocolatey.org/packages/haskell-stack) on Windows. On Linux I advise you to use the [install script](https://docs.haskellstack.org/en/latest/install_and_upgrade/) with Curl

```shell
curl -sSL https://get.haskellstack.org/ | sh
```

or Wget:

```shell
wget -qO- https://get.haskellstack.org/ | sh
```

because the Stack packages are old (<2.0) and you'd need to install using `stack upgrade` anyway. The Homebrew and Chocolatey packages are up-to-date.

You do not need to install [GHC](https://www.haskell.org/ghc/), the Haskell compiler, or [Cabal](https://www.haskell.org/cabal/), Stack does that for us in the next step.

If Stack is working, which you can test with

```shell
stack --version
```

it's time to install all needed tools for the project. There are two scripts

- [./install_build_tools.bat](https://github.com/Release-Candidate/HaskellTemplate/blob/main/install_build_tools.bat) on Windows
- [./install_build_tools.sh](https://github.com/Release-Candidate/HaskellTemplate/blob/main/install_build_tools.sh) on a unixish OS like Linux or Mac OS X

that do that for us. The scripts call the following commands:

```shell
pipenv install --dev
stack build --copy-compiler-tool hlint
stack build --copy-compiler-tool hoogle
stack build --copy-compiler-tool implicit-hie
stack build --copy-compiler-tool ghcid
stack build --copy-compiler-tool haskell-dap
stack build --copy-compiler-tool ghci-dap
stack build --copy-compiler-tool haskell-debug-adapter
stack build --copy-compiler-tool ormolu
stack build --copy-compiler-tool weeder
stack exec gen-hie > hie.yaml
```

so afterwards we can use [HLint](https://hackage.haskell.org/package/hlint-1.7/src/hlint.htm), [Hoogle](https://wiki.haskell.org/Hoogle), [gen-hie](https://github.com/Avi-D-coder/implicit-hie#readme), [GHCi](https://github.com/ndmitchell/ghcid#readme), [Debug Adapter](https://github.com/phoityne/haskell-debug-adapter/), [Ormolu](https://github.com/tweag/ormolu) and [Weeder](https://github.com/ocharles/weeder).

#### Source Code and Tests

The sources of the project's library are located in the directory `src`, the executable in the directory `app` and the tests in `test`.

##### Library Sources

All sources of the library `Lib` reside in the directory `./src`.

- [./src/Lib.hs](https://github.com/Release-Candidate/TestHaskell/blob/main/src/Lib.hs) - The module `Lib` containing  some functions to calculate the Fibonacci sequence and the golden ratio.

##### Executable Sources

Sources needed to build the executable `TestHaskell-exe` are contained in the directory `./app`.

- [./app/Main.hs](https://github.com/Release-Candidate/TestHaskell/blob/main/app/Main.hs) - The main entry point of the executable, parses the command line and calls the Fibonacci and golden ratio functions.

##### Test Sources

All test cases and everything test related: directory `./test`. Uses [Hspec](https://hspec.github.io/) as testing framework and to automatically discover the tests. And [QuickCheck](http://www.cse.chalmers.se/~rjmh/QuickCheck/manual.html), [LeanCheck](https://github.com/rudymatela/leancheck#readme), [SmallCheck](https://hackage.haskell.org/package/smallcheck) and [Hedgehog](https://hedgehog.qa/).

- [./test/Spec.hs](https://github.com/Release-Candidate/TestHaskell/blob/main/test/Spec.hs) - The main entry point of the test, contains no source code, just the header to let HSpec automatically discover the other test sources.
- [./test/FibonacciSpec.hs](https://github.com/Release-Candidate/TestHaskell/blob/main/test/FibonacciSpec.hs) - Test the Fibonacci number generation.
- [./test/GoldenRatioSpec.hs](https://github.com/Release-Candidate/TestHaskell/blob/main/test/GoldenRatioSpec.hs) - Test the golden ratio calculations.

#### Build

[Stack](https://docs.haskellstack.org/en/stable/README/) is used to build the project.

```shell
stack build
```

builds the library and executable (but not the tests).

To build and run the tests use

```shell
stack test
```

To rerun the build and test when some file has been changed, use

```shell
stack test --file-watch
```

I like to also generate Haddock documentation for all dependencies and use the non-optimized compilation:

```shell
stack test --fast --haddock-deps --file-watch
```

This is also what the two scripts

- [./run_watch.bat](https://github.com/Release-Candidate/TestHaskell/blob/main/run_watch.bat)
- [./run_watch.sh](https://github.com/Release-Candidate/TestHaskell/blob/main/run_watch.sh)

do when they are called.

To get coverage data from the tests, Stack has to be called with `--coverage`, but first the project has to be recompiled, so we `clean` it.

```shell
stack clean
stack test --coverage
```

which generates `.tix` and `.mix` files needed by programs which generate alternative output formats, like LCov.

To get LCov output saved to the file `./lcov.info`, we need the executable `hpc-lcov` - one of the dependencies of the test executable and automatically installed by Stack:

```shell
stack exec -- hpc-lcov --file $TIX_PATH/TestHaskell/TestHaskell-test/TestHaskell-test.tix
```

`TIX_PATH` is the output of

```shell
stack path --local-hpc-root
```

There exists two scripts

- [./run_coverage.bat](https://github.com/Release-Candidate/HaskellTemplate/blob/main/run_coverage.bat)
- [./run_coverage.sh](https://github.com/Release-Candidate/HaskellTemplate/blob/main/run_coverage.sh)

which run all these steps to generate the LCov result file `./lcov.info`.

## CodeCov Configuration

- [./codecov.yml](https://github.com/Release-Candidate/HaskellTemplate/blob/main/codecov.yml) - CodeCov configuration file, lists all files to be ignored in coverage result calculations:

```YAML
  ignore:
    - "./test/"
```

Used in the workflows [linux_test.yml](https://github.com/Release-Candidate/TestHaskell/blob/main/.github/workflows/linux_test.yml),
[osx_test.yml](https://github.com/Release-Candidate/TestHaskell/blob/main/.github/workflows/osx_test.yml) and [windows_test.yml](https://github.com/Release-Candidate/TestHaskell/blob/main/.github/workflows/windows_test.yml) - see [GitHub Workflows](#github-workflows)

## GitHub Workflows

All tests and builds are executed on Linux, Mac OS X and Windows.

!!! Warning

    The GitHub workflow files are not included in the Stack template, because they use Moustache fields themselves and get mangled by `stack new`. To use them, you have to download them from [GitHub](https://github.com/Release-Candidate/HaskellTemplate/tree/main/.github/workflows).

These are the GitHub workflows defined in the directory `.github/workflows`

- `create_packages.yml` creates and uploads the executable an each OS (Linux, Mac OS X and Windows) and
  generates a new GitHUb release with these files appended. Runs automatically after tagging
  the source with a release tag of the form `v?.?.?`. Appends the newest entry in [CHANGELOG.md](https://github.com/Release-Candidate/HaskellTemplate/blob/main/CHANGELOG.md) to the release - script [`scripts/get_changelog.sh`](https://github.com/Release-Candidate/TestHaskell/blob/main/scripts/get_changelog.sh)
  See the [latest release](https://github.com/Release-Candidate/HaskellTemplate/releases/latest) as an example
- `linux_test.yml` runs the tests and coverage tests on Linux, uploads the test results as artifacts,
  uploads the coverage results to CodeCov.
- `osx_test.yml` runs the tests and coverage tests on Mac OS X, uploads the test results as artifacts,
  uploads the coverage results to CodeCov.
- `windows_test.yml` runs the tests and coverage tests on Windows, uploads the test results as artifacts,
  uploads the coverage results to CodeCov.

The badges of the workflows are linked in the section [Badges](https://github.com/Release-Candidate/TestHaskell#badges )

## GitHub Issue Templates

Issue templates for GitHub in `.github/ISSUE_TEMPLATE/`

- `bug_report.md` Bug report template
- `feature_request.md` Feature request template

## What is What? - List of all Files

A list of all files in this template and what they do or configure.

- `./README.md` - The file that you are reading right now.
- `./LICENSE` - The project'S license, MIT.
- `./CHANGELOG.md` - The project's changelog.

### Scripts

- `./BigTemplate.hsfiles` from the files in this repository.
- `./install_build_tools.bat` - Locally installs all needed build tools using `stack install`.
- `./install_build_tools.sh` - Locally installs all needed build tools using `stack install`.
- `./run_watch.bat` - Watches for source code changes to continuously rebuild and run tests. Calls:

```shell
stack test --fast --haddock-deps --file-watch
```

- `./run_watch.sh` - Watches for source code changes to continuously rebuild and run tests. Calls:

```shell
stack test --fast --haddock-deps --file-watch
```

- `./run_coverage.bat` - Generate a LCov coverage report `./lcov.info`, by calling

```shell
stack clean
stack test --coverage
stack exec -- hpc-lcov --file %RESULT%\TestHaskell\TestHaskell-test\TestHaskell-test.tix
```

- `./run_coverage.sh` - Generate a LCov coverage report `./lcov.info`, by calling

```shell
stack clean
stack test --coverage
stack exec -- hpc-lcov --file ${TIX_FILE}
```

- `./run_codecov_coverage.bat` - Generates a CodeCov coverage report `./coverage.json`, by calling

```shell
stack clean
stack test --coverage
stack exec -- hpc-codecov --verbose -o coverage.json %RESULT%\TestHaskell\TestHaskell-test\TestHaskell-test.tix -m %MIX_DIR%\hpc
```

- `./run_codecov_coverage.sh` - Generates a CodeCov coverage report `./coverage.json`, by calling

```shell
stack clean
stack test --coverage
stack exec -- hpc-codecov --verbose -o coverage.json ${TIX} -m ${MIX_DIR}\hpc
```

- `./run_haddock.bat` - Runs Haddock to generate the source documentation in `docs/html/`. Calls

```shell
stack clean

stack haddock --haddock-arguments="--odir=docs/html --html --built-in-themes"

```

- `./run_haddock.sh` - Runs Haddock to generate the source documentation in `docs/html/`. Calls

```shell
stack clean

stack haddock --haddock-arguments="--odir=docs/html --html --built-in-themes"

```

- `./run_hoogle.bat` - Runs Hoogle to create the local search index. Calls:

```shell
stack hoogle -- generate --local
stack hoogle -- server --local --port=8080
```

- `./run_hoogle.sh`- Runs Hoogle to create the local search index. Calls:

```shell
stack hoogle -- generate --local
stack hoogle -- server --local --port=8080
```

- `./run_code.bat` - Script to start Visual Studio Code (Insiders) with the Stack environment, so we don't need to add Stack's local `bin` directory to the user PATH. Calls

```shell
stack exec "%LOCALAPPDATA%\Programs\Microsoft VS Code Insiders\bin\code-insiders" TestHaskell.code-workspace
```

- `./run_code.sh` - Script to start Visual Studio Code (Insiders) with the Stack environment, so we don't need to add Stack's local `bin` directory to the user PATH.

```shell
stack exec code-insiders TestHaskell.code-workspace
```

- `./make_sh_executable.bat` - Windows only, recursively sets the executable bit of all shell scripts with a suffix of `.sh`.

- `./CHANGELOG.md`.

### GitHub Workflows & Issue Templates

Directory `./github/ISSUE_TEMPLATE/`:

- `./.github/ISSUE_TEMPLATE/bug_report.md` - Bug report template for GitHub
- `./.github/ISSUE_TEMPLATE/feature_request.md` - Feature request template for GitHub

Directory `./.github/workflows/`:

!!! Warning
    The GitHub workflow files are not included in the Stack template, because they use Moustache fields themselves and get mangled by `stack new`. To use them, you have to download them from [GitHub](https://github.com/Release-Candidate/HaskellTemplate/tree/main/.github/workflows).

- `.github/workflows/create_packages.yml`. This workflow runs on all 3 GitHub OSes, Linux, Mac OS X and Windows, it is started after tagging the source with a release tag of the form `v.?.?.?` or manually creating a release using GitHubs web-frontend. Build the executable(s) on all 3 OSes and appends them to the GitHub release.
- `./.github/workflows/linux_test.yml` - Run tests and coverage tests on Linux in 2 jobs, upload the coverage results to Codecov.
- `./.github/workflows/osx_test.yml` - Run tests and coverage tests on Mac OS X in 2 jobs, upload the coverage results to Codecov.
- `./.github/workflows/windows_test.yml` - Run tests and coverage tests on Windows in 2 jobs, upload the coverage results to Codecov.

### MkDocs documentation

- `./Pipfile` - Packages nedded by MkDocs to install using `pipenv` and the package `mkdocs` itself.
- `./mkdocs.yml` - The configuration file for MkDocs, contains the website's index:

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

- `https://readthedocs.org/`, to host the generated documentation.

Directory `./docs`:

- `docs/requirements.txt` - Packages (plugins for MkDocs) that have to be installed by Read the Docs to generate the documentation.
- `./docs/index.md` - The documentation's home page.
- `./docs/contributing.md` - Usage information.
- `./docs/contributing.md` - Information on how to contribute to the project.

Directory `./run_haddock.bat`. Sadly I haven't found a way to generate that documentation using `mkdocs build`, which is what Read the Docs calls to build it. So for now it is included in the source repository (but not the Stack template file).

### Haskell Source

Stack, Cabal and HPack configuration:

- `./stack.yaml` - Stack configuration file, mainly the resolver (LTS) configuration and some extra packages.
- `./stack.yaml.lock` - Stack lockfile containing package/LTS versions.
- `./package.yaml` - The main Stack/HPack configuration file.
- `./Setup.hs` - Cabal `Setup.hs` file.
- `./hie.yaml` - The Hie configuration file, generated using

```shell
stack exec gen-hie > hie.yaml
```

The Haskell source consists of three parts, the library in the directory `src`, the command line program in the directory `app` and the tests in the directory `test`.

- `./src/Lib.hs` - The library, Fibonacci and golden ratio calculations (you certainly have never seen anything similar!)
- `./app/Main.hs` - The command line program, does nothing else but parse the command line arguments and call the library functions.
- `./test/Spec.hs` - The entry point of the tests.
- `./test/FibonacciSpec.hs` - Fibonacci test cases, automatically discovered by Hspec
- `./test/GoldenRatioSpec.hs` - Golden ratio test cases, automatically discovered by Hspec.

### Visual Studio Code

- `./run_code.bat` - Script to start Visual Studio Code (Insiders) with the Stack environment, so we don't need to add Stack's local `bin` directory to the user PATH. Calls

```shell
stack exec "%LOCALAPPDATA%\Programs\Microsoft VS Code Insiders\bin\code-insiders" TestHaskell.code-workspace
```

- `./run_code.sh` - Script to start Visual Studio Code (Insiders) with the Stack environment, so we don't need to add Stack's local `bin` directory to the user PATH.

```shell
stack exec code-insiders TestHaskell.code-workspace
```

- `./TestHaskell.code-workspace` - The Visual Studio Code workspace file.
- `./.vscode/` - Directory containing additional Visual Studio Code configuration.
