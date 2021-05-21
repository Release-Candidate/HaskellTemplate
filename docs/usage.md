# Installation & Usage

## Installation

You can either use this GitHub template repository as a GitHub template for a Haskell project and manually change all occurrences of `TestHaskell`, `Release-Candidate` and my name. Or use `BigTemplate.hsfiles` to generate a local project from this repository, using `stack new`:

```shell
stack new PROJECT_NAME https://raw.githubusercontent.com/Release-Candidate/HaskellTemplate/main/BigTemplate.hsfiles
```

Information on how to install Stack you find [below](#haskell-stack).

!!! Warning
    The GitHub Workflow templates
    - [.github/workflows/create_packages.yml](./.github/workflows/create_packages.yml)
    - [.github/workflows/linux_test.yml](./.github/workflows/linux_test.yml)
    - [.github/workflows/osx_test.yml](./.github/workflows/osx_test.yml)
    - [.github/workflows/windows_test.yml](./.github/workflows/windows_test.yml)
    are not included in the Stack template, because they contain Mustache fields themselves and would get mangled by using `stack new`. So please download the directory [.github/workflows/](./.github/workflows/) manually.

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

### Download

[This is for the project template, not the Stack template]
Download the latest release at GitHub: [Release at GitHub](https://github.com/Release-Candidate/TestHaskell/releases/latest)
