# Glados - Epitech Promo 2026

--------------------------------------

The goal of this project is to implement a programming language of our own design, in Haskell. It's a Basic scheme interpreter written all in Haskell.
We have a Strict testing policy, with more than 190 unit tests and close than 5 Integration tests. This program is on the standart input reading.

### Compile Glados

```
└─$ [make]
└─$ [make re]
```

--------------------------------------

### Start Glados

```
Without File:
└─$ [./glados]

With File:
└─$ [./galdos < example_test.scm]
or:
└─$ [cat example_test.scm | ./galdos]
```

--------------------------------------

### Command Lines

```
./glados [--compile file | --disassemble file | --vm file | --man | --help]
```

```--compile file```: Compile and run the GLaDOS program from the specified file.
```--disassemble file```: Disassemble the bytecode of the GLaDOS program from the specified file.
```--vm file```: Run the GLaDOS program using the virtual machine from the specified file.
```--man```: Display the manual for GLaDOS.
```--help```: Display this help message.

--------------------------------------

### Unit Tests

```
Start:
└─$ [make test]

Packages used:
  - hspec
  - silently
```

--------------------------------------

### CI/CD

This project utilizes continuous integration and continuous deployment (CI/CD) pipelines to automate the testing and deployment processes.

#### Build and Test Workflow

Upon every push to the main branch, a GitHub Actions workflow is triggered. This workflow comprises two main jobs: one for mirroring the project to the Epitech repository and another for building and testing the project.

##### 1. Mirroring to Epitech Repo

- **Trigger:** This job is triggered on every push to the main branch.
- **Steps:**
  - **Checkout Repository:** Checks out the repository with full commit history.
  - **Mirror to Epitech Repo:** Utilizes a custom GitHub Actions action to mirror the project to the specified Epitech repository using SSH key authentication.

##### 2. Building and Testing

- **Trigger:** This job is triggered on every push to the main branch.
- **Steps:**
  - **Checkout Repository:** Checks out the repository with full commit history.
  - **Update stack repository information:** Ensures the stack repository information is up-to-date.
  - **Install hlint:** Installs the `hlint` tool for checking Haskell code.
  - **Build Project:** Compiles the project using the `make` command.
  - **Run Tests:** Executes unit tests using the `make test` command.
  - **Measure code coverage:** Determines code coverage using `stack test --coverage`.
  - **Generate documentation with Haddock:** Creates project technical documentation using `make docs`.
  - **Analyze code with Haskell-Platform:** Performs code analysis using `stack build --pedantic`.
  - **Check coding style with hlint:** Verifies coding style using `hlint` and ignores errors.
  - **Create Release Artifact:** Uploads the compiled project and test results as an artifact named `release-artifact`.

### Project structure

```
├── app
│   └── Main.hs
├── CHANGELOG.md
├── glados.cabal
├── langage.bnf
├── LICENSE
├── Makefile
├── package.yaml
├── README.md
├── Setup.hs
├── src
│   ├── CommandLines.hs
│   ├── Compiler.hs
│   ├── Disassembler.hs
│   ├── Eval.hs
│   ├── File.hs
│   ├── Funcs.hs
│   ├── Glados.hs
│   ├── Parser.hs
│   ├── ParserUtils.hs
│   ├── Print.hs
│   ├── Types.hs
│   ├── Utils.hs
│   └── VM.hs
├── stack.yaml
├── stack.yaml.lock
└── test
    ├── CommandLinesSpec.hs
    ├── examples
    │   ├── test_factorial.scm
    │   ├── test_fibonacci.scm
    │   ├── test_five.scm
    │   ├── test_four.scm
    │   ├── test_one.scm
    │   ├── test_sum_of_integers.scm
    │   ├── test_three.scm
    │   └── test_two.scm
    ├── Instances.hs
    ├── IntegrationSpec.hs
    ├── Main.hs
    ├── ParserSpec.hs
    ├── ParserUtilsSpec.hs
    └── TypesSpec.hs
```

--------------------------------------

### Author

- Victor Doucet
- Hugo Mouraud
- Lucas Mechin
- Amaury Bariety
- Ashton Meuret
