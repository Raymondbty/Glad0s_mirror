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
Welcome to Glados !

Commands:
  !quit - Quit the program.
  !man  - Display the manual.
  !help - Display this help message.
```

--------------------------------------

### Manual Glados

```
Welcome to the manual !

---------------------------------------------
Add:
└─$ example -> (+ 1 2)
---------------------------------------------
Sub:
└─$ example -> (- 2 1)
---------------------------------------------
Mul:
└─$ example -> (* 2 2)
---------------------------------------------
Div:
└─$ example -> (/ 6 2)
└─$ example -> (div 6 2)
---------------------------------------------
Eq:
└─$ example -> (eq? "test" "test")
---------------------------------------------
Cond:
└─$ example -> (if (eq? 5 4) (+ 6 6) (- 7 1))
---------------------------------------------
Def:
└─$ example -> (define test 5)
└─$ example -> (define \"test\" (+ 5 6))
---------------------------------------------
```

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
  - **Generate documentation with Haddock:** Creates project documentation using `make docs`.
  - **Analyze code with Haskell-Platform:** Performs code analysis using `stack build --pedantic`.
  - **Check coding style with hlint:** Verifies coding style using `hlint` and ignores errors.
  - **Create Release Artifact:** Uploads the compiled project and test results as an artifact named `release-artifact`.

### Project Architecture

```
├── app
│   └── Main.hs
├── CHANGELOG.md
├── examples
│   ├── call.scm
│   ├── error.scm
│   └── foo.scm
├── glados
├── glados.cabal
├── langage.bnf
├── LICENSE
├── Makefile
├── package.yaml
├── README.md
├── Setup.hs
├── src
│   ├── Ast.hs
│   ├── CommandLines.hs
│   ├── Eval.hs
│   ├── Funcs.hs
│   ├── Glados.hs
│   ├── Parser.hs
│   └── Print.hs
├── stack.yaml
├── stack.yaml.lock
└── test
    ├── AstSpec.hs
    ├── CommandLinesSpec.hs
    ├── EvalSpec.hs
    ├── FuncsSpec.hs
    ├── GladosSpec.hs
    ├── Instances.hs
    ├── Main.hs
    └── ParserSpec.hs
```

--------------------------------------

### Author

- Victor Doucet
- Hugo Mouraud
- Lucas Mechin
- Amaury Bariety
- Ashton Meuret
