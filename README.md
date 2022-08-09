# Mars Rover

## Building the project

### Prerequisites

1. `curl`
2. [`direnv`](https://direnv.net/)

Both should be pre-installed on your machine.

### Installing the compiler and build tools

1. Type `direnv allow .` in your terminal.
   The `.envrc` file checks if the tooling needed to build the project is
   already installed on your machine and it installs it if it is not.
2. Wait for [Stack](https://docs.haskellstack.org/en/stable) to be downloaded.
   Stack is a building tool to build and test Haskell projects.
3. Type `stack build` in your terminal to build the project.
   Please be patient the first time you run it. It might take a while for it
   to download the compiler and dependencies and build them.

It should work on Mac OS X Catalina and Ubuntu. It might work on Windows.

### Uninstalling

See the page [How to uninstall](https://docs.haskellstack.org/en/stable/README/#how-to-uninstall)
page on the Stack website.

## Running the program

1. Type `stack install` in your terminal to build and install the binary.
   Make sure the folder in which Stack installs the binary is in your
   `PATH`. Stack should have printed a message inviting you to add it if
   that wasn't the case.
2. Type `mars-rover` to start the program.

## Running the tests

Type `stack test` in your terminal to run all the tests.

## Continuous integration

A Github Action is setup using [Haskell CI](https://github.com/haskell-CI/haskell-ci)
to automate building and testing the project.

## Project structure

```
.
├── README.md
├── app
│   └── Main.hs # main module
├── mars-rover.cabal # generated dependecy file from package.yaml
├── package.yaml # dependecies file in yaml
├── src
│   ├── Parser.hs # input parser
│   └── Robot.hs # business logic
├── stack.yaml # build file
├── stack.yaml.lock # build file lock
└── test
    └── Main.hs # test module
    ├── ParserTest.hs # input parser tests
    └── RobotTest.hs # business logic tests
```

## Assumptions

The problem description leaves some details a little bit vague and I had
to make some assumptions in order to implement a solution.

> The world should be modelled as a grid with size m x n

There's no mention about the maximum size. The current implementation uses `Int`s
which means that the maximum value is:

```haskell
ƛ: maxBound :: Int
9223372036854775807
```

The code checks for overflow and in case it rejects the input. I could have used
`Integer` (unbounded `Int`s) to allow for a bigger grid.

> Your program should read the input, update the robots, and print out
  the final states of the robots

It's not clear to me how many robots the program should expect. From the examples
I assumed the program would work like this:

1. enter grid size
2. enter initial position and movements for robot 1
3. enter initial position and movements for robot 2
4. calculate final position movements for robot 1 and display it
5. calculate final position movements for robot 2 and display it

but a more realistic implementation might be:

1. enter grid size
2. enter initial position and movements for robot 1
3. calculate final position movements for robot 1 and display it
4. enter initial position and movements for robot 2
5. calculate final position movements for robot 2 and display it

Changing this part of the implementation is trivial.

## Future work

* Clarify requirements (see provious point)
* Make robot generic to support different capabilities e.g. move more than 1 step,
  or allow to express diagonal movements in one instruction etc.
* Add more unit tests esp. of the business logic
* Build and run code in a dockerised environment
* Support larger grids
* Expose an API
* Handle some parsing errors more gracefully e.g. `"not enough input"` error
