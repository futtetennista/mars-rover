# Mars Rover

## Building the project

### Installing the compiler and build tools

Type `direnv allow .` in your terminal.

This will to install the [Stack](https://docs.haskellstack.org/en/stable,
build tool for Haskell. You shouldn't need to do anything else.

Type `stack build` in your terminal to build the project.

Please be patient the first time you run it. It might take a while for the
compilation to be over. After that you should be ready to go.

## Running the code

1. Type `stack install` in your terminal to build and install the binary.
2. Type `mars-rover` to start the program.

## Running the tests

Type `stack test` in your terminal to build and install the binary.

## Project structure

```
.
├── README.md
├── app
│   └── Main.hs # main executable
├── mars-rover.cabal # generated dependecy file from package.yaml
├── package.yaml # dependecies file in yaml
├── src
│   ├── Parser.hs # input parser
│   └── RobotV1.hs # business logic
├── stack.yaml # build file
├── stack.yaml.lock # build file lock
└── test
    └── Main.hs # test executable
    ├── ParserTest.hs # input parser tests
    └── RobotV1Test.hs # business logic tests
```

## Implementation assumptions

The problem description leaves some details a little bit vague and assumptions
had to made in order in the implementation.

> The world should be modelled as a grid with size m x n

There's no mention about the maximum size. The current implementation uses `Int`s
which means that the maximum value is:

```haskell
ƛ: maxBound :: Int
9223372036854775807
```

The code checks for overflow and in case it rejects the input. I could have used
`Integer` (unbounded `Int`s) and I choose to be conservative since the maximum
bound for `Int`s is already quite high.

> Your program should read the input, update the robots, and print out
  the final states of the robots

It's not clear to me how many robots the program should expect. From the examples
I assumed the program would work like this:

1. input grid size
2. input initial position and movements for robot 1
3. input initial position and movements for robot 2
4. calculate final position movements for robot 1 and display it
5. calculate final position movements for robot 2 and display it

I can imagine that the implementation should accept a grid and then as many robots
until the program is terminated. It was unclear and I simply followed the examples.
Modifying the implementation is trivial.

## Future work

* Clarify requirements (see provious point)
* Make robot generic to support different capabilities e.g. move more than 1 step,
  or allow to express diagonal movements in one move etc.
* Add more unit tests
* Build and run code in a dockerised environment
* Create a CI / CD pipeline to build and run tests
* Support more than 2 robots
* Support larger grids
* Expose an API
* Handle some parsing errors more gracefully e.g. `"not enough input"` error

## Bonus

There's a `generic-robot` branch in which I wrote an alternative implementation that
generalises the robot. It's not in `master` because it was written outside the
target time suggested by the problem description.
