# goku

[![Haskell Tests](https://github.com/woodRock/goku/actions/workflows/test.yml/badge.svg)](https://github.com/woodRock/goku/actions/workflows/test.yml)

A functional programming language with C compilation and interactive REPL support.

## Features

- **Multiple Execution Modes**: Compile to C, interpret directly, or use interactive REPL
- **Functional Programming**: First-class functions, lambda expressions, and immutable variables
- **Static Typing**: Type system with integers, booleans, and strings
- **Modern Language Features**: Pattern matching, assertions, and print statements
- **C Compilation**: Generate optimized C code for high performance
- **Interactive Development**: REPL mode for rapid prototyping and testing

## Data Types

- **Integers**: `42`, `-10`, `0`
- **Booleans**: `true`, `false`
- **Strings**: `"hello"`, `"world"`, `""` (with escape sequences)
- **Functions**: Lambda expressions `(x) -> x + 1`

## Syntax Examples

### Basic Operations
```goku
# Variables and basic types
let x = 42
let name = "Goku"
let is_ready = true

# String operations
let greeting = "Hello, " ++ name ++ "!"
print greeting

# Arithmetic
let result = (x + 10) * 2
assert result == 104
```

### Functions and Lambdas
```goku
# Lambda expressions
let add = (x, y) -> x + y
let double = (x) -> x * 2

# Function application
let sum = add(5, 3)
let doubled = double(sum)
print doubled  # Outputs: 16
```

### Control Flow
```goku
# Conditional expressions
let max = (a, b) -> if a > b then a else b

# Assertions for testing
assert max(10, 5) == 10
assert max(3, 7) == 7
```

### String Operations
```goku
# String literals and concatenation
let first = "Hello"
let second = "World"
let message = first ++ ", " ++ second ++ "!"

# String equality
assert message == "Hello, World!"
assert first == "Hello"
```

## Usage

### Installation

Requires [Haskell Stack](https://docs.haskellstack.org/en/stable/README/):

```bash
git clone https://github.com/woodRock/goku.git
cd goku
stack build
```

### Execution Modes

#### 1. Interactive REPL
```bash
stack exec goku-exe -- --interactive
```

```
goku> let x = 42
=> LitInt 42
goku> let msg = "Hello" ++ " World"
=> LitString "Hello World"
goku> :quit
```

#### 2. Interpret Files
```bash
stack exec goku-exe -- --interpret program.goku
```

#### 3. Compile to C
```bash
stack exec goku-exe -- --compile program.goku
```
Generates `program.c` and compiles to executable.

## Language Reference

### Operators

| Operator | Description | Example |
|----------|-------------|---------|
| `+` | Addition | `2 + 3` |
| `-` | Subtraction | `5 - 2` |
| `*` | Multiplication | `3 * 4` |
| `/` | Division | `8 / 2` |
| `//` | Integer Division | `7 // 2` |
| `++` | String Concatenation | `"hello" ++ " world"` |
| `==` | Equality | `x == 5` |
| `<` | Less Than | `x < 10` |

### Keywords

- `let` - Variable declaration
- `set` - Variable assignment (for functions)
- `if`/`then`/`else` - Conditional expressions
- `while`/`do` - Loop constructs
- `assert` - Assertion statements
- `print` - Output statements
- `return` - Return expressions
- `true`/`false` - Boolean literals

### Comments
```goku
# This is a line comment
let x = 42  # End-of-line comment
```

## Testing

Run the comprehensive test suite:

```bash
# Unit and integration tests
stack test

# Integration tests with shell scripts
./test/run_goku_tests.sh
```

## Project Structure

```
src/
├── Language/
│   ├── Syntax.hs         # AST definitions
│   ├── Lexer.hs          # Tokenization
│   ├── Parser.hs         # Syntax parsing
│   ├── Evaluator.hs      # Expression evaluation
│   ├── Context.hs        # Variable context
│   └── Compiler/
│       ├── CodeGen.hs    # C code generation
│       └── Optimizer.hs  # Code optimization
app/
└── Main.hs               # CLI entry point
test/
├── Spec.hs               # Unit tests
├── *.goku                # Test programs
└── run_goku_tests.sh     # Integration tests
```

## Development

The project uses:
- **Haskell Stack** for build management
- **HSpec** for unit testing
- **GitHub Actions** for CI/CD
- **GHC 9.6+** for compilation

### Contributing

1. Fork the repository
2. Create a feature branch
3. Add tests for new functionality
4. Ensure all tests pass: `stack test`
5. Submit a pull request

## License

See [LICENSE](LICENSE) file for details.