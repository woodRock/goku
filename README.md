# goku

[![Haskell Tests](https://github.com/woodRock/goku/actions/workflows/test.yml/badge.svg)](https://github.com/woodRock/goku/actions/workflows/test.yml)

A functional programming language with C compilation and interactive REPL support.

## Features

- **Multiple Execution Modes**: Compile to C, interpret directly, or use interactive REPL
- **Functional Programming**: First-class functions, lambda expressions, and immutable variables
- **Strong Type System**: Type inference with integers, booleans, strings, and lists
- **Advanced Data Types**: Lists with cons operator and comprehensive built-in functions (head, tail, empty, length, append, nth, reverse, elem)
- **Type-Aware C Generation**: Generates optimized C code with proper type handling
- **Modern Language Features**: Pattern matching, assertions, and print statements
- **String Operations**: Full string support with concatenation and type-safe equality
- **Interactive Development**: REPL mode for rapid prototyping and testing
- **Comprehensive Testing**: 46+ integration tests covering all language features

## Data Types

- **Integers**: `42`, `-10`, `0`
- **Booleans**: `true`, `false`
- **Strings**: `"hello"`, `"world"`, `""` (with escape sequences)
- **Lists**: `[1, 2, 3]`, `[]` (homogeneous collections with cons operator `::`)
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
let remainder = x % 3
assert result == 104
assert remainder == 0
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

### List Operations
```goku
# List creation
let numbers = [1, 2, 3, 4, 5]
let empty_list = []

# Cons operator (prepend)
let extended = 0 :: numbers  # [0, 1, 2, 3, 4, 5]

# Basic list functions
let first = head(numbers)     # 1
let rest = tail(numbers)      # [2, 3, 4, 5]
let is_empty = empty(numbers) # false
let size = length(numbers)    # 5

# New list operations
let appended = append(numbers, 6)    # [1, 2, 3, 4, 5, 6]
let concatenated = numbers ++ [6, 7] # [1, 2, 3, 4, 5, 6, 7]
let third = nth(numbers, 2)          # 3 (0-based indexing)
let reversed = reverse(numbers)      # [5, 4, 3, 2, 1]
let contains = elem(3, numbers)      # true

# Chaining operations
let second = head(tail(numbers))     # 2

# Building lists with cons
let built = 1 :: 2 :: 3 :: []        # [1, 2, 3]
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

### Mathematical Operations
```goku
# All arithmetic operators
let x = 17
let y = 5

let sum = x + y        # 22
let diff = x - y       # 12
let product = x * y    # 85
let quotient = x / y   # 3 (integer division)
let remainder = x % y  # 2 (modulo)

# Modulo is useful for checking divisibility
assert (10 % 2) == 0   # 10 is even
assert (15 % 4) == 3   # remainder when 15 divided by 4
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
goku> 10 % 3
=> LitInt 1
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
Generates type-aware C code with proper string handling, memory management, and optimized arithmetic operations. The compiler automatically:
- Uses `strcmp()` for string comparisons
- Generates appropriate C types (`char*`, `int`, `bool`)
- Handles string concatenation with proper memory allocation
- Includes error checking for division by zero and modulo by zero

## Language Reference

### Operators

| Operator | Description | Example |
|----------|-------------|---------|
| `+` | Addition | `2 + 3` |
| `-` | Subtraction | `5 - 2` |
| `*` | Multiplication | `3 * 4` |
| `/` | Division | `8 / 2` |
| `//` | Integer Division | `7 // 2` |
| `%` | Modulo | `10 % 3` |
| `++` | String Concatenation | `"hello" ++ " world"` |
| `::` | List Cons (prepend) | `1 :: [2, 3]` |
| `==` | Equality | `x == 5` |
| `<` | Less Than | `x < 10` |

### Built-in Functions

| Function | Description | Example |
|----------|-------------|---------|
| `head(list)` | Get first element | `head([1, 2, 3])` |
| `tail(list)` | Get all but first element | `tail([1, 2, 3])` |
| `empty(list)` | Check if list is empty | `empty([])` |
| `length(list)` | Get list length | `length([1, 2, 3])` |
| `append(list, elem)` | Add element to end | `append([1, 2], 3)` |
| `nth(list, index)` | Get element at index | `nth([1, 2, 3], 1)` |
| `reverse(list)` | Reverse list order | `reverse([1, 2, 3])` |
| `elem(element, list)` | Check membership | `elem(2, [1, 2, 3])` |
| `print(expr)` | Print expression | `print("hello")` |
| `assert(expr)` | Assertion testing | `assert(2 + 2 == 4)` |

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
# Haskell unit tests
stack test

# Integration tests (46+ organized test cases)
./test/run_goku_tests.sh

# Test specific features
stack exec goku-exe -- --interpret test/arithmetic_modulo_basic.goku
stack exec goku-exe -- --compile test/string_concatenation_complex.goku
```

The test suite includes organized categories:
- `arithmetic_*` - Mathematical operations and precedence
- `assert_*` - Assertion testing in various contexts  
- `basic_*` - Fundamental language features
- `control_*` - Control flow structures
- `function_*` - Function definition, calls, and recursion
- `print_*` - Print statement functionality
- `string_*` - String operations and equality

## Project Structure

```
src/
├── Language/
│   ├── Syntax.hs         # AST definitions
│   ├── Lexer.hs          # Tokenization
│   ├── Parser.hs         # Syntax parsing
│   ├── Evaluator.hs      # Expression evaluation
│   ├── Context.hs        # Variable context
│   ├── Types.hs          # Type inference system
│   └── Compiler/
│       ├── CodeGen.hs    # Type-aware C code generation
│       └── Optimizer.hs  # Code optimization
app/
└── Main.hs               # CLI entry point
test/
├── Spec.hs               # Unit tests
├── *.goku                # 46+ organized test programs
└── run_goku_tests.sh     # Integration test suite
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