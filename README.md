# Lambda Expressions

## Description

Lambda expression interpreter in `Haskell` based on the fundamental concepts of lambda calculus: **β-reduction, name collision resolution, and strategies for reducing expressions** to their normal forms. The interpreter evaluates lambda expressions, parses expressions from strings, and allows the use of macros for simpler expression writing.

## Evaluation

- **Free Variable Identification (`free_vars`)**: Finds variables in an expression that are not bound by a function.
- **Redex Reduction (`reduce`)**: Applies a **β-reduction** to a given redex, handling potential name collisions to maintain the integrity of variable scoping.

### Reducing Expressions

Reduction involves applying **β-reduction** to redexes while considering name collisions to maintain variable scoping. The `reduce` function takes a redex in deconstructed form and returns the resulting expression after the reduction applied, there are 2 types: **Normal/Applicative**.

#### Normal Strategy

**Selection of Redex:** `Normal` strategy prioritizes the **outermost, leftmost redex** for reduction. This means that it looks for the most superficial redex in the expression, starting from the outermost layers and moving inward.

**Evaluation Order:** reducting the outermost redexes first, until no further reductions can be made. All possible reductions are explored at each step before moving on.

**Advantages:** `Normal` strategy ensures that the **entire expression is explored in a systematic manner**, potentially leading to a `more exhaustive reduction process`.

#### Applicative Strategy

**Selection of Redex:** `Applicative` strategy focuses on the **innermost, leftmost redex** for reduction. It looks for redexes that are nested deeply within the expression and starts reducing from there.

**Evaluation Order:** reducing the innermost redexes first, before moving on to outer ones. Fewer steps overall, as it prioritizes the immediate application of functions to their arguments.

**Advantages:** `Applicative` strategy can lead to more efficient reduction sequences, as it **evaluates arguments before applying functions**, potentially leading to a `avoiding unnecessary reductions`.

## Parse

- **Expression Parsing**: Transforms a string representation of a lambda expression into its structured form as an `Expr` type which can be a `Variable`, `Function`, `Application`,`Macro`.
- **Code Parsing**: Interprets lines of code that define or evaluate expressions, supporting the `Evaluate` and `Assign` constructors, which correspond to expression evaluation and macro definition, respectively.

## Macros

The use of macros is introduced to simplify writing expressions:

- Macros are named expressions that can be used to replace repetitive parts of lambda expressions.
- The interpreter's context maintains a dictionary mapping macro names to their corresponding expressions.
- The `evalMacros` function evaluates an expression with macros, performing textual substitution for each macro based on the current context.

## Code Evaluation

`Code evaluation` refers to the process of interpreting and executing lines of code within a programming language or environment. In the context of this project, we are extending our lambda calculus interpreter to support code evaluation, including the definition and execution of macros.

New abstract data type (`ADT`) called `Code` to represent lines of code. It consists of two possible constructors:

- `Evaluate Expr`: Represents an expression to be evaluated and its result printed.
- `Assign String Expr`: Defines a macro by assigning a name to an expression.

**Example:**

```haskell
-- Input Macros
true = λx.λy.x
false = λx.λy.y
and = λx.λy.(x y x)
-- Example Usage:
-- Must use the symbol `$`
$and $true $false
-- Output Code
λx.λy.y
```

`Code evaluation` let users to define macros and execute code lines, it enhances: usability and flexibility of the system, making it easier to write and evaluate lambda calculus expressions.

## Usage

Ensure Dependencies: Make sure you have `ghc`, `ghci`, `runhaskell` installed.

```bash
bash check.sh
```

**Testing with provided scripts:**

- To see the score: `./check.sh`
- For detailed results: `runhaskell test.hs`

**Testing with GHCi:**

- Launch GHCi: Open your terminal and type `ghci`
- Load All Modules: Inside GHCi, use `:l all.hs` to load all modules
- Test Your Code: Now you can interactively test the code
