---
author: David Johnson
title: GRIN
date: 10/20/2021
---

### Graph Reduction Intermediate Notation
  - A strategy for the evaluation of lazy functional programs

---

### Example
![](https://user-images.githubusercontent.com/875324/138087456-d9392986-b2e9-4411-b195-47159ca51e3b.png)

---

### Graph Reduction
![](https://user-images.githubusercontent.com/875324/138077347-7da69a71-3611-48d2-a111-58f5255f2e3c.png)

---

### History

  - Research spans 70s-00s
	- Interpreters
	- Compiled graph reduction
	- G-machine (Thomas Johnsson)
	- STG (SPJ / Marlow) - GHC's current strategy
	- GRIN (Thomas Johnnson)
	- Reduceron (Hardware accelerated graph reduction)
	  - https://github.com/tommythorn/Reduceron

---

### Example
![](https://user-images.githubusercontent.com/875324/138076364-04b17e0c-1e17-4daf-a25d-e58baad0450e.png)

---

### Another example

```haskell
					   f = \x -> x + x
```

![](https://user-images.githubusercontent.com/875324/138079286-ada04e78-c8c5-4429-ae26-14c792aeae77.png)

---

### GHC Pipeline

![](https://user-images.githubusercontent.com/875324/138091797-b573c198-85ef-4a57-9d14-0b96df826ff0.png)

---

### GRIN Pipeline

<img width="400" src="https://user-images.githubusercontent.com/875324/138148070-afd337c6-c91a-4737-9749-95f3797a6204.png"/>

---


### GRIN vs. STG

- Evaluation of a suspended function application
  - STG: The function to call is unknown at compile time, the evaluation of a closure will result in some kind of indirect call, through a pointer. This makes optimisations harder.
  - GRIN: forbids all unknown calls and uses control flow analysis to approximate the possible targets.

---

### GRIN Language

 - First order
   - No HOFs, defunctionalized
 - Strict
   - GRIN statements are evaluated strictly
   - Lazy graph traversal is encoded implicitly via `eval`
 - State monadic
   - `update` is like `modify'`
   - `fetch` is like `get`
   - `pure` is like `return`
   - `(<-)` is like `(>>=)`

---

### GRIN Language cont.

 - SSA
   - Single static assignment (all names are unique)
 - Whole program optimizing
   - Entire program available for optimization

---

### GRIN AST
<img width="500" src="https://user-images.githubusercontent.com/875324/138086203-df054600-1799-42d6-a4fa-2194dac720e3.png"/>


---

### GRIN BNF

```haskell
grin :: { Grin }
: many1(def) { Grin $1 }

def
: var many(var) '=' do exp { Def $1 $2 $5 }

exp
: val '<-' sexp ';' exp { Bind $1 $3 $5 }
| case val of '{' many1(cpat_exp) '}' { Case $2 $5 }
| sexp ';' { Op $1 }
```

---

### GRIN BNF cont.

```haskell
sexp
: var many1(val) { App $1 $2 }
| pure val { Pure $2 }
| store val { Store $2 }
| fetch var '[' int ']' { Fetch $2 (Just $4) }
| fetch var { Fetch $2 Nothing }
| update var val { Update $2 $3 }
| do '{' exp '}' { SExp $3 }
```

---

### GRIN BNF cont.

```haskell
val
: '(' tag many(val) ')' { Node $2 $3 }
| '()' { Unit }
| lit { VLit $1 }
| var { Var $1 }


lit
: int	 { LitInt $1 }
| string { LitString $1 }
| double { LitDouble $1 }
| bool	 { LitBool $1 }
```

---

### GRIN AST

```haskell
-- | SSA, first-order, strict, monadic, graph-reduction IR.
newtype Grin = Grin [ Def ]
  deriving (Eq, Show, Data, Typeable)
```

```haskell
-- | Top-level function definition in GRIN
-- main = pure 1 ~ Def "main" [] (Op (Pure (Lit 1)))
-- All Grin programs must have a main to execute
data Def = Def VarName [VarName] Exp
  deriving (Eq, Show, Data, Typeable)
```

---

### GRIN AST

```haskell
data Exp
  = Bind LPat SExp Exp
  -- ^ Bind a value to a name, sequence computation
  -- ^ do { a <- pure 1; () <- store a; pure () }
  -- Bind (Pure (LitInt 1
  | Case Val [CasePat]
  -- ^ Case expression for forking control flow
  -- ^ case x of { 1 -> 2; }
  | Op SExp
  -- ^ Mutually-recursive on SExp
  deriving (Eq, Show, Data, Typeable, Generic)
```

---

### GRIN AST

```haskell
-- | GRIN core operations (fetch, store, update) modify the heap.
data SExp
  = App VarName [Val]
  -- f (1,2) ~ App "f" [Lit 1, Lit 2]
  | Pure Val
  -- ^ Returns value
  | Store Val
  -- ^ Stores value on the heap
  | Fetch VarName (Maybe Int)
  -- ^ Fetch value from the heap, optionally indexed
  -- fetch a[0] ~ Fetch "a" (Just 0)
  | Update VarName Val
  -- ^ Update variable in-place on the heap
  -- update a 2 ~ Update "a" (Lit (Lit 2))
  | SExp Exp
  -- ^ Mutually recursive on Exp
  deriving (Eq, Show, Data, Typeable)
```

---

### GRIN AST

```haskell
-- | Constant patterns
data CPat
  = NodePat Tag [VarName]
  -- ^ Constructor node pattern
  -- ^ \(Foo a b c) ~ CNodePat "Foo" ["a","b","c"]
  | LitPat Lit
  -- ^ Constant literal pattern
  -- ^ 1 ~ CLit 1
  | WildcardPat
  -- ^ Matches on any wildcard pattern
  deriving (Eq, Show, Data, Typeable)
```

---

### GRIN AST

```haskell
data Val
  = Node Tag [Val]
  -- ^ (Foo a b c) ~ Node "Foo" [Var "a", Var "b"]
  | Unit
  -- ^ (), empty / void
  | VLit Lit
  -- ^ Literal
  | Var VarName
  -- ^ Variable
  deriving (Eq, Show, Data, Typeable)
```

---

### GRIN AST

```haskell
-- | GRIN literals go here
data Lit
  = LitInt Int
  | LitDouble Double
  | LitString Text
  | LitBool Bool
  deriving (Eq, Show, Data, Typeable)
```

---

### Another example

```haskell
					   f = \x -> x + x
```

![](https://user-images.githubusercontent.com/875324/138079286-ada04e78-c8c5-4429-ae26-14c792aeae77.png)

---

### GRIN Translation

```haskell
main = do
  m1 <- store (CInt 5);
  m2 <- store (CInt 2);
  m3 <- store (Fsub m1 m2);
  m4 <- store (Ff m3);
  (CInt result) <- eval m4;
  pure result;

sub s1 s2 = do
  (CInt s3) <- eval s1;
  (CInt s4) <- eval s2;
  s5 <- _prim_int_sub s3 s4;
  pure (CInt s5);

f f1 = do
  (CInt f2) <- eval f1;
  f3 <- _prim_int_add f2 f2;
  pure (CInt f3);
```

---

### GRIN Translation

```haskell
eval e1 = do
  e2 <- fetch e1;
  case e2 of {
    (CInt e3) ->
      pure e2;
    (Fsub e4 e5) ->
      e6 <- sub e4 e5;
      () <- update e1 e6;
      pure e6;
    (Ff e7) ->
      e8 <- f e7;
      () <- update e1 e8;
      pure e8;
  }
```

---

### Demo

Interpret the GRIN

---

### GRIN benefits
  - No unknown control flow
	- GHC hardcodes `eval` in the RTS.
  - Inlining is the gateway to all optimizations
	- Secrets of the GHC inliner
	- GRIN inlines the `eval` function
  - Inlining eval has the effect of removing the overhead of laziness
  - Inlining eval opens up a world of additional optimizations only strict languages can use

---

### GRIN Goals
  - Remove as many `fetch`, `update`, `store` calls as possible
	- Removes allocations, improves performance
  - Inline as much of the RTS into the user's application as possible
  - Deforestation for free
  - Smaller binaries
  - Very fast code

---

### GRIN Goals

![](https://user-images.githubusercontent.com/875324/138152959-25d048a6-5ec5-41cf-9bd1-af676f14ee34.png)

---

### History

 - Created by Thomas Johnsson
 - Documented only in Boquist Thesis

---

### History

![](https://user-images.githubusercontent.com/875324/138149363-bdb2e149-3af0-4552-93d2-776781fb542a.png)

---

### Another example

```haskell
main = sum (upto 1 10)

upto m n =
  if m > n
  then []
  else m : upto (m+1) n

sum l =
  case l of
    [] -> 0
    (x:xs) -> x + sum xs

-- > main
-- 55
```

---

### Grin translation

<img width="400" src="https://user-images.githubusercontent.com/875324/138148673-b28087c5-5206-4e65-b7ab-d9fb665aab0c.png" />

---

### Static Analysis

![](https://user-images.githubusercontent.com/875324/138145652-e60b8c7e-bd8c-400c-bb14-348df23bf5d1.png)

---

### Static analysis

![](https://user-images.githubusercontent.com/875324/138149082-ce04c7f1-ee17-4218-a3bb-5e7ae897000f.png)

---

### Static analysis

  - Inlining eval should be done intelligently
    - Can't just inline the entire `eval` function everywhere
  - Precise pointer analysis
    - Computationally intractable (NP-hard problem)

---

### Static analysis

![](https://user-images.githubusercontent.com/875324/138162604-ffe46665-8f2d-485d-925f-9eea941867fe.png)

---

### Pointer analysis algorithms

  - Steensgaard O(n)
	- trades precision for speed
  - Andereson O(n^3)
	- trades speed for precision
  - Shapiro
	- configurable implementation
    - O(n) - O(n^3)

---

### Steengaard

![](https://user-images.githubusercontent.com/875324/138157393-ba11f012-55b4-4e34-aa1e-8357c1192580.png)

---

### Static analysis result

![](https://user-images.githubusercontent.com/875324/138142439-50c77fc0-9ea3-470a-9d53-1aadf9ad738f.png)

---

### Pointer analysis tools

  - [Souffle](https://souffle-lang.github.io/) datalog

---

### Type system

  - GRIN does not have a type system by default
  - Can infer types using the static analysis result

---

### Type inference via static analysis

![](https://user-images.githubusercontent.com/875324/138159719-a3e55720-64c9-48c6-ba7a-fc71ea38f0d2.png)

---

### Eval inlining example

---

# Optimizations

- Local code transformations
  - Evaluated Case Elimination
  - Trivial Case Elimination
  - Update Elimination
  - Copy Propagation
  - Constant Propagation
  - Dead Procedure Elimination
  - Dead Parameter Elimination
  - Case Copy Propagation

---

# Optimizations

- Whole program analysis based transformations
  - Sparse Case Optimisation
  - Dead Variable Elimination
  - Dead Data Elimination
  - Common Subexpression Elimination
  - Case Hoisting
  - Generalized Unboxing
  - Arity Raising

---

### Optimization example

![](https://github.com/grin-compiler/grin/raw/master/images/evaluated-case-elimination.png)

---

### Optimization example

```haskell
evaluatedCaseElimination :: Grin -> Grin
evaluatedCaseElimination =
  transformOn (defs . each . expr) $ \expr' ->
	 case expr' of
	   Case val pats
		 | all (casesReturnSameValue val) pats -> Op (Pure val)
		 | otherwise -> expr'
	   _ -> expr'
	 where
	   casesReturnSameValue
		 :: Val
		 -> CasePat
		 -> Bool
	   casesReturnSameValue val (_, ex) =
		 ex == Op (Pure val)
```

---

### Optimized GRIN

```haskell
  main = do
    sum 0 1 1000;

  sum n29 n30 n31 = do
    b2 <- _prim_int_gt n30 n31;
    case b2 of {
      True -> pure n29;
      False ->
        n18 <- _prim_int_add n30 1;
        n28 <- _prim_int_add n29 n30;
        sum n28 n18 n31;
    }
```

---

### Who uses GRIN?
 - [LHC (Luxurious Haskell Compiler)](https://github.com/Lemmih/lhc)
 - [JHC ("John Meachem" Haskell Compiler)](http://repetae.net/computer/jhc/)
 - [GRIN compiler project](https://github.com/grin-compiler/grin)

---

### Why study GRIN?
  - Understand how functional programs are evaluated
  - Exposure to many optimizations that are similar at the GHC core level
  - Exposure to many packages that GHC itself uses (`alex`, `happy`)

---

### Further study

[A highly optimizing backend for lazy functional languages](http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.27.3918&rep=rep1&type=pdf)

---

### Future work

 - Garbage Collection
 - Exceptions
 - HOFs
 - LLVM Codegen
 - Lambda calculus translation
 - Additional optimizations

---

### Thanks!
![](https://upload.wikimedia.org/wikipedia/commons/b/bc/Face-grin.svg)

