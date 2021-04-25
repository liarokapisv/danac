# Danac

Danac is a compiler for the [Dana](https://courses.softlab.ntua.gr/compilers/2017a/dana2017.pdf) programming language created for the National Technical University of Athens' School of Electrical and Computer Engineering 2016-2017 compilers course. Dana is a minimal statically typed language only supporting primitive types. Main interesting features include optional identation-based syntax, nested functions with lexical scope and named loops.

## Building, installation and usage

The project requires either [stack](https://docs.haskellstack.org/en/stable/README/) or [cabal](https://www.haskell.org/cabal/).
It also requires LLVM 9 to be available.

In order to build invoke `stack build` on the root directory.

For user-local installation invoke `stack install`

`danac` supports visualizing the annotated ASTs of each stage as well as the final `llvm-hs-pure` AST and the generated llvm assembly.

Run `danac -h` for supported options and usage instructions. 

## Examples

A set of valid example programs can be found in the `examples` folder.

A set of erroneous programs can be found in the `error-examples` folder and can be used to inspect formatting of common errors.

## Architecture

### Ast representation and processing

Main goal of the compiler project was to research compiler-writing techniques that help enhance the implementation process. Main interesting points are: 

-	How to handle information introduced via each compiler pass and how to make it available to consumers.
-	How to reduce boilerplate and make each pass more specialized and expressive.


Mainstream approaches to storing produced information from each pass is through lookup tables.
Inherently this means that there is no way to ensure that a node's information is stored within a 
lookup table as the lookup table inherently requires a lookup process. A better approach is to directly annotate the nodes with produced info. Then subsequent passes can just directly access the info associated with each node. Inherently we need to support node-specific annotations that can change between passes. Therefore our end-goal is an AST encoding that supports extensible annotations. We want this to be a type-safe interface, ideally each pass' AST is a unique type.
There are a lot of existing approaches that attempt to solve this problem including [Data types a la Carte](http://www.cs.ru.nl/~W.Swierstra/Publications/DataTypesALaCarte.pdf), [Trees that Grow](https://www.microsoft.com/en-us/research/uploads/prod/2016/11/trees-that-grow.pdf), extensible tuples and more.

When working a lot with Tree-based representations it's incredibly useful to be able to avoid resorting to low-level explicit recursion and instead leverage existing processing schemes. One good example of this is the renaming pass. Only a handful of nodes actually require any interesting and non-trivial recursion. Most other nodes just recurse the renaming algorithm on their sub-nodes. We would like to be able to avoid specifying all these non-interesting cases. If our AST was trivial this would be directly solvable by using open-recursion to define our AST tree.
Let's think of a a simple expression type:

```haskell
data Expr = Lit Int | Add Expr Expr | Let String Expr | ...
```

We define it's base Functor:
```haskell
data ExprF r = Lit Int | Add r r | Let String r | ...
	deriving Functor, Foldable, Traversable
```


Then we can use the `cata` [recursion-scheme](https://hackage.haskell.org/package/recursion-schemes) along with `Traversable` 's `sequence` in order
to define a catch-all case for uninteresting nodes:

```haskell
algo = cata algo'
	where algo' (Let x y) = ... -- example interesting case
    	      algo' x = sequence x -- all uninteresting cases
```

Notice how well the `cata` algebra abstraction cooperates with the `sequence` function. 
Splitting the recursion into an algebra allows us to expose the recursively evaluated sub-nodes  and
the `Traversable` instance allows us to generically lift the partially-evaluated node to an effectful computation.

Finally, we can use the [`Fix`/`Mu`](https://en.wikibooks.org/wiki/Haskell/Fix_and_recursion) combinators in order to actually express our Tree data structure.

```haskell
type Tree = Fix TreeF
```

Annotating is now a simple case of composing the Base Functor with an Annotation Functor:

```haskell
type AnnotatedTree = Fix (TreeF :*: Annotation)
```

where `(:*:)` is the Product combinator. 

This way we can use a phase-specific annotation data type and achieve our previous goal.


Unfortunately in general our ASTs consist of multiple mutual-recursive types so we can't use the usual recursion schemes and Fix pattern directly. We need to somehow first express our mutual recursion AST and then find a way to apply our usual recursion schemes to it. [Compositional Data Types](http://dx.doi.org/10.1145/2036918.2036930) uses a very nice approach to actually expressing the base functor although in the paper it's used as a tool for the auto-generation machinery and is actually generated automatically. In our case we are only interested in the simple human-readable version. The trick is to define all the mutual data types as a single GADT which each node family being represented by a separate tag.

```haskell
data AST t where
    NodeA1 :: X -> AST NodeA
    NodeA2 :: X -> AST NodeY -> AST NodeA

    NodeB1 :: X -> AST NodeB
    NodeB2 :: AST NodeA -> AST NodeB
    
    ...
```

Then we can also define our open-recursion base "Functor"

```haskell
data ASTF r t where
    NodeA1 :: X -> AST r NodeA
    NodeA2 :: X -> r NodeY -> AST r NodeA

    NodeB1 :: X -> AST r NodeB
    NodeB2 :: r NodeA -> AST r NodeB
    
    ...
```

Here `ASTF` is not actually a Functor but it's similar in that instead of mapping morphisms it maps natural transformations. We will refer to objects satisfying this property as Higher-Order Functors. This abstraction is not new and is in fact documented in the Compositional Data Types paper but also [elsewhere](https://bartoszmilewski.com/2018/08/20/recursion-schemes-for-higher-algebras/).

compare
```haskell
class Functor f where
	fmap :: (a -> b) -> (f a -> f b)
```


to
```haskell
type a :~> b = forall t. a t -> b t

class HFunctor f where
	hfmap :: (a :~> b) -> (f a :~> f b)
```

Similarly we can define Higher order Foldable and Traversables as well as all usual recursion-schemes. 

Our final AST then consists of the GADT, the family of tags used to represent node families in the GADT and the Annotation data types that are also GADT's indexed by the tag types allowing each node family to have it's own separate annotations 

Main problem is then deriving the instances as well as the higher-order recursion schemes and combinator utilities such as `Fix` and `Product`. Thankfully [compdata](https://hackage.haskell.org/package/compdata-0.12.1) has done most of the work for us. We had to patch some parts of the auto-derivation machinery as well as define more convenient annotation types but for the most part it was a simple case of just deriving the higher-order type-classes and using the provided higher-order recursion schemes.

### Parsing and Source annotations

For parsing we use [megaparsec](https://hackage.haskell.org/package/megaparsec-9.0.1) which is a modern parser-combinator framework with focus on good user-customizable error messages. So far it has been extremely valuable in allowing rapid prototyping and implementation of Dana's interesting parsing rules. 

In order to support good error messages on subsequent stages we annotate each node with it's relevant source spans which are first inserted into the produced structured error types of each module and then used in the final pretty-printing stage in order to highlight the relevant parts of the original source code. Megaparsec trivially supports this usecase. We use [errata](https://hackage.haskell.org/package/errata-0.2.0.0) for the final pretty printing of the error messages.

### Code generation and targeting LLVM

For obvious reasons we target llvm as our IR. [llvm-hs-pure](https://hackage.haskell.org/package/llvm-hs-pure) has been invaluable for this, its [IRBuilder](https://hackage.haskell.org/package/llvm-hs-pure-9.0.0/docs/LLVM-IRBuilder.html) and it's `MonadFix` implementation has been especially useful for producing the instructions in a concise way. For cross-compatibility reasons our runtime is implemented in C and the compiler invokes `clang` at the final step in order to compile and link the runtime with the generated llvm-assembly. 

Other than tooling the biggest hurdle in implementing the code generation has been supporting the nested functions and it's activation record chaining. In order to minimize the cognitive load we attempted to provide as much required info as possible from the previous compilation passes. This reduces required state booking in the code generation stage and makes the implementation more straightforward. 

Most notable examples of this are:
- The number of links and offsets required to access each identifier from the parent activation records. We actually produce this info in the renaming stage since it contains most relevant data structures.
- The type of each intermediate expression type. This is needed in order to differentiate between the various arithmetic operation types since llvm requires explicit type annotations for them. We produce this info during type checking.
- The origin of each identifier. Eg parameter vs global object vs stack-allocated. This is needed in order to properly use `getelementptr`. This is produced in the renamer stage.

In general we try to produce each required info in the stage where it's most convenient to do so either due to required data structures or due to relevant node processing.

### Type checking and evaluation/annotation combinator

Our type-checker consists of a simple higher-order algebra and is a purely bottom-up algorithm that outputs the composition of [`Validation`](https://hackage.haskell.org/package/validation-1.1.1) used for applicative-like merging of errors and an `Evaluation` indexed GADT that contains data only used for the typechecking algorithm. This includes any Source spans that are needed for error reporting.

Due to the bottom-up nature of the algorithm it would be trivial to add type-inference functionality. We don't due to the language's specifications.

Most interesting thing about the implementation is that the `Evaluation` data type produced from the pass consists of only the relevant data used in the typechecking algorith. It does not include any irrelevant data produced by previous passes.

In order to achieve this we defined a custom recursion-scheme-like combinator that takes:
- a recursion-scheme-compatible applicative-effectful algebra
- a function that merges an Indexed Annotation with an Indexed Evaluation type and produces a new Indexed Annotation 

This combinator then generates a new recursion-scheme-compatible algebra that annotates each node with the merged result of the previous tree and the new pass' evaluation data type.

This is used in practice to recursively merge the renamer-pass annotation with the typechecking-specific evaluation data type. This way we completely separate the type-checking algorithm from any previously generated info that is only relevant to subsequent stages.
