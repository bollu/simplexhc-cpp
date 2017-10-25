Simplexhc
---------

Simplexhc is a compiler for a lazy functional programming language
similar to [`STG (Spineless, Tagless, G-machine`](https://ghc.haskell.org/trac/ghc/wiki/Commentary/Compiler/GeneratedCode), which is an intermediate representation used by the
Glasgow Haskell compiler (hence "hc" = "haskell compiler").
`simplex` so it is vaguely related to polyhedral compilation, and because
simplicies are cool (as are simplicial complexes).

## Aims
The aim of this project is to have a fully functioning backend for GHC that
takes `STG` straight to `LLVM`.

Bonus point if I can steal ideas from polyhedral compilation. I'd like to model
the sum & product types in `STG` as spaces, perhaps modeled with integer
polyhedra, so I can reuse the machinery of [`isl`](http://isl.gforge.inria.fr/).
This doesn't really work, since `isl` needs affine maps, and I don't think
there's a reasonable interpretation of "affine" that works to analyse
parallelism for lazy languages.

Some of the ideas related to this moonshot project are
[written down in my repo: `bollu/dependence-analysis-hask`](https://github.com/bollu/dependence-analysis-hask)

# Progress
## Minimum viable
- [x] Function application (only bound parameters).
- [x] Function application (free parameters).
- [x] Constructors.
- [x] Primitive Case (over `Int#`).
- [x] Boxed Case (over regular types).
- [x] Free variables in Case
- [x] Let (non-recursive).
- [x] Letrec.
- [ ] Threads
- [ ] Garbage collection.

## Completeness
- [ ] Black holes
- [ ] Basic IO (Right now, only `printInt` exists).
- [ ] ST(?).
- [ ] SMT.
- [ ] Add `double` and `float` types.
- [ ] GHC `Core` plugin to dump out `simplexhc` style STG.




# Relationship to the other `simplechc` repo.

This is a rewrite of [`simplexhc`](https://github.com/bollu/simplexhc)
in C++, simply because I am much more comfortable doing compiler related
things in C++. 


`simplexhc` was written in Haskell. However, as time progressed, I found that
Haskell was an impediment to my joy coding (in this project, I usually love
the language). Perhaps I don't really know how to "think functional" for
mostly mutable problems: graphs, state-based code generation, etc.


I don't believe this is a shortcoming of Haskell. Rather, this is a sign
of my immaturity when it comes to Haskell design patterns.


To fix holes in my knowledge, I have been reading through the
[Futhark source code](https://github.com/diku-dk/futhark), which is an
optimising compiler written in Haskell. It is extremely well written, and I
wish I could design Haskell programs as well as they can.


Either way, as soon as I switched to C++, I felt much happier with coding this
in my hobby time. I've been procrastinating on `simplexhc` due to my fear of
returning to the codebase with spaghetti code
[such as this](https://github.com/bollu/simplexhc/blob/master/src/StgToIR.hs#L259).


Perhaps it is just me, but I find that code very unclear to read. I do not
know how to convey intent with Haskell. I would love for pointers to other
haskell compilers that are cleanly designed, so I can learn.

# References
 - [Implementing functional languages on stock hardware: the spineless, tagless, G machine](https://www.dcc.fc.up.pt/~pbv/aulas/linguagens/peytonjones92implementing.pdf)
 - [Making a fast curry, Push/Enter versus Eval/Apply](http://www.cs.tufts.edu/~nr/cs257/archive/simon-peyton-jones/eval-apply-jfp.pdf)
 - [GRIN - Whole program optimisation for lazy functional languages](http://web.archive.org/web/20080506021638/http://www.cs.chalmers.se:80/~boquist/phd/index.html)
 - [FLRC: intel haskell research compiler](https://github.com/IntelLabs/flrc)
