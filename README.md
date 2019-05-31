# mmset-unif
Unification of multisets with multiple multiset variables

## Getting Started
These instructions will get you a copy of the project up and running on your local machine for testing purposes.

### Prerequisites
The projects runs on Standard ML of New Jersey >= v110.82.

### Installing

Clone the repo

```
git clone git@github.com:meta-logic/mmset-unif.git
```

### Notes
In this project, the *datatypes.sml* file  describes the datatypes used to represent the terms, multiset variables, and mmsets.

A term, *t* is referred to as type **form** represented as either **Atom "t"** (grounded atom), **AtomVar "t"** (atom variable), or **FormVar "t"** (formula variable). If *t = f(a1,a2,...,an)* for some function *f* and list of terms *a1,a2,...,an*, it is referred to as type **form** represented as **Form (Con "f", [*a1,a2,...,an*])**, where terms *a1,a2,...,an* each are also referred to as type **form**.

A multiset variable, *G* is referred to as type **ctx_var** represented as **CtxVar "G"**.

A mmset, *M = (t1, t2,...,tn|G1,G2,...,Gm)* is referred to as tpye **ctx** represented as **Ctx([*G1,G2,...,Gm*], [*t1, t2,...,tn*])**, where terms *t1, t2,...,tn* each are referred to as type **form** and multiset variables *G1,G2,...,Gm* each are referred to as type **ctx_var**.

### Running
The *tests.sml* file is where testing of mmset unification can be done. There are several test cases that can be run directly in there, or you can create your own. To get it running, just be in the directory where all the files with *tests.sml* exist, and run the following command:
```
sml
- use "tests.sml";
```
All tests are functions, so for example, to run test case 15 just do:
```
- test_15();
```

To create your own test case write your test like the existing test cases in the format below:
```
fun test_**TEST NAME**() = 
    let val CONTEXT_1 = Ctx(**LIST OF MULTISETS**, **LIST OF TERMS**)
        val CONTEXT_2 = Ctx(**LIST OF MULTISETS**, **LIST OF TERMS**)
        val LIST_OF_UNIFICATIONS = Unify_ctx(CONTEXT_1, CONTEXT_2)
    in  print_sigs_cons LIST_OF_UNIFICATIONS end
```
##### Interpreting Output
The output of these tests is printed on the terminal console as a list of subsitution and constraint pairs.
For example:
```
- test_15();
val it =
  [(["G1 => G1', C","G3 => G3', A"],["Gamma_120 |- G1', G2 = G3', G4"]),
   (["G1 => G1', C","G4 => G4', A"],["Gamma_121 |- G1', G2 = G3, G4'"]),
   (["G2 => G2', C","G3 => G3', A"],["Gamma_122 |- G1, G2' = G3', G4"]),
   (["G2 => G2', C","G4 => G4', A"],["Gamma_123 |- G1, G2' = G3, G4'"]),
   (["A => C"],["Gamma_124 |- G1, G2 = G3, G4"])]
  : (string list * string list) list
```
