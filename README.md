# mmset-unif
Unification of multisets with multiple multiset variables

## Getting Started
These instructions will get you a copy of the project up and running on your local machine for testing purposes.

### Prerequisites
You need to have SML on your machine.

### Installing

Clone the repo

```
git clone git@github.com:meta-logic/mmset-unif.git
```

### Notes
In this project, the *datatypes.sml* file  describes the datatypes used to represent the terms, multiset variables, and mmsets.

A term, *t* is refered to as **form** of *t* which can be represented as either **Atom "t"** (grounded atom), **AtomVar "t"** (atom variable), or **FormVar "t"** (formula variable). If *t = f(a1,a2,...,an)* for some function *f* and list of terms *a1,a2,...,an*, it is refered to as **form** of *t* represented as **Form (Con "f", [form** of *a1*, **form** of *a2*,..., **form** of *an* **])**.
A multiset variable, **G** is refered to as **ctx_var** of *G* represented as **CtxVar "G"**.
A mmset, *M = (t1, t2,...,tn|G1,G2,...,Gm)* is referred to as **ctx** of *M* represented as **Ctx([ctx_var** of *G1*, **ctx_var** of *G2*,..., **ctx_var** of *Gm* **], [form** of *t1*, **form** of *t2*,..., **form** of *tn* **])**.

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
