use "unification.sml";

(* Example Test Cases Below *)

fun test_1() = 
    let val x = Ctx([CtxVar("D1")], [FormVar "T"]) 
        val y = Ctx([], [])
        val b = Unify_ctx(x, y)
    in  print_sigs_cons b end

fun test_2() = 
    let val x = Ctx([CtxVar("D1")], []) 
        val y = Ctx([CtxVar("D3")], [])
        val b = Unify_ctx(x, y)
    in  print_sigs_cons b end

fun test_3() = 
    let val x = Ctx([], [FormVar "X", Form (Con "v", [FormVar "X", FormVar "Y"])]) 
        val y = Ctx([], [Form (Con "v", [FormVar "Z", FormVar "Y"]), AtomVar "t"])
        val b = Unify_ctx(x, y)
    in  print_sigs_cons b end

fun test_4() = 
    let val x = Ctx([CtxVar("D1")], [FormVar "X"]) 
        val y = Ctx([CtxVar("D3")], [])
        val b = Unify_ctx(x, y)
    in  print_sigs_cons b end

fun test_5() = 
    let val x = Ctx([], [FormVar "X"]) 
        val y = Ctx([CtxVar("D3")], [])
        val b = Unify_ctx(x, y)
    in  print_sigs_cons b end

fun test_6() = 
    let val w = Ctx([CtxVar "G1", CtxVar "G2"], [])
        val z = Ctx([CtxVar "G3", CtxVar "G4", CtxVar "G5"], [])
        val b = Unify_ctx(w, z)
    in  print_sigs_cons b end

fun test_7() = 
    let val x = Ctx([], [AtomVar "t", FormVar "X"]) 
        val y = Ctx([CtxVar("D3")], [FormVar "Q"])
        val b = Unify_ctx(x, y)
    in  print_sigs_cons b end

fun test_8() = 
    let val w = Ctx([CtxVar "G1"], [FormVar "X", AtomVar "t"])
        val z = Ctx([CtxVar "D2"], [AtomVar "f"])
        val b = Unify_ctx(w, z)
    in  print_sigs_cons b end

fun test_9() = 
    let val w = Ctx([], [])
        val z = Ctx([], [])
        val b = Unify_ctx(w, z)
    in  print_sigs_cons b end

fun test_10() = 
    let val w = Ctx([CtxVar "G1", CtxVar "G2"], [Form (Con "v", [FormVar "A", FormVar "B"])])
        val z = Ctx([CtxVar "G3", CtxVar "G4"], [Form (Con "v", [FormVar "C", FormVar "D"])])
        val b = Unify_ctx(w, z)
    in  print_sigs_cons b end

fun test_11() = 
    let val w = Ctx([CtxVar "G1", CtxVar "G2"], [Form (Con "<", [FormVar "A", FormVar "B"])])
        val z = Ctx([CtxVar "G3", CtxVar "G4"], [Form (Con "v", [FormVar "C", FormVar "D"])])
        val b = Unify_ctx(w, z)
    in  print_sigs_cons b end

fun test_12() = 
    let val w = Ctx([CtxVar "G1", CtxVar "G2"], [Form (Con "v", [FormVar "A", FormVar "B"])])
        val z = Ctx([CtxVar "G3", CtxVar "G4"], [Form (Con "v", [FormVar "A", FormVar "B"])])
        val b = Unify_ctx(w, z)
    in  print_sigs_cons b end

fun test_13() = 
    let val w = Ctx([CtxVar "G1"], [])
        val z = Ctx([CtxVar "G3"], [FormVar "C"])
        val b = Unify_ctx(w, z)
    in  print_sigs_cons b end

fun test_14() = 
    let val w = Ctx([CtxVar "G1"], [FormVar "A"])
        val z = Ctx([], [FormVar "C"])
        val b = Unify_ctx(w, z)
    in  print_sigs_cons b end

fun test_15() = 
    let val w = Ctx([CtxVar "G1", CtxVar "G2"], [FormVar "A"])
        val z = Ctx([CtxVar "G3", CtxVar "G4"], [FormVar "C"])
        val b = Unify_ctx(w, z)
    in  print_sigs_cons b end

fun test_16() = 
    let val w = Ctx([], [Form (Con "v", [FormVar "A", FormVar "B"])])
        val z = Ctx([], [Form (Con "v", [FormVar "C", FormVar "D"])])
        val b = Unify_ctx(w, z)
    in  print_sigs_cons b end

fun test_17() = 
    let val w = Ctx([], [Form (Con "<", [FormVar "A", FormVar "B"])])
        val z = Ctx([], [Form (Con ">", [FormVar "C", FormVar "D"])])
        val b = Unify_ctx(w, z)
    in  print_sigs_cons b end

fun test_18() = 
    let val w = Ctx([CtxVar "M"], [AtomVar "a", AtomVar "a"])
        val z = Ctx([CtxVar "N"], [AtomVar "a"])
        val b = Unify_ctx(w, z)
    in  print_sigs_cons b end

fun test_19() = 
    let val w = Ctx([CtxVar "G1"], [FormVar "A", FormVar "B", AtomVar "v"])
        val z = Ctx([CtxVar "G3"], [AtomVar "C", AtomVar "C", AtomVar "v"])
        val b = Unify_ctx(w, z)
    in  print_sigs_cons b end

fun test_20() = 
    let val w = Ctx([CtxVar "G1"], [FormVar "x", Form (Con "f", [FormVar "x"])])
        val z = Ctx([CtxVar "G2"], [FormVar "x", Form (Con "f", [FormVar "t"])])
        val b = Unify_ctx(w, z)
    in  print_sigs_cons b end

fun test_21() = 
    let val w = Ctx([], [FormVar "x", FormVar "z", FormVar "w"])
        val z = Ctx([], [FormVar "y", FormVar "y", FormVar "t"])
        val b = Unify_ctx(w, z)
    in  print_sigs_cons b end

fun test_22() = 
    let val w = Ctx([CtxVar "G1"], [FormVar "x", FormVar "z"])
        val z = Ctx([CtxVar "G2"], [FormVar "y", FormVar "y"])
        val b = Unify_ctx(w, z)
    in  print_sigs_cons b end

fun test_23() = 
    let val w = Ctx([CtxVar "G1", CtxVar "G2"], [FormVar "H", Form (Con "v", [FormVar "X", FormVar "Y"]), AtomVar "t"])
        val z = Ctx([CtxVar "G3", CtxVar "G4"], [Form (Con "v", [FormVar "X", FormVar "Y"]), Atom "q", FormVar "F"])
        val b = Unify_ctx(w, z)
    in  print_sigs_cons b end

fun test_24() = 
    let val w = Ctx([], [FormVar "H", FormVar "R", FormVar "T", FormVar "R"])
        val z = Ctx([], [FormVar "H", FormVar "G", FormVar "G", FormVar "V"])
        val b = Unify_ctx(w, z)
    in  print_sigs_cons b end

fun test_25() = 
    let val w = Ctx([CtxVar "G1"], [FormVar "x", FormVar "z"])
        val z = Ctx([CtxVar "G2"], [FormVar "y", FormVar "w"])
        val b = Unify_ctx(w, z)
    in  print_sigs_cons b end


(* Insert your Test Cases below with the following format *)

(* fun test_**NAME**() = 
    let val CONTEXT_1 = Ctx( **LIST OF MULTISETS**, **LIST OF TERMS** )
        val CONTEXT_2 = Ctx( **LIST OF MULTISETS**, **LIST OF TERMS** )
        val LIST_OF_UNIFICATIONS = Unify_ctx(CONTEXT_1, CONTEXT_2)
    in  print_sigs_cons LIST_OF_UNIFICATIONS end

(For terms, different types can be found in datatypes.sml)*)
