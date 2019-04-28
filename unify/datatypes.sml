datatype conn = Con of string 
datatype form = Atom of string | AtomVar of string | FormVar of string | Form of conn * form list
datatype ctx_var = CtxVar of string
datatype ctx = Ctx of ctx_var list * form list


datatype s = Fs of form * form | CVs of ctx_var * ctx 
type subs = s list


(*Printing functions for the above datatypes*)

fun form_toString (Atom (s)) = s
    | form_toString (AtomVar(s)) = s
    | form_toString (FormVar (s)) = s
    | form_toString (Form (Con (c), fl)) = c ^ "(" ^ subforms_toString(fl) ^ ")"
and subforms_toString (nil) = ""
    | subforms_toString ([x]) = form_toString(x)
    | subforms_toString (x::l) = form_toString(x) ^ ", " ^ subforms_toString(l)

fun ctx_var_toString (CtxVar(s)) = s

fun ctx_varL_toString (nil) = ""
    | ctx_varL_toString ([x]) = ctx_var_toString(x)
    | ctx_varL_toString (x::l) = ctx_var_toString(x) ^ ", " ^ ctx_varL_toString (l)

fun formL_toString (nil) = ""
    | formL_toString ([x]) = form_toString(x)
    | formL_toString (x::l) = form_toString(x) ^ ", " ^ formL_toString(l)

fun ctx_toString (Ctx([],[])) = "EMPTY" ^ ", " ^ "EMPTY"
    | ctx_toString (Ctx([],fl)) = "EMPTY" ^ ", " ^ formL_toString(fl)
    | ctx_toString (Ctx(vl,[])) = ctx_varL_toString(vl) ^ ", " ^ "EMPTY"
    | ctx_toString (Ctx(vl,fl)) = ctx_varL_toString(vl) ^ ", " ^ formL_toString(fl)


(*Equality functions for the above datatypes*)

fun list_subgroup (nil, _, _) = true
    | list_subgroup (a::list1, list2, compare_fun) = 
        List.exists (fn x => compare_fun(x, a)) list2 andalso list_subgroup (list1, list2, compare_fun)

fun list_eq (list1, list2, compare_function) = 
        list_subgroup (list1, list2, compare_function) andalso list_subgroup (list2, list1, compare_function)

fun conn_eq (Con(a), Con(b)) = a=b

fun form_eq (Atom(a), Atom(b)) = a=b
    | form_eq (AtomVar(a), AtomVar(b)) = a=b
    | form_eq (FormVar(a), FormVar(b)) = a=b
    | form_eq (Form(c1, l1), Form(c2, l2)) = conn_eq(c1,c2) andalso subforms_eq(l1,l2)
    | form_eq (_, _) = false
and subforms_eq (nil, nil) = true
    | subforms_eq (x::l1, y::l2) = form_eq(x,y) andalso subforms_eq(l1,l2)
    | subforms_eq (_, _) = false

fun ctx_var_eq (CtxVar(a), CtxVar(b)) = a=b

fun ctx_subgroup (Ctx(vl1,fl1), Ctx(vl2,fl2)) = 
    list_subgroup(vl1,vl2,ctx_var_eq) andalso list_subgroup(fl1,fl2,form_eq)

fun ctx_eq (Ctx(vl1,fl1), Ctx(vl2,fl2)) = 
    ctx_subgroup (Ctx(vl1,fl1), Ctx(vl2,fl2)) andalso ctx_subgroup(Ctx(vl2,fl2), Ctx(vl1,fl1))


fun subs_prefix_eq (Fs(a,_), Fs(b,_)) = form_eq(a,b)
    | subs_prefix_eq (CVs(a,_), CVs(b,_)) = ctx_var_eq(a,b)
    | subs_prefix_eq (_, _) = false

fun subs_eq (Fs(a1,b1), Fs(a2,b2)) = form_eq(a1,a2) andalso form_eq(b1,b2)
    | subs_eq (CVs(a1,b1), CVs(a2,b2)) = ctx_var_eq(a1,a2) andalso ctx_eq(b1,b2) 
    | subs_eq (_, _) = false
