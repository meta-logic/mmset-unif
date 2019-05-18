use "apply.sml";


val init_fresh = ref 100;

fun printF (NONE) = []
    | printF (SOME(sigma)) = 
        List.map(fn ls => 
            List.map(fn sb => 
                (case sb of Fs(a,b) => (form_toString a ^ " => " ^ form_toString b)
                        |  CVs(a,b) =>  (ctx_var_toString a ^ " => " ^ ctx_toString b)
                ))ls)sigma

fun printS ([]) = []
    | printS (sigma) = 
        List.map(fn ls => 
            List.map(fn sb => 
                (case sb of Fs(a,b) => (form_toString a ^ " => " ^ form_toString b)
                        |  CVs(a,b) =>  (ctx_var_toString a ^ " => " ^ ctx_toString b)
                ))ls)sigma

fun printC ([]) = []
    | printC (cons) = 
        List.map(fn ls => 
            List.map(fn (g1,g2,g3) => 
                if (List.length g2 = 0) andalso (List.length g3 = 0) then "NO CONSTRAINT"
                else ctx_var_toString g1 ^ " |- " ^ ctx_varL_toString g2 ^ " = " ^ ctx_varL_toString g3)ls)cons

fun print_sigs_cons (NONE) = []
    | print_sigs_cons (SOME sc_ls) = let val (s,c) = ListPair.unzip(sc_ls) in ListPair.zip (printS s, printC c) end


fun Unify_form (x,y) = 
    let
        fun cat_compose(sigma1, unilst) = 
            let val pair = ListPair.zip(sigma1, unilst) 
            in List.concat(
                    List.map(fn(a, b) =>  
                    (case b of NONE => [] 
                            | SOME(bls) => List.map(fn bv => UnifierComposition(a, bv))bls ) 
                    )pair)end

        fun search (f1, Form(c, fls)) = 
                List.exists (fn f2 => form_eq(f1, f2) orelse search (f1, f2)) fls
            | search (_, _) = false

        fun Unify_f (AtomVar(x), AtomVar(y)) = if x=y then SOME[nil] else
                    SOME([ [Fs(AtomVar(x), AtomVar(y))] ])
            | Unify_f (AtomVar(x), FormVar(y)) = SOME([ [Fs(FormVar(y), AtomVar(x))] ])
            | Unify_f (AtomVar(x), Atom(y)) = SOME([ [Fs(AtomVar(x), Atom(y))] ])
            | Unify_f (Atom(x), AtomVar(y)) = SOME([ [Fs(AtomVar(y), Atom(x))] ])
            | Unify_f (Atom(x), FormVar(y)) = SOME([ [Fs(FormVar(y), Atom(x))] ])
            | Unify_f (FormVar(x), AtomVar(y)) = SOME([ [Fs(FormVar(x), AtomVar(y))] ])
            | Unify_f (FormVar(x), FormVar(y)) = if x=y then SOME([nil]) else 
                    SOME([ [Fs(FormVar(x), FormVar(y))] ])
            | Unify_f (FormVar(x), Atom(y)) = SOME([ [Fs(FormVar(x), Atom(y))] ])
            | Unify_f (FormVar(x), Form(c, fls)) = if search(FormVar(x), Form(c, fls)) 
                    then NONE else SOME([ [Fs(FormVar(x), Form(c, fls))] ])
            | Unify_f (Form(c, fls), FormVar(x)) = if search(FormVar(x), Form(c, fls)) 
                    then NONE else SOME([ [Fs(FormVar(x), Form(c, fls))] ])
            | Unify_f (Form(c1, fls1), Form(c2, fls2)) = 
                if form_eq(Form(c1, fls1), Form(c2, fls2)) then SOME([nil])
                else if conn_eq(c1,c2) then Unify_sf (fls1, fls2) else NONE
            | Unify_f (_, _) = NONE
        and Unify_sf ([], []) = NONE
            | Unify_sf ([x], [y]) = Unify_f(x,y)
            | Unify_sf (f1::fls1, f2::fls2) = 
                (case Unify_f(f1,f2) of
                    NONE => NONE
                    | SOME(sigma1) => 
                        let val sigma1fls1 = apply_formL_allUnifiers (fls1, sigma1)
                            val sigma1fls2 = apply_formL_allUnifiers (fls2, sigma1)
                            val sigma1flspair = ListPair.zip(sigma1fls1,sigma1fls2)
                            val unis = List.map(fn(a,b) => Unify_sf(a,b))sigma1flspair 
                        in
                            SOME(cat_compose(sigma1, unis))
                        end)
            | Unify_sf (_, _) = NONE
    in
        Unify_f(x,y)
    end


fun Unify_formL (x, y) = 
    let
        fun cat_compose(sigma1, unilst) = 
            let val pair = ListPair.zip(sigma1, unilst) 
            in List.concat(
                    List.map(fn(a, b) =>  
                    (case b of NONE => [] 
                            | SOME(bls) => List.map(fn bv => UnifierComposition(a, bv))bls ) 
                    )pair)end

        fun Unify_fL ([], []) = SOME(nil)
            | Unify_fL ([x], [y]) = Unify_form(x,y)
            | Unify_fL (f1::fls1, f2::fls2) = 
                (case Unify_form(f1,f2) of
                    NONE => NONE
                    | SOME(sigma1) => 
                        let val sigma1fls1 = apply_formL_allUnifiers (fls1, sigma1)
                            val sigma1fls2 = apply_formL_allUnifiers (fls2, sigma1)
                            val sigma1flspair = ListPair.zip(sigma1fls1,sigma1fls2)
                            val unis = List.map(fn(a,b) => Unify_fL(a,b))sigma1flspair 
                        in
                            SOME(cat_compose(sigma1, unis))
                        end)
            | Unify_fL (_, _) = NONE  
    in
        Unify_fL(x,y)
    end


(* NEED TO MAKE SURE THAT THERE ARE NO REPEATS, POSSIBLY MEMOIZE *)

fun Unify_ctx (Ctx(vl1, fl1), Ctx(vl2, fl2)) =
    let
        fun fresh(CtxVar(x)) = CtxVar(x ^ "'")

        fun get_constraint (g1, g2) = 
            let val () = () in init_fresh := !init_fresh + 1;
            (CtxVar("Gamma_" ^ Int.toString(!init_fresh)), g1, g2) end

        fun post_ctx (sigma) = List.concat(List.map(fn CVs(_, Ctx(cv, _)) => cv) sigma)

        fun try_permutations (chosen_l1, chosen_l2) =
            if List.length(chosen_l1) = 0 andalso List.length(chosen_l2) = 0 then [nil] else
            if not (List.length(chosen_l1) = List.length(chosen_l2))  then [] else
                List.concat(
                    List.map(fn SOME(s) => s)
                        (List.filter(fn NONE => false | _ => true)
                            (List.map(fn (perm_l1, perm_l2) => Unify_formL(perm_l1, perm_l2))
                                (List.map(fn p1 => (p1, chosen_l2))
                                    (permutations(chosen_l1, form_eq))
                                )
                            )
                        )
                    )

        fun part (vl1, fl1, vl2) =
            if List.length(fl1) = 0 andalso List.length(vl2) = 0 then [nil] else
                let val part = partition_into (List.length(vl2), fl1)
                    val vl_sigmas =
                        List.map(fn pf =>
                            List.map(fn (p,g) =>
                                if List.length(p) = 0 then
                                    if List.length(vl1) = 0 then CVs(g, Ctx([], []))
                                    else CVs(g, Ctx([g], []))
                                else 
                                    if List.length(vl1) = 0 then CVs(g, Ctx([], p))
                                    else CVs(g, Ctx([fresh(g)], p))
                            )(ListPair.zip(pf, vl2))
                        )part
                in vl_sigmas end

        fun try_partitions (fl1, vl1, fl2, vl2) =
            let val sigma1 = part (vl1, fl1, vl2)
                val sigma2 = part (vl2, fl2, vl1)
            in
                if List.length(sigma1) = 0 then [] else
                if List.length(sigma2) = 0 then [] else
                let val temp =
                    List.concat(
                        List.map(fn s1 => 
                            List.map(fn s2 => 
                                (s1 @ s2, [get_constraint(post_ctx s1, post_ctx s2)])
                            )sigma1
                        )sigma2)
                in 
                    List.map(fn (sb,c) => (List.filter(fn CVs(cx, Ctx(cxs, _)) => 
                        not (List.length(cxs) = 1 andalso ctx_var_eq (List.hd cxs, cx)))sb, c))temp
                end
            end

        fun unify_specific_k (vl1, fl1, vl2, fl2, i) =
            let val k_fl1 = chooseK(fl1, i)
                val k_fl2 = chooseK(fl2, i)
                val everyMatchOfK  = 
                    List.concat(
                        List.map(fn (chosen_l2, left_l2) => 
                            List.concat(
                                List.map(fn (chosen_l1, left_l1) => 
                                    let val permSubs_chl1_chl2 = try_permutations (chosen_l1, chosen_l2)
                                        val partSubs_left1_left2 = try_partitions (left_l1, vl1, left_l2, vl2)
                                    in
                                        List.concat(
                                            List.map(fn p1 => 
                                                List.map(fn (p2, c) => (UnifierComposition(p2,p1), c)
                                                )partSubs_left1_left2
                                            )permSubs_chl1_chl2)
                                    end
                                )k_fl1)
                        )k_fl2)
            in
                everyMatchOfK
            end

        fun Unify_ctx_AUX (Ctx([], fl1), Ctx([], fl2)) = 
                let val subs = try_permutations (fl1, fl2)
                    val cons = List.map(fn (s) => [get_constraint ([], [])])subs
                in if List.length(subs) = 0 then NONE
                else SOME(ListPair.zip(subs, cons)) end
            | Unify_ctx_AUX (Ctx(vl1, fl1), Ctx([], fl2)) = 
                if List.length(fl1) > List.length(fl2) then NONE
                else SOME(unify_specific_k(vl1, fl1, [], fl2, List.length fl1))
            | Unify_ctx_AUX (Ctx([], fl1), Ctx(vl2, fl2)) = 
                if List.length(fl1) < List.length(fl2) then NONE
                else SOME(unify_specific_k([], fl1, vl2, fl2, List.length fl2))
            | Unify_ctx_AUX (Ctx(vl1, fl1), Ctx(vl2, fl2)) = 
                let val minforms = Int.min(List.length fl1, List.length fl2)
                    val sc = List.concat(List.tabulate(minforms+1, fn i => 
                                        unify_specific_k(vl1, fl1, vl2, fl2, i)))
                in SOME(sc) end
    in
        let val (fl1,fl2) = remove_similar (fl1, fl2, form_eq) in
            case Unify_ctx_AUX(Ctx(vl1, fl1), Ctx(vl2, fl2)) of
                NONE => NONE
                | SOME(sb) => SOME(rm_dup_subs sb)
        end
    end