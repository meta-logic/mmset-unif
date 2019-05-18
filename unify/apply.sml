use "helpers.sml";


fun apply_form_Unifier (item, []) = item
    | apply_form_Unifier (Form(c, subforms), ls) = Form(c, apply_subform_Unifier(subforms, ls)) 
    | apply_form_Unifier (item, Fs(f1,f2)::ls) = (case form_eq(item, f1) of
            true => f2 | false => apply_form_Unifier (item, ls))
    | apply_form_Unifier (item, a::ls) = apply_form_Unifier (item, ls)
and apply_subform_Unifier ([], subs) = []
    | apply_subform_Unifier (item::fl, subs) =
        apply_form_Unifier (item, subs) :: apply_subform_Unifier (fl, subs)

fun apply_formL_Unifier ([], subs) = []
    | apply_formL_Unifier (item::fl, subs) =
        apply_form_Unifier (item, subs) :: apply_formL_Unifier (fl, subs)

fun apply_formL_allUnifiers (itemL, subsL) = List.map(fn sb => apply_formL_Unifier(itemL, sb))subsL


fun apply_ctx_var_Unifier (item, []) = ([item], [])
    | apply_ctx_var_Unifier (item, CVs(f1,Ctx(a,b))::ls) = if ctx_var_eq(item, f1) then (a,b) else apply_ctx_var_Unifier (item, ls)
    | apply_ctx_var_Unifier (item, a::ls) = apply_ctx_var_Unifier (item, ls)

fun apply_ctx_varL_Unifier ([], subs) = []
    | apply_ctx_varL_Unifier (item::vl, subs) =  
        apply_ctx_var_Unifier (item, subs) :: apply_ctx_varL_Unifier (vl, subs)

fun apply_ctx_varL_allUnifiers (itemL, subsL) = List.map(fn sb => apply_ctx_varL_Unifier(itemL, sb))subsL


fun compose (Fs(a,b), sigma) =  let val new_form = apply_form_Unifier(b, sigma) 
                                in (Fs(a, new_form), form_eq(a,b)) end
    | compose (CVs(a, Ctx(vls, fls)), sigma) =  let val fls' = apply_formL_Unifier(fls, sigma)
                                                    val (vls2, fls2) = ListPair.unzip(apply_ctx_varL_Unifier(vls, sigma))
                                                    val new_vls = List.concat(vls2)
                                                    val new_fls = fls' @ List.concat(fls2)
                                                    val new_ctx = Ctx(new_vls, new_fls)
                                                in 
                                                    (CVs(a, new_ctx), false)
                                                end

fun UnifierComposition (sigma1, []) = sigma1
    | UnifierComposition ([], sigma2) = sigma2
    | UnifierComposition (s::sigma1, sigma2) = 
        let val (s', equivalent) = compose(s, sigma2) in
        case equivalent of
            true => let val sigma1new = UnifierComposition(sigma1, sigma2) 
                        val sigma2new = 
                        List.filter(fn s2 => not (List.exists (fn s1 => subs_prefix_eq(s1, s2)) sigma1new))sigma2
                    in 
                        sigma1new @ sigma2new
                    end
            | _ =>  let val sigma1new = (s')::(UnifierComposition(sigma1, sigma2))
                        val sigma2new = 
                        List.filter(fn s2 => not (List.exists (fn s1 => subs_prefix_eq(s1, s2)) sigma1new))sigma2
                    in 
                        sigma1new @ sigma2new
                    end
        end
