{
    open Named
    Let("x", Cst(17), Add(Var("x"), Var("x")))
}
->Nameless.comp(list{})
->CompileHelper.namelessExprToStackVM
->StackMachineWithVar.interpret
->Js.log2("result", _)  // output 34 <- 17 + 17
