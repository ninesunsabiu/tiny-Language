{
  open Named
  Let("x", Cst(17), Add(Var("x"), Var("x")))
}
->Nameless.comp(list{})
->CompileHelper.namelessExprToStackVM
->StackMachineWithVar.interpret
->Js.log2("result", _) // output 34 <- 17 + 17

let addNamedExpr = {
  open Named
  Apply(Fn(list{"x", "y"}, Add(Var("x"), Var("y"))), list{Cst(1), Cst(2)})
}

addNamedExpr->Named.eval(list{})->Js.log2("named fn expr result", _)

addNamedExpr
->Nameless.comp(list{})
->Nameless.eval(list{})
->Js.log2("result fn nameless expr result", _) // output 3

{
  open Lambda
  // λy.λx.xy(λy.λx.xy) => λx.x(λy.λx.xy)
  show(
    eval(App(Fn("y", Fn("x", App(Var("x"), Var("y")))), Fn("y", Fn("x", App(Var("x"), Var("y")))))),
  )
}->Js.log2("after eval lambda is", _)
