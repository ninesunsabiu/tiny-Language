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

{
  open Lambda
  let expr = App(Fn("a", Fn("b", Fn("c", App(Fn("d", Var("c")), Var("b"))))), Var("h"))

  show(expr)->Js.log2("lambda is ", _)

  getFreeVar(expr)
  ->ShowableList.show(it => "(" ++ it ++ ")", _)
  ->Js.log2("here is free Variable", _)
}

{
  open Lambda
  let x = Var("x")
  let y = Fn("y", x)
  let z = App(y, x)

  let expr = Fn("x", App(App(x, y), z))

  let n = App(z, x)

  let newExpr = subst("a", n, expr)
  Js.log2("origin expr", show(expr))
  Js.log2("subst [n/a] and n is", show(n))
  getFreeVar(n)->ShowableList.show(it => "(" ++ it ++ ")", _)->Js.log2("free Variable in n", _)
  Js.log2("subst ret", show(newExpr))
}
