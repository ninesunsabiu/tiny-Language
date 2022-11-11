type rec expr =
  | Cst(int)
  | Add(expr, expr)
  | Mul(expr, expr)
  | Var(int) // 类似使用偏移量来读取绑定量
  | Let(expr, expr)
  | Fn(expr)
  | Apply(expr, list<expr>)

type rec env = list<value>
and value =
  | Vint(int)
  | Vclosure(env, expr)

let vadd = (v1, v2): value => {
  switch (v1, v2) {
  | (Vint(a), Vint(b)) => Vint(a + b)
  | _ => assert false // 编译器检查 加法两边 只能是值 不能是函数加值
  }
}

let vmul = (v1, v2): value => {
  switch (v1, v2) {
  | (Vint(a), Vint(b)) => Vint(a * b)
  | _ => assert false // 编译器检查 乘法两边 只能是值 不能是函数加值
  }
}

let rec eval = (expr, env): value => {
  switch expr {
  | Cst(i) => Vint(i)
  | Add(a, b) => vadd(eval(a, env), eval(b, env))
  | Mul(a, b) => vmul(eval(a, env), eval(b, env))
  | Var(n) =>
    // env->ShowableList.show(it => it->Belt.Int.toString ++ "::", _)->Js.log2("此时的变量环境的值为", _)
    // n->Belt.Int.toString->Js.log2("此时索引的偏移量为", _)
    Belt.List.getExn(env, n)
  | Let(e1, e2) => eval(e2, list{eval(e1, env), ...env})
  | Fn(e) => Vclosure(env, e)
  | Apply(e, es) =>
    switch eval(e, env) {
    | Vclosure(closureEnv, fnBody) =>
      fnBody->eval(Belt.List.concatMany([es->Belt.List.map(eval(_, env)), closureEnv]))
    | _ => assert false
    }
  }
}

type cenv = list<string>

let findIndex = (list, str) => {
  let rec findWithIndx = (list, idx) => {
    switch list {
    | list{} => -1
    | list{hd, ...tail} => hd === str ? idx : findWithIndx(tail, idx + 1)
    }
  }
  findWithIndx(list, 0)
}

// 将一个 Named 转换为 Nameless
let rec comp = (expr: Named.expr, cenv: cenv): expr => {
  // cenv->ShowableList.show(it => it ++ "::", _)->Js.log2("cenv: ", _)
  switch expr {
  | Cst(i) => Cst(i)
  | Add(a, b) => Add(comp(a, cenv), comp(b, cenv))
  | Mul(a, b) => Mul(comp(a, cenv), comp(b, cenv))
  | Var(x) => Var(findIndex(cenv, x))
  | Let(x, e1, e2) => Let(comp(e1, cenv), comp(e2, list{x, ...cenv}))
  | Fn(args, e) => Fn(comp(e, Belt.List.concatMany([args, cenv])))

  | Apply(fnExp, paramsExpr) => Apply(comp(fnExp, cenv), paramsExpr->Belt.List.map(comp(_, cenv)))
  }
}

let rec show = expr => {
  switch expr {
  | Cst(i) => "Cst(" ++ i->Belt.Int.toString ++ ")"
  | Add(a, b) => "Add(" ++ show(a) ++ "," ++ show(b) ++ ")"
  | Mul(a, b) => "Mul(" ++ show(a) ++ "," ++ show(b) ++ ")"
  | Let(a, b) => "Let(" ++ show(a) ++ "," ++ show(b) ++ ")"
  | Var(a) => "Var(" ++ a->Belt.Int.toString ++ ")"
  | Fn(e) => "Fn(" ++ show(e) ++ ")"
  | Apply(e, es) => "Apply(" ++ show(e) ++ "exprs: " ++ ShowableList.show(show, es) ++ ")"
  }
}
