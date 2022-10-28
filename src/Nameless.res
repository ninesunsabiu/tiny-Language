type env = list<int>

type rec expr =
  | Cst(int)
  | Add(expr, expr)
  | Mul(expr, expr)
  | Var(int) // 类似使用偏移量来读取绑定量
  | Let(expr, expr)

let rec eval = (expr, env) => {
  switch expr {
  | Cst(i) => i
  | Add(a, b) => eval(a, env) + eval(b, env)
  | Mul(a, b) => eval(a, env) * eval(b, env)
  | Var(n) =>
    // env->ShowableList.show(it => it->Belt.Int.toString ++ "::", _)->Js.log2("此时的变量环境的值为", _)
    // n->Belt.Int.toString->Js.log2("此时索引的偏移量为", _)
    Belt.List.getExn(env, n)

  | Let(e1, e2) => eval(e2, list{eval(e1, env), ...env})
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
  }
}

let rec show = expr => {
  switch expr {
  | Cst(i) => "Cst(" ++ i->Belt.Int.toString ++ ")"
  | Add(a, b) => "Add(" ++ show(a) ++ "," ++ show(b) ++ ")"
  | Mul(a, b) => "Mul(" ++ show(a) ++ "," ++ show(b) ++ ")"
  | Let(a, b) => "Let(" ++ show(a) ++ "," ++ show(b) ++ ")"
  | Var(a) => "Var(" ++ a->Belt.Int.toString ++ ")"
  }
}
