type env = list<(string, int)>

type rec expr =
  | Cst(int)
  | Add(expr, expr)
  | Mul(expr, expr)
  | Var(string)
  | Let(string, expr, expr)

let assoc = (varName, l: env) => {
  switch l->Belt.List.getAssoc(varName, (a, b) => a === b) {
  | Some(r) => r
  | _ => assert false // 这里似乎是需要一个 语法 保证
  }
}

let rec eval = (expr, env) => {
  switch expr {
  | Cst(i) => i
  | Add(a, b) => eval(a, env) + eval(b, env)
  | Mul(a, b) => eval(a, env) * eval(b, env)
  | Var(x) => assoc(x, env) // 读取执行环境中 变量名为 x 的值
  | Let(x, e1, e2) => {
      let e1Val = e1->eval(env)
      // 在环境中写入 x 的值为 eval(e1, env)
      let newEnv = list{(x, e1Val), ...env}
      e2->eval(newEnv)
    }
  }
}
