module Named = {
  type env = list<(string, int)>

  type rec expr =
    | Cst(int)
    | Add(expr, expr)
    | Mul(expr, expr)
    | Var(string)
    | Let(string, expr, expr)

  let assoc = (varName, l: env) => {
    switch l->Belt.List.getAssoc(varName, (a, b) => a == b) {
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
}

module Nameless = {
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
    | Var(n) => Belt.List.getExn(env, n)
    | Let(e1, e2) => eval(e2, list{eval(e1, env), ...env})
    }
  }

  type cenv = list<string>

  let findIndex = (list, str) => {
    let rec findWithIndx = (list, idx, str) => {
      switch list {
      | list{} => -1
      | list{hd, ...tail} => hd == str ? idx : findWithIndx(tail, idx + 1, str)
      }
    }
    findWithIndx(list, 0, str)
  }

  let rec comp = (expr: Named.expr, cenv: cenv): expr => {
    switch expr {
    | Cst(i) => Cst(i)
    | Add(a, b) => Add(comp(a, cenv), comp(b, cenv))
    | Mul(a, b) => Mul(comp(a, cenv), comp(b, cenv))
    | Var(x) => Var(findIndex(cenv, x))
    | Let(x, e1, e2) => Let(comp(e1, cenv), comp(e2, list{x, ...cenv}))
    }
  }
}

module StackMachineWithVar = {
  type instr = Cst(int) | Add | Mul | Var(int) | Pop | Swap
  type instrs = list<instr>
  type operand = int
  type stack = list<operand>

  let rec eval = (instrs: instrs, stk: stack) => {
    switch (instrs, stk) {
    | (list{Cst(i), ...rest}, _) => eval(rest, list{i, ...stk})
    | (list{Add, ...rest}, list{a, b, ...stk}) => eval(rest, list{a + b, ...stk})
    | (list{Mul, ...rest}, list{a, b, ...stk}) => eval(rest, list{a * b, ...stk})
    | (list{Var(i), ...rest}, stk) => eval(rest, list{Belt.List.getExn(stk, i), ...stk})
    | (list{Pop, ...rest}, list{_, ...stk}) => eval(rest, stk)
    | (list{Swap, ...rest}, list{a, b, ...stk}) => eval(rest, list{b, a, ...stk})
    | (list{}, list{hd, ..._}) => hd
    | _ => assert false
    }
  }

  let interpret = instr => instr->eval(list{})
}
