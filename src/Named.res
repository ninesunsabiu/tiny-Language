type rec expr =
  | Cst(int)
  | Add(expr, expr)
  | Mul(expr, expr)
  | Var(string)
  | Let(string, expr, expr)
  | Fn(list<string>, expr)
  | Apply(expr, list<expr>)

type rec value =
  | Vint(int)
  | Vclosure(env, list<string>, expr)
and env = list<(string, value)>

let assoc = (varName, l: env) => {
  switch l->Belt.List.getAssoc(varName, (a, b) => a === b) {
  | Some(r) => r
  | _ => assert false // 这里似乎是需要一个 语法 保证
  }
}

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

  | Var(x) => assoc(x, env) // 读取执行环境中 变量名为 x 的值

  | Let(x, e1, e2) => {
      let e1Val = e1->eval(env)
      // 在环境中写入 x 的值为 eval(e1, env)
      let newEnv = list{(x, e1Val), ...env}
      e2->eval(newEnv)
    }

  | Fn(args, e) =>
    // 保存当前环境 作为闭包变量
    // 这里直接将当前环境整一个的捕获到闭包内
    // 应该有优化空间，按照现在 js 调试时的情况来看，闭包捕获的变量只有在函数体内有使用的部份
    Vclosure(env, args, e)

  | Apply(e, paramsExpr) =>
    switch eval(e, env) {
    | Vclosure(closureEnv, paramsVariableNameList, body) => {
        // 实参列表 通过 eval 计算出真实值出来
        let paramsValue = paramsExpr->Belt.List.map(eval(_, env))
        let envForFunction = Belt.List.concatMany([
          // 把参数名和对应参数的值构成变量环境
          Belt.List.zip(paramsVariableNameList, paramsValue),
          // 和闭包环境一起合成执行函数主体时的运行环境
          closureEnv,
        ])
        body->eval(envForFunction)
      }

    | _ => assert false // 应该还是要通过编译期检查 确保 apply 只能接受不了函数
    }
  }
}
