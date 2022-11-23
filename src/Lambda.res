type rec expr =
  | Var(string)
  | Fn(string, expr)
  | App(/* 函数 */ expr, /* 应用参数 */ expr) // lambda 可以用函数做参数

let show = l => {
  let printParen = (parenthesis, str) => {
    if parenthesis {
      "(" ++ str ++ ")"
    } else {
      str
    }
  }
  let rec go = (l, p) => {
    switch l {
    | Var(x) => x
    | Fn(x, a) => printParen(p > 0, "λ" ++ x ++ "." ++ go(a, 0))
    | App(a, b) => printParen(p > 1, go(a, 1) ++ " " ++ go(b, 2))
    }
  }
  go(l, 0)
}

// 从一个 expr 中得到自由变量列表
let getFreeVar = expr => {
  let rec go = (ans, expr) => {
    switch expr {
    | Var(a) => list{a, ...ans}
    | Fn(arg, body) => go(ans, body)->Belt.List.keep(it => it !== arg) // 排除掉绑定变量 arg

    | App(fn, param) => go(go(ans, fn), param)
    }
  }
  go(list{}, expr)
}

// 返回一个新的标识符
// getNewSymbol(i) !== i
let getNewSymbol = i => i ++ "_"

// expr2[expr1/s] 在表达式2中 用 表达式1 把 s 给替换掉
// s, expr2 一般是一个 lambda fn
// 这个过程 感觉有点像 贝塔规约
let rec subst = (s, expr1, expr2) => {
  // bind s and expr1
  let substitution = subst(s, expr1)
  // 根据 expr2 的情况枚举处理
  switch expr2 {
  | Var(ss) if ss === s => expr1 // 找到替换的目标 则 Var(ss) 换成 expor1
  | Var(_) => expr2 // 没得换 直接返回 expor2
  | Fn(arg, body) =>
    // Think about how substitution works on arbitrary terms, i.e. N[M/x] where M could contain free variables.
    if arg === s {
      // 否则如果绑定变量和当前替换的 s 相等的话，产生遮蔽的效果，替换不了了 原样返回
      expr2
    } else if getFreeVar(expr1)->Belt.List.has(arg, String.equal) {
      // 如果 body 中绑定变量 arg 和 expr1 中有命名冲突需要进行处理
      let symbol = getNewSymbol(arg)
      let newBody = subst(arg, Var(symbol), body)
      // 基于新的
      Fn(symbol, substitution(newBody))
    } else if arg !== s {
      // 如果绑定变量和当前替换的 s 不相等的话，直接穿透 在 body 继续替换
      Fn(arg, substitution(body))
    } else {
      assert false
    }
  | App(a, b) => App(substitution(a), substitution(b)) // a,b 递归处理即可
  }
}

let rec eval = e => {
  switch e {
  | Var(_) => assert false
  | Fn(_, _) => e // 函数本身不做计算了 直接返回
  | App(f, arg) => {
      // 求值计算
      let (x, body) = switch eval(f) {
      | Fn(x, body) => (x, body)
      | _ => assert false // bottom value 异常情况
      }
      let va = eval(arg)
      // 替换 => body[Va/x]
      subst(x, va, body)->eval
    }
  }
}
