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
  | Fn(ss, e) if ss !== s => Fn(ss, substitution(e)) // 如果 expr2 是个函数定义 并且参数没有和要替换的相同 替换 body 部份 并且保留参数 ss
  | Fn(_, _) => expr2 // 要替换的参数 s 由于内部 expr2 遮蔽了,所以也换不得;同名但是不同含义
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
