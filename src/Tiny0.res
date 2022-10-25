type rec expr =
  | Cst(int)
  | Add(expr, expr)
  | Mul(expr, expr)

/**
 * 假装有一个函数在做 代码 到 AST 的转换
 */
type parse = string => expr

// Interpreter
let rec eval = expr => {
  switch expr {
  | Cst(i) => i
  | Add(a, b) => eval(a) + eval(b)
  | Mul(a, b) => eval(a) * eval(b)
  }
}

eval(Cst(1))->Js.log // log 1
eval(Add(Cst(1), Cst(2)))->Js.log // 3

// 2 + 1 * (3 + 4)
// equal 9
eval(Add(Cst(2), Mul(Cst(1), Add(Cst(3), Cst(4)))))->Js.log
