// 栈式虚拟机支持的指令集
// 为了支持变量名的寻址 需要增加 Var(int) | Pop | Swap 操作
type instr = Cst(int) | Add | Mul | Var(int) | Pop | Swap
// 所有指令 是一个列表
type instrs = list<instr>
// 操作数
type operand = int
// 操作数的栈，对于一个 表达式 expr 来说 结束时栈顶元素的值就是表达式的值
// 要遵守栈平衡原则
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

let interpret = instrs => instrs->eval(list{})

let show = expr => {
  switch expr {
  | Cst(i) => "Cst(" ++ i->Belt.Int.toString ++ ");"
  | Add => "Add;"
  | Mul => "Mul;"
  | Pop => "Pop;"
  | Swap => "Swap;"
  | Var(a) => "Var(" ++ a->Belt.Int.toString ++ ");"
  }
}
