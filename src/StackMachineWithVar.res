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
    | Var(n) => {
      // env->ShowableList.show(it => it->Belt.Int.toString ++ "::", _)->Js.log2("此时的变量环境的值为", _)
      // n->Belt.Int.toString->Js.log2("此时索引的偏移量为", _)
      Belt.List.getExn(env, n)
    }
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

  // 将一个 Named 转换为 Nameless 
  let rec comp = (expr: Named.expr, cenv: cenv): expr => {
    // cenv->ShowableList.show(it => it ++ "::", _)->Js.log2("cenv: ", _)
    switch expr {
    | Cst(i) => Cst(i)
    | Add(a, b) => Add(comp(a, cenv), comp(b, cenv))
    | Mul(a, b) => Mul(comp(a, cenv), comp(b, cenv))
    | Var(x) => Var(findIndex(cenv, x))
    | Let(x, e1, e2) => {
      Let(comp(e1, cenv), comp(e2, list{x, ...cenv}))
    }
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
}

// 作业 1
// Write an interpreter for the stack machine with variables
module StackMachineWithVar = {
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
}

type stackOfMachine = {
  variablesPosition: list<int>, // 记录每次绑定的变量时，栈的高度
  lengthOfMachinStack: int, // 记录栈虚拟机的栈高度
}
// Write a compiler to translate to stack machine instructions
// 将 Nameless.expr 转换到 栈式虚拟机中
let namelessExprToStackVM = (expr: Nameless.expr): StackMachineWithVar.instrs => {
  open Belt.List
  open StackMachineWithVar
  let rec compileToInstr = (expr: Nameless.expr, stack: stackOfMachine) => {
    switch expr {
    | Nameless.Cst(i) => list{Cst(i)}
    | Nameless.Add(a, b) =>
      concatMany([
        compileToInstr(a, stack),
        compileToInstr(b, {...stack, lengthOfMachinStack: stack.lengthOfMachinStack + 1}),
        list{Add},
      ])
    | Nameless.Mul(a, b) =>
      concatMany([
        compileToInstr(a, stack),
        compileToInstr(b, {...stack, lengthOfMachinStack: stack.lengthOfMachinStack + 1}),
        list{Mul},
      ])
    | Nameless.Var(i) => {
        // 当初被放置在虚拟机中栈的高度（位置）
        let thePosWhenLetBind = stack.variablesPosition->getExn(i)
        // 当前虚拟机栈顶到放置时高度的偏移量
        let offsetOfCureentLength = stack.lengthOfMachinStack - thePosWhenLetBind
        list{Var(offsetOfCureentLength)}
      }

    | Nameless.Let(a, b) =>
      concatMany([
        compileToInstr(a, stack),
        compileToInstr(
          b,
          {
            let newLength = stack.lengthOfMachinStack + 1
            {
              variablesPosition: list{newLength, ...stack.variablesPosition},
              lengthOfMachinStack: newLength,
            }
          },
        ),
        list{Swap, Pop},
      ])
    }
  }

  expr->compileToInstr({variablesPosition: list{}, lengthOfMachinStack: 0})
}

// let namedExpr = {
//   open Named
//   Let("x", Add(Cst(1), Cst(3)), Let("y", Cst(10), Add(Var("x"), Var("y"))))
// }

// // namedExpr->Named.eval(list{})->Js.log
// Js.log("nameless: ")
// namedExpr->Nameless.comp(list{})->Nameless.show->Js.log

// let namelessExpr = {
//   open Named
//   Let(
//     "x", 
//     Cst(17),
//     Add(
//       Var("x"),
//       Let(
//         "x",
//         Cst(6),
//         Mul(Cst(3), Add(Var("x"), Var("x")))
//       )
//   ))
//   // Let("x", Cst(17), Add(Cst(1), Var("x")))
// }
// // Let(Cst(17),Add(Var(0),Let(Var(0),Mul(Cst(3),Add(Var(0),Cst(4))))))
// // Let("x", Cst(17), Add(Var("x"), Var("x")))
// ->Nameless.comp(list{})
// Js.log2("nameless expr:", Nameless.show(namelessExpr))
// namelessExpr->Nameless.eval(list{})->Js.log

// let instrs = namelessExpr->namelessExprToStackVM
// ->ShowableList.show(StackMachineWithVar.show, _)
// ->Js.log2("stack machine instr", _)
// Cst(17);Var(0);Var(0);Cst(3);Var(0);Cst(4);Add;Mul;Swap;Pop;Add;Swap;Pop;
// instrs->StackMachineWithVar.interpret->Js.log
