open StackMachineWithVar

type stackOfMachine = {
  variablesPosition: list<int>, // 记录每次绑定的变量时，栈的高度
  lengthOfMachinStack: int, // 记录栈虚拟机的栈高度
}

let namelessExprToStackVM = (expr: Nameless.expr): instrs => {
  open Belt.List
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
