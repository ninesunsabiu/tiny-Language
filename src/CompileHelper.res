open StackMachineWithVar

type varType = SLocal | STmp
type env = list<varType>

// 在 env 中查找第 i 个 SLocal
let indexInEnv = (i, env) => {
  let rec findNthLocalVar = (findedCount, idx, env) => {
    switch env {
    | list{} => raise (Not_found)
    | list{STmp, ...tail} => findNthLocalVar(findedCount, idx + 1, tail)
    | list{SLocal, ...tail} => findedCount === 0 ? idx : findNthLocalVar(findedCount - 1, idx + 1, tail)
    } 
  }

  findNthLocalVar(i, 0, env)
}

let namelessExprToStackVM = (expr: Nameless.expr): instrs => {
  open Belt.List
  let rec compileToInstr = (expr: Nameless.expr, env: env) => {
    switch expr {
    | Nameless.Cst(i) => list{Cst(i)}
    | Nameless.Add(a, b) =>
      concatMany([compileToInstr(a, env), compileToInstr(b, list{STmp, ...env}), list{Add}])
    | Nameless.Mul(a, b) =>
      concatMany([compileToInstr(a, env), compileToInstr(b, list{STmp, ...env}), list{Mul}])
    | Nameless.Var(i) => list{Var(indexInEnv(i, env))}
    | Nameless.Let(a, b) =>
      concatMany([compileToInstr(a, env), compileToInstr(b, list{SLocal, ...env}), list{Swap, Pop}])
    }
  }

  expr->compileToInstr(list{})
}
