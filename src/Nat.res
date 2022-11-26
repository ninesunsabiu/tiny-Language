// Peano numbers
type rec nat = Z | S(nat)

let peano_zero = Z
let peano_one = S(Z)
let peano_two = S(S(Z))

// Church numbers
type cnum<'a> = ('a => 'a, 'a) => 'a

let church_zero = (s, z) => z
let church_one = (s, z) => s(z)
let church_two = (s, z) => s(s(z))
let church_three = (s, z) => s(s(s(z)))

// successor function
let peano_succ = x => S(x)
let church_succ = (n, s, z) => s(n(s, z))

// isomorphism between Peano and Church numbers
let church_to_peano = n => n(x => S(x), Z)
let rec peano_to_church = n => {
  switch n {
  | Z => church_zero
  | S(n') => church_succ(peano_to_church(n'))
  }
}

// predecessor
let pred = n => {
  let init = (church_zero, church_zero)
  let iter = ((_, y)) => (y, church_succ(y))
  let (ans, _) = n(iter, init)
  ans
}

let church_decode = n => n(x => x + 1, 0)

Js.Console.log(church_decode(church_two)) // 2
Js.Console.log(church_decode(pred(church_two))) // 1

// a + b
let rec peano_add = (n: nat, m: nat): nat => {
  switch n {
  | Z => m // 0 + k === k
  | S(n') => peano_add(n', peano_succ(m)) // n + m <=> (n - 1) + (m + 1)
  }
}
let rec peano_mul = (n: nat, m: nat): nat => {
  switch n {
  | Z => Z
  | S(n') =>
    // n * m <=> ((n - 1) + 1) * m <=> (n - 1) * m + m
    peano_add(peano_mul(n', m), m)
  }
}

// add = λnmfx.nf(mfx)
let church_add = (n: cnum<_>, m: cnum<_>): cnum<_> => (f, x) => n(f, m(f, x))

// let constTrue = (x, _) => x
// let constFalse = (_, y) => y
// let ifThenElse = (x) => x

// // isZero(church_zero) => T
// // isZero(church_one) => F
// let isZero = (n: cnum<_>) => n((_) => constFase, constTrue)

// mul = λnmfx.f^(n*m)x    mul m n => 对 f 迭代 n * m 次
let church_mul = (n: cnum<_>, m: cnum<_>): cnum<_> => {
  // n * m
  // <=> m + m + ... + m   (一共 n 个 m 相加)
  // <=> (...((m) + m) + m) + ...)
  // <=> (...((0 + m) + m) + m) + ...)

  // n => λnfx.f^(n)x  对 f 迭代 n 次
  // m => λmfx.f^(m)x  对 f 迭代 m 次

  // 记 F = λx.f^m  把 f apply 到 m 上 即 m(f)
  // mulBody = F^(n)x 把 F appy 到 n 上，即 n(F)
  f => n(/* F */ m(f))
}

Js.Console.log("church_check:")
Js.Console.log(church_decode(church_one)) // 1
Js.Console.log(church_decode(church_two)) // 2
Js.Console.log(church_decode(church_add(church_one, church_two))) // 3
Js.Console.log(church_decode(church_mul(church_two, church_three))) // 6

// The following is an alternative approach to define arithmetic
// function for peano numbers
let nat_fold = (n, s, z) => {
  let rec go = n =>
    switch n {
    | Z => z
    | S(n') => s(go(n'))
    }
  go(n)
}

let add = (n, m) => nat_fold(n, x => S(x), m)
let mul = (n, m) => nat_fold(n, x => add(m, x), Z)
let exp = (n, m) => nat_fold(m, x => mul(n, x), S(Z))

let peano_decode = n => nat_fold(n, x => x + 1, 0)

let three = S(S(S(Z)))
let two = S(S(Z))

Js.Console.log("nat_fold check") // 3
Js.Console.log(peano_decode(three)) // 3
Js.Console.log(peano_decode(two)) // 2
Js.Console.log(peano_decode(add(three, two))) // 5
Js.Console.log(peano_decode(mul(three, two))) // 6
Js.Console.log(peano_decode(exp(three, two))) // 9

{
  open Lambda

  // λfx.x
  let church_zero = Fn("f", Fn("x", Var("x")))
  // λfx.fx
  let church_one = Fn("f", Fn("x", App(Var("f"), Var("x"))))
  // λfx.f(fx)
  let church_two = Fn("f", Fn("x", App(App(Var("f"), Var("f")), Var("x"))))
  // λfx.f(f(fx))
  let church_three = Fn("f", Fn("x", App(App(App(Var("f"), Var("f")), Var("f")), Var("x"))))

  let constTrue = Fn("x", Fn("y", Var("x")))
  let constFalse = Fn("x", Fn("y", Var("y")))
  let ifThenElse = Fn("x", Var("x"))

  // λnfx.f(nfx)
  // succ zero => (λnfx.f(nfx))(λfx.x) => λfx.f((λfx.x)fx) => λfx.fx => one
  // succ one => (λnfx.f(nfx))(λfx.fx) => λfx.f((λfx.fx)fx) => λfx.f(fx)
  let successor = Fn("n", Fn("f", Fn("x", App(Var("f"), App(App(Var("n"), Var("f")), Var("x"))))))
  eval(App(successor, church_two))->show->Js.log2("succ church_two", _)

  // λnmfx.nf(mfx)
  // add one two => λfx.(one)f((two)fx) => λfx.(λx.fx)(f(fx)) => λfx.f(f(fx))
  let add = Fn(
    "n",
    Fn("m", Fn("f", Fn("x", App(App(Var("n"), Var("f")), App(App(Var("m"), Var("f")), Var("x")))))),
  )
  eval(App(App(add, church_one), church_two))->show->Js.log2("add one two", _)

  let iszero = Fn("n", App(App(Var("n"), Fn("z", constFalse)), constTrue))
  eval(App(iszero, church_one))->show->Js.log2("is zero", _)

  let pair = Fn("x", Fn("y", Fn("z", App(App(Var("z"), Var("x")), Var("y")))))
  let fst = Fn("p", App(Var("p"), constTrue))
  let snd = Fn("p", App(Var("p"), constFalse))

  eval(App(fst, App(App(pair, church_one), church_two)))->show->Js.log2("fst pair one two", _)

  let pred = Fn(
    "n",
    App(
      fst,
      App(
        App(
          Var("n"),
          Fn(
            "p",
            {
              let p = Var("p")
              let sndP = App(snd, p)
              let pairSndP = App(pair, sndP)
              let succSndP = App(successor, sndP)
              App(pairSndP, succSndP)
            },
          ),
        ),
        App(App(pair, church_zero), church_zero),
      ),
    ),
  )

  // FIXME 这里算出来 和 church_three 不等价
  eval(App(pred, App(successor, church_three)))->show->Js.log2("pred lambda", _)
}
