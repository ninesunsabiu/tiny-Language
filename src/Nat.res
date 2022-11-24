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
  | S(n') => {
    // n * m <=> ((n - 1) + 1) * m <=> (n - 1) * m + m
    peano_add(peano_mul(n', m), m)
  }
  }
}

let church_add = (n: cnum<_>, m: cnum<_>): cnum<_> => {
  // add = λnmfx.nf(mfx)
  // impl  λfx.nf(mfx)
  (f, x) => n(f, m(f, x))
}

// let constTrue = (x, _) => x
// let constFase = (_, y) => y
// let ifThenElse = (x) => x

// // isZero(church_zero) => T 
// // isZero(church_one) => F
// let isZero = (n: cnum<_>) => n((_) => constFase, constTrue)

let church_mul = (n: cnum<_>, m: cnum<_>): cnum<_> => {
  // (λfx.f^nx) * (λfx.f^mx)
  // => λfx.f^(n * m)x
  (f, x) => n(fn => m(f, fn), x)
}

Js.Console.log("church_check:")
Js.Console.log(church_decode(church_one)) // 1
Js.Console.log(church_decode(church_two)) // 2
Js.Console.log(church_decode(church_add(church_one, church_two))) // 3
Js.Console.log(church_decode(church_mul(church_two, church_two))) // 4



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


