type Term =
  Term.IntConst |
  Term.StrConst |
  Term.IntAdd |
  Term.StrConcat |
  Term.GetLocal |
  Term.GetGlobal |
  Term.IfZero |
  Term.CallFunc;

namespace Term {
  export class IntConst {
    constructor(public value: bigint) {
    }
  }

  export class StrConst {
    constructor(public value: string) {
    }
  }

  export class IntAdd {
    constructor(public lhs: Term, public rhs: Term) {
    }
  }

  export class StrConcat {
    constructor(public lhs: Term, public rhs: Term) {
    }
  }

  export class GetLocal {
    constructor(public name: string) {
    }
  }

  export class GetGlobal {
    constructor(public name: string) {
    }
  }

  export class IfZero {
    constructor(public testExpr: Term, public thenExpr: Term, public elseExpr: Term) {
    }
  }

  export class CallFunc {
    constructor(public funcExpr: Term, public argExprs: Array<Term>) {
    }
  }
}

type Value =
  Value.Int |
  Value.Str |
  Value.Func;

namespace Value {
  export class Int {
    constructor(public payload: bigint) {
    }
  }

  export class Str {
    constructor(public payload: string) {
    }
  }

  export class Func {
    constructor(public parameters: Array<string>, public body: Term) {
    }
  }
}

type Env = Map<string, Value>;

function assertInt(value: Value): asserts value is Value.Int {
  if (value instanceof Value.Int) {
    return;
  }
  else {
    throw new Error(`Mismatched type: expected Int, got ${value.constructor.name}`)
  }
}

function assertStr(value: Value): asserts value is Value.Str {
  if (value instanceof Value.Str) {
    return;
  }
  else {
    throw new Error(`Mismatched type: expected Str, got ${value.constructor.name}`)
  }
}

function assertFunc(value: Value): asserts value is Value.Func {
  if (value instanceof Value.Func) {
    return;
  }
  else {
    throw new Error(`Mismatched type: expected Func, got ${value.constructor.name}`)
  }
}

// precondition: the arrays are of equal length
function zip<S, T>(leftValues: Array<S>, rightValues: Array<T>): Array<[S, T]> {
  let result: Array<[S, T]> = [];
  for (let i = 0; i < leftValues.length; i++) {
    result.push([leftValues[i], rightValues[i]]);
  }
  return result;
}

function evaluate(term: Term, localEnv: Env, globalEnv: Env): Value {
  if (term instanceof Term.IntConst) {
    return new Value.Int(term.value);
  }
  else if (term instanceof Term.StrConst) {
    return new Value.Str(term.value);
  }
  else if (term instanceof Term.IntAdd) {
    let lhs = evaluate(term.lhs, localEnv, globalEnv);
    assertInt(lhs);
    let rhs = evaluate(term.rhs, localEnv, globalEnv);
    assertInt(rhs);
    return new Value.Int(lhs.payload + rhs.payload);
  }
  else if (term instanceof Term.StrConcat) {
    let lhs = evaluate(term.lhs, localEnv, globalEnv);
    assertStr(lhs);
    let rhs = evaluate(term.rhs, localEnv, globalEnv);
    assertStr(rhs);
    return new Value.Str(lhs.payload + rhs.payload);
  }
  else if (term instanceof Term.GetLocal) {
    let value = localEnv.get(term.name);
    if (value === undefined) {
      throw new Error(`Could not find local variable '${term.name}'`);
    }
    return value;
  }
  else if (term instanceof Term.GetGlobal) {
    let value = globalEnv.get(term.name);
    if (value === undefined) {
      throw new Error(`Could not find global variable '${term.name}'`);
    }
    return value;
  }
  else if (term instanceof Term.IfZero) {
    let testValue = evaluate(term.testExpr, localEnv, globalEnv);
    assertInt(testValue);
    if (testValue.payload === 0n) {
      return evaluate(term.thenExpr, localEnv, globalEnv);
    }
    else {
      return evaluate(term.elseExpr, localEnv, globalEnv);
    }
  }
  else if (term instanceof Term.CallFunc) {
    let funcValue = evaluate(term.funcExpr, localEnv, globalEnv);
    assertFunc(funcValue);
    if (funcValue.parameters.length !== term.argExprs.length) {
      let pl = funcValue.parameters.length;
      let al = term.argExprs.length;
      throw new Error(`${pl} parameters vs ${al} argument; mismatch`);
    }
    let argValues = term.argExprs.map((expr) => evaluate(expr, localEnv, globalEnv));
    let funcEnv: Env = new Map();
    for (let [paramName, argValue] of zip(funcValue.parameters, argValues)) {
      funcEnv.set(paramName, argValue);
    }
    return evaluate(funcValue.body, funcEnv, globalEnv);
  }
  else {
    let _coverageCheck: never = term;
    return _coverageCheck;
  }
}

type Program = Map<string, Value.Func>;

function runProgram(program: Program): void {
  let result: Value = evaluate(
    new Term.CallFunc(new Term.GetGlobal("main"), []),
    new Map(),
    program,
  );
  console.log("Program finished");
  if (result instanceof Value.Int) {
    console.log(`Int result: ${result.payload}`);
  }
  else if (result instanceof Value.Str) {
    console.log(`Str result: "${result.payload}"`);
  }
  else if (result instanceof Value.Func) {
    console.log(`Func result`);
  }
  else {
    let _coverageCheck: never = result;
    return _coverageCheck;
  }
}

const ZERO = new Term.IntConst(0n);
const ONE = new Term.IntConst(1n);
const MINUS_ONE = new Term.IntConst(-1n);
const MINUS_TWO = new Term.IntConst(-2n);
const TEN = new Term.IntConst(10n);

let program: Program = new Map();
program.set("fib", new Value.Func(["n"],
  new Term.IfZero(
    new Term.GetLocal("n"),
    ZERO,
    new Term.IfZero(
      new Term.IntAdd(new Term.GetLocal("n"), MINUS_ONE),
      ONE,
      new Term.IntAdd(
        new Term.CallFunc(
          new Term.GetGlobal("fib"),
          [new Term.IntAdd(new Term.GetLocal("n"), MINUS_TWO)],
        ),
        new Term.CallFunc(
          new Term.GetGlobal("fib"),
          [new Term.IntAdd(new Term.GetLocal("n"), MINUS_ONE)],
        ),
      )
    )
  ),
));
program.set("main", new Value.Func([],
  new Term.CallFunc(new Term.GetGlobal("fib"), [TEN]),
));

runProgram(program);
