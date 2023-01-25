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

// I like linked lists. Linked lists have very few actual uses. Here we're using them
// as a (somewhat doubtful) way to string together a bunch of values in a persistent
// data structure, without doing any mutation. If we were to use arrays, and also insist
// that we never mutate those arrays, we would suffer a time complexity which is
// quadratic in the final length of such an array (due to all the copying).
type LinkedList<T> =
  LinkedList.Nil |
  LinkedList.Cons<T>;

namespace LinkedList {
  export class Nil {
  }

  export class Cons<T> {
    constructor(public payload: T, public nextLink: LinkedList<T>) {
    }
  }
}

function nil() {
  return new LinkedList.Nil();
}

function cons<T>(payload: T, nextLink: LinkedList<T>) {
  return new LinkedList.Cons(payload, nextLink);
}

function isCons<T>(list: LinkedList<T>): list is LinkedList.Cons<T> {
  return list instanceof LinkedList.Cons;
}

function forEach<T>(list: LinkedList<T>, fn: (el: T) => void): void {
  while (isCons(list)) {
    let el = list.payload;
    fn(el);
    list = list.nextLink;
  }
}

// We're going to introduce "resumption points" in the evaluator, which are all the
// points _after_ a recursive evaluation of a nested term comes back with a result.
// This result will be on top of the result stack, ready for use. The resumption points
// themselves will go on the term stack, mixing freely with as-yet unevaluated terms.
//
// Enumerating the terms and their resumption points, we get the following. Keep in
// mind that we get these simply by looking for recursive calls to `evaluate` in the
// original implicit-stack evaluator _after which there is more evaluator code_:
//
//     Term.IntConst
//         (none)
//     Term.StrConst
//         (none)      // these two have zero nested terms to evaluate
//     Term.IntAdd
//         After.IntAddLhs
//         After.IntAddRhs
//     Term.StrConcat
//         After.StrConcatLhs
//         After.StrConcatRhs
//     Term.GetLocal
//         (none)
//     Term.GetGlobal
//         (none)     // these two have zerzo nested terms to evaluate
//     Term.IfZero
//         After.IfZeroTestExpr
//         (no more)  // the `then` and `else` evaluations don't have any code After
//     Term.CallFunc
//         After.CallFuncEvalFunc  // this one does the length check
//         After.CallFuncNthArg    // `n` ranges from 0..N, N number of args
//         (no more)               // the last of the previous makes the actual call
//
// All in all, we have only 7 resumption points. The data that goes into these are in
// a one-to-one correspondence with the lexical variables in the evaluator at those
// points needed to complete the evaluation of the term. If they have already been
// type-asserted, they will have the type narrowed by the assertion. Additionally, they
// also carry the subterms that will possibly be needed in future evaluations. In
// particular, anything that needs to push more closed terms on the stack needs the
// local environment.

type After =
    After.IntAddLhs |
    After.IntAddRhs |
    After.StrConcatLhs |
    After.StrConcatRhs |
    After.IfZeroTestExpr |
    After.CallFuncEvalFunc |
    After.CallFuncNthArg;

namespace After {
  export class IntAddLhs {
    constructor(public rhs: Term, public localEnv: Env) {
    }
  }

  export class IntAddRhs {
    constructor(public lhs: Value.Int) {
    }
  }

  export class StrConcatLhs {
    constructor(public rhs: Term, public localEnv: Env) {
    }
  }

  export class StrConcatRhs {
    constructor(public lhs: Value.Str) {
    }
  }

  export class IfZeroTestExpr {
    constructor(public thenExpr: Term, public elseExpr: Term, public localEnv: Env) {
    }
  }

  export class CallFuncEvalFunc {
    constructor(public argExprs: Array<Term>, public localEnv: Env) {
    }
  }

  export class CallFuncNthArg {
    constructor(
      public funcValue: Value.Func,
      public i: number,
      public args: LinkedList<Value>,
      public argExprs: Array<Term>,
      public params: Array<string>,
      public localEnv: Env,
    ) {
    }
  }
}

// We call a term plus its local environment a "clsoed term". This was a change that
// snuck up on me; I was too focused on the resumption points to notice this one.
// Basically, we need to always pair up terms-to-be-evaluated (on the stack) with
// their local environment. This matters because not only do different functions have
// different local environments, but different function _activations_ have different
// local environment. As in, you can definitely call the same function with two different
// sets of arguments, and it will run in different local environments.
type ClosedTerm = { term: Term, localEnv: Env };

function isClosedTerm(thing: ClosedTerm | After): thing is ClosedTerm {
  return thing.hasOwnProperty("term") && thing.hasOwnProperty("localEnv");
}

function evaluate(term: Term, localEnv: Env, globalEnv: Env): Value {
  // To repeat, the new evaluator distinguishes itself by never calling itself
  // recursively. Instead, it's now mainly a _loop_ working down a _stack_ containing
  // a mix of unevaluated closed terms, and resumptions. Initially, the stack contains
  // only the term we start with.
  let stack: Array<ClosedTerm | After> = [{ term, localEnv }];

  // We also have a stack of result values. Due to the circumstances of how we use this
  // stack, it's either empty or contains one value at any point. The important thing
  // is that values that we used to return from (nested) evaluations, we now push on the
  // result stack instead, and the resumption code pops them off the result stack. After
  // the whole evaluation (of the initial term) finishes and we exit the loop, there will
  // be exactly one one result on the result stack, which we return.
  let results: Array<Value> = [];

  let current: ClosedTerm | After;
  while (current = stack.pop()!) {
    if (isClosedTerm(current)) {
      let { term, localEnv } = current;
      // I give these two functions underscores, because they're essentially "private"
      // functions that only `evaluate` uses. It's fine to mentally inline them here.
      _evaluateClosedTerm(term, localEnv, globalEnv, stack, results);
    }
    else {
      // Some part of me feels sad I couldn't just intermix handling the different
      // closed terms of different term type with the different resumption points.
      // But while figuring out how to handle calls and realizing I needed to put closed
      // terms on the stack (not just terms), it also became too hard to handle them
      // all in the same if/else chain. But instead I inserted comments acting like a
      // kind of cross-references between both helper functions.
      _evaluateResumptionPoint(current, stack, results);
    }
  }

  // By construction, there's only one result on the result stack now; we could have
  // written this as `results[0]`
  return results[results.length - 1];
}

function _evaluateClosedTerm(
  term: Term,
  localEnv: Env,
  globalEnv: Env,
  stack: Array<ClosedTerm | After>,   // modified (.push)
  results: Array<Value>,              // modified (.push)
): void {
  if (term instanceof Term.IntConst) {
    results.push(new Value.Int(term.value));
  }
  else if (term instanceof Term.StrConst) {
    results.push(new Value.Str(term.value));
  }
  else if (term instanceof Term.IntAdd) {
    stack.push(new After.IntAddLhs(term.rhs, localEnv));
    stack.push({ term: term.lhs, localEnv });
  }
  // see After.IntAddLhs in _evaluateResumptionPoint
  // see After.IntAddRhs in _evaluateResumptionPoint
  else if (term instanceof Term.StrConcat) {
    stack.push(new After.StrConcatLhs(term.rhs, localEnv));
    stack.push({ term: term.lhs, localEnv });
  }
  // see After.StrConcatLhs in _evaluateResumptionPoint
  // see After.StrConcatRhs in _evaluateResumptionPoint
  else if (term instanceof Term.GetLocal) {
    let value = localEnv.get(term.name);
    if (value === undefined) {
      throw new Error(`Could not find local variable '${term.name}'`);
    }
    results.push(value);
  }
  else if (term instanceof Term.GetGlobal) {
    let value = globalEnv.get(term.name);
    if (value === undefined) {
      throw new Error(`Could not find global variable '${term.name}'`);
    }
    results.push(value);
  }
  else if (term instanceof Term.IfZero) {
    stack.push(new After.IfZeroTestExpr(term.thenExpr, term.elseExpr, localEnv));
    stack.push({ term: term.testExpr, localEnv });
  }
  // see After.IfZeroTestExpr in _evaluateResumptionPoint
  else if (term instanceof Term.CallFunc) {
    stack.push(new After.CallFuncEvalFunc(term.argExprs, localEnv));
    stack.push({ term: term.funcExpr, localEnv });
  }
  // see After.CallFuncEvalFunc in _evaluateResumptionPoint
  // see After.CallFuncNthArg in _evaluateResumptionPoint
  else {
    let _coverageCheck: never = term;
    return _coverageCheck;
  }
}

function _evaluateResumptionPoint(
  resumptionPoint: After,
  stack: Array<ClosedTerm | After>,   // modified (.push)
  results: Array<Value>,              // modified (.pop)
): void {
  // See Term.IntAdd in _evaluateClosedTerm
  if (resumptionPoint instanceof After.IntAddLhs) {
    let localEnv = resumptionPoint.localEnv;
    let lhs = results.pop()!;
    assertInt(lhs);
    stack.push(new After.IntAddRhs(lhs));
    stack.push({ term: resumptionPoint.rhs, localEnv });
  }
  else if (resumptionPoint instanceof After.IntAddRhs) {
    let lhs = resumptionPoint.lhs;
    let rhs = results.pop()!;
    assertInt(rhs);
    results.push(new Value.Int(lhs.payload + rhs.payload));
  }
  // See Term.StrConcat in _evaluateClosedTerm
  else if (resumptionPoint instanceof After.StrConcatLhs) {
    let localEnv = resumptionPoint.localEnv;
    let lhs = results.pop()!;
    assertStr(lhs);
    stack.push(new After.StrConcatRhs(lhs));
    stack.push({ term: resumptionPoint.rhs, localEnv });
  }
  else if (resumptionPoint instanceof After.StrConcatRhs) {
    let lhs = resumptionPoint.lhs;
    let rhs = results.pop()!;
    assertStr(rhs);
    results.push(new Value.Str(lhs.payload + rhs.payload));
  }
  // See Term.IfZero in _evaluateClosedTerm
  else if (resumptionPoint instanceof After.IfZeroTestExpr) {
    let localEnv = resumptionPoint.localEnv;
    let testValue = results.pop()!;
    assertInt(testValue);
    if (testValue.payload === 0n) {
      stack.push({ term: resumptionPoint.thenExpr, localEnv });
    }
    else {
      stack.push({ term: resumptionPoint.elseExpr, localEnv });
    }
  }
  // See Term.CallFunc in _evaluateClosedTerm
  else if (resumptionPoint instanceof After.CallFuncEvalFunc) {
    let localEnv = resumptionPoint.localEnv;
    let funcValue = results.pop()!;
    assertFunc(funcValue);
    let argCount = resumptionPoint.argExprs.length;
    let paramCount = funcValue.parameters.length;
    if (paramCount !== argCount) {
      throw new Error(`${paramCount} parameters vs ${argCount} argument; mismatch`);
    }
    stack.push(
      new After.CallFuncNthArg(
        funcValue,
        0,
        nil(),
        resumptionPoint.argExprs,
        funcValue.parameters,
        localEnv,
      )
    );
  }
  else if (resumptionPoint instanceof After.CallFuncNthArg) {
    let { funcValue, i, args, argExprs, params, localEnv } = resumptionPoint;
    if (i > 0) { // we actually just evaluated an argument
      let arg = results.pop()!;
      args = cons(arg, args);
    }
    let argCount = funcValue.parameters.length; // yes, yes, but they're the same now
    if (i < argCount) {
      // evaluate the next argument
      stack.push(new After.CallFuncNthArg(funcValue, i + 1, args, argExprs, params, localEnv));
      stack.push({ term: argExprs[i], localEnv });
    }
    else { // we're done, ready to call
      let funcEnv: Env = new Map();
      i = argCount - 1;   // re-using `i`, so sue me
      forEach(args, (argValue) => {
        let paramName = params[i--];  // post-decrement, so sue me even harder
        funcEnv.set(paramName, argValue);
      });

      // This is the only one we push with a different local environment -- the one we
      // just created for the called function
      stack.push({ term: funcValue.body, localEnv: funcEnv });
    }
  }
  else {
    let _coverageCheck: never = resumptionPoint;
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
