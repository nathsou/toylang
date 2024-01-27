import { constructors, match, type DataType, matchMany } from "itsamatch";
import { Expr, type Stmt } from "./ast";

export type Value = DataType<{
  Unit: {};
  Bool: boolean;
  Num: number;
  Str: string;
  Tuple: { elems: Value[] };
  Array: { elems: Value[] };
  Fun: { args: string[]; body: Expr };
  NativeFun: NativeFunction;
}>;

function assert(cond: boolean, message?: string): asserts cond {
  if (!cond) {
    throw new Error(message ?? "Assertion failed");
  }
}

function elemsEq(as: Value[], bs: Value[]): boolean {
  if (as.length !== bs.length) {
    return false;
  }

  for (let i = 0; i < as.length; i++) {
    if (!Value.eq(as[i], bs[i])) {
      return false;
    }
  }

  return true;
}

export const Value = {
  Unit: Object.freeze<Value>({ variant: "Unit" }),
  ...constructors<Value>().get(
    "Bool",
    "Num",
    "Str",
    "Tuple",
    "Array",
    "Fun",
    "NativeFun"
  ),
  eq: (lhs: Value, rhs: Value): boolean => {
    return matchMany([lhs, rhs], {
      "Unit Unit": () => true,
      "Bool Bool": (a, b) => a === b,
      "Num Num": (a, b) => a === b,
      "Str Str": (a, b) => a === b,
      "Tuple Tuple": (a, b) => elemsEq(a.elems, b.elems),
      "Array Array": (a, b) => elemsEq(a.elems, b.elems),
      "NativeFun NativeFun": (a, b) => a.name === b.name,
      "Fun Fun": () => false,
      _: () => false,
    });
  },
  show: (value: Value): string => {
    return match(value, {
      Unit: () => "()",
      Bool: (bool) => (bool ? "true" : "false"),
      Num: (num) => num.toString(),
      Str: (str) => '"' + str + '"',
      Tuple: ({ elems }) => `(${elems.map(Value.show).join(", ")})`,
      Array: ({ elems }) => `[${elems.map(Value.show).join(", ")}]`,
      Fun: () => "<fun>",
      NativeFun: (fun) => `<@${fun.name}>`,
    });
  },
};

type NativeFunction = {
  name: string;
  arity: number;
  run: (...args: Value[]) => Value;
};

const NATIVE_FUNCTIONS: NativeFunction[] = [
  {
    name: "print",
    arity: -1,
    run: (...values: Value[]) => {
      for (const value of values) {
        process.stdout.write(Value.show(value));
        process.stdout.write(" ");
      }

      process.stdout.write("\n");

      return Value.Unit;
    },
  },
  {
    name: "str",
    arity: 1,
    run: (value: Value) => Value.Str(Value.show(value)),
  },
  {
    name: "len",
    arity: 1,
    run: (value: Value) => {
      switch (value.variant) {
        case "Array":
        case "Tuple":
          return Value.Num(value.elems.length);
        case "Str":
          return Value.Num(value.$value.length);
        default:
          throw new Error("len expects an array or tuple or string");
      }
    },
  },
  {
    name: "append",
    arity: 2,
    run: (array: Value, value: Value) => {
      assert(array.variant === "Array", "append expects an array");
      array.elems.push(value);
      return array;
    },
  },
  {
    name: "at",
    arity: 2,
    run: (array: Value, index: Value) => {
      assert(index.variant === "Num", "at expects a number");

      switch (array.variant) {
        case "Array":
          if (index.$value < 0 || index.$value >= array.elems.length) {
            throw new Error("Index out of bounds");
          }

          return array.elems[index.$value];
        case "Tuple":
          if (index.$value < 0 || index.$value >= array.elems.length) {
            throw new Error("Index out of bounds");
          }

          return array.elems[index.$value];
        case "Str":
          if (index.$value < 0 || index.$value >= array.$value.length) {
            throw new Error("Index out of bounds");
          }

          return Value.Str(array.$value[index.$value]);
        default:
          throw new Error("at expects an array or tuple or string");
      }
    },
  },
  {
    name: "array_set",
    arity: 3,
    run: (array: Value, index: Value, value: Value) => {
      assert(index.variant === "Num", "array_set expects a number");

      switch (array.variant) {
        case "Array":
          if (index.$value < 0 || index.$value >= array.elems.length) {
            throw new Error("Index out of bounds");
          }

          array.elems[index.$value] = value;
          return array;
        default:
          throw new Error("array_set expects an array");
      }
    },
  },
  {
    name: "ord",
    arity: 1,
    run: (str: Value) => {
      assert(
        str.variant === "Str" && str.$value.length === 1,
        "ord expects a single-character string"
      );

      return Value.Num(str.$value.charCodeAt(0));
    },
  },
];

export class Env {
  private global: Env;
  private parent?: Env;
  private members: Map<string, Value> = new Map();
  private returnValue: Value | undefined;

  constructor(parent?: Env) {
    this.parent = parent;
    this.global = parent?.global ?? this;

    // declare native functions in the global scope
    if (parent === undefined) {
      for (const fun of NATIVE_FUNCTIONS) {
        this.declare(fun.name, Value.NativeFun(fun));
      }
    }
  }

  public child(): Env {
    return new Env(this);
  }

  public declare(name: string, value: Value): void {
    if (this.members.has(name)) {
      throw new Error(`Variable ${name} already declared`);
    }

    this.members.set(name, value);
  }

  public lookup(name: string): Value | undefined {
    return this.members.get(name) ?? this.parent?.lookup(name);
  }

  public assign(name: string, value: Value): void {
    if (this.members.has(name)) {
      this.members.set(name, value);
    } else if (this.parent) {
      this.parent.assign(name, value);
    } else {
      throw new Error(`Undefined variable ${name}`);
    }
  }

  public evalExpr(expr: Expr): Value {
    return match(expr, {
      Literal: ({ literal }) =>
        match(literal, {
          Unit: () => Value.Unit,
          Bool: (bool) => Value.Bool(bool),
          Num: (num) => Value.Num(num),
          Str: (str) => Value.Str(str),
        }),
      Variable: ({ name }) => {
        const value = this.lookup(name);

        if (value === undefined) {
          throw new Error(`Undefined variable ${name}`);
        }

        return value;
      },
      Unary: ({ op, expr }) => {
        const value = this.evalExpr(expr);
        switch (op) {
          case "+":
            assert(value.variant === "Num");
            return this.evalExpr(expr);
          case "-":
            assert(value.variant === "Num");
            return Value.Num(-value.$value);
          case "!":
            assert(value.variant === "Bool");
            return Value.Bool(!value.$value);
        }
      },
      Binary: ({ lhs, op, rhs }) => {
        const lhsValue = this.evalExpr(lhs);
        const rhsValue = this.evalExpr(rhs);

        switch (op) {
          case "+":
            assert(lhsValue.variant === "Num");
            assert(rhsValue.variant === "Num");
            return Value.Num(lhsValue.$value + rhsValue.$value);
          case "-":
            assert(lhsValue.variant === "Num");
            assert(rhsValue.variant === "Num");
            return Value.Num(lhsValue.$value - rhsValue.$value);
          case "*":
            assert(lhsValue.variant === "Num");
            assert(rhsValue.variant === "Num");
            return Value.Num(lhsValue.$value * rhsValue.$value);
          case "/":
            assert(lhsValue.variant === "Num");
            assert(rhsValue.variant === "Num");
            return Value.Num(lhsValue.$value / rhsValue.$value);
          case "mod":
            assert(lhsValue.variant === "Num");
            assert(rhsValue.variant === "Num");
            return Value.Num(lhsValue.$value % rhsValue.$value);
          case "<":
            assert(lhsValue.variant === "Num");
            assert(rhsValue.variant === "Num");
            return Value.Bool(lhsValue.$value < rhsValue.$value);
          case "<=":
            assert(lhsValue.variant === "Num");
            assert(rhsValue.variant === "Num");
            return Value.Bool(lhsValue.$value <= rhsValue.$value);
          case ">":
            assert(lhsValue.variant === "Num");
            assert(rhsValue.variant === "Num");
            return Value.Bool(lhsValue.$value > rhsValue.$value);
          case ">=":
            assert(lhsValue.variant === "Num");
            assert(rhsValue.variant === "Num");
            return Value.Bool(lhsValue.$value >= rhsValue.$value);
          case "==":
            return Value.Bool(Value.eq(lhsValue, rhsValue));
          case "!=":
            return Value.Bool(!Value.eq(lhsValue, rhsValue));
          case "and":
            assert(lhsValue.variant === "Bool");
            assert(rhsValue.variant === "Bool");
            return Value.Bool(lhsValue.$value && rhsValue.$value);
          case "or":
            assert(lhsValue.variant === "Bool");
            assert(rhsValue.variant === "Bool");
            return Value.Bool(lhsValue.$value || rhsValue.$value);
          case "&":
            assert(lhsValue.variant === "Num");
            assert(rhsValue.variant === "Num");
            return Value.Num(lhsValue.$value & rhsValue.$value);
          case "|":
            assert(lhsValue.variant === "Num");
            assert(rhsValue.variant === "Num");
            return Value.Num(lhsValue.$value | rhsValue.$value);
          case "**":
            assert(lhsValue.variant === "Num");
            assert(rhsValue.variant === "Num");
            return Value.Num(lhsValue.$value ** rhsValue.$value);
          case "++":
            assert(lhsValue.variant === "Str");
            assert(rhsValue.variant === "Str");
            return Value.Str(lhsValue.$value + rhsValue.$value);
        }
      },
      Block: ({ stmts, ret }) => {
        const blockEnv = this.child();

        for (const stmt of stmts) {
          if (blockEnv.returnValue) {
            this.returnValue = blockEnv.returnValue;
            return blockEnv.returnValue;
          }

          blockEnv.evalStmt(stmt);
        }

        if (blockEnv.returnValue) {
          this.returnValue = blockEnv.returnValue;
          return blockEnv.returnValue;
        }

        if (ret) {
          return blockEnv.evalExpr(ret);
        } else {
          return Value.Unit;
        }
      },
      If: ({ cond, then, otherwise }) => {
        const condValue = this.evalExpr(cond);
        assert(condValue.variant === "Bool");

        if (condValue.$value) {
          const thenEnv = this.child();
          const ret = thenEnv.evalExpr(then);
          this.returnValue = thenEnv.returnValue;
          return ret;
        } else if (otherwise) {
          const otherwiseEnv = this.child();
          const ret = otherwiseEnv.evalExpr(otherwise);
          this.returnValue = otherwiseEnv.returnValue;
          return ret;
        }

        return Value.Unit;
      },
      Tuple: ({ elems }) => {
        return Value.Tuple({ elems: elems.map((elem) => this.evalExpr(elem)) });
      },
      Array: ({ elems }) => {
        return Value.Array({ elems: elems.map((elem) => this.evalExpr(elem)) });
      },
      Fun: ({ args, body }) => {
        return Value.Fun({ args: args.map((arg) => arg.name), body });
      },
      Call: ({ fun, args }) => {
        const funValue = this.evalExpr(fun);
        assert(
          funValue.variant === "Fun" || funValue.variant === "NativeFun",
          "lhs of call expression must be a function"
        );

        const arity =
          funValue.variant === "Fun" ? funValue.args.length : funValue.arity;

        assert(
          arity === -1 || args.length === arity,
          "wrong number of arguments in function call"
        );

        if (funValue.variant === "NativeFun") {
          return funValue.run(...args.map((arg) => this.evalExpr(arg)));
        }

        const childEnv = this.child();

        funValue.args.forEach((name, index) => {
          childEnv.declare(name, this.evalExpr(args[index]));
        });

        return childEnv.evalExpr(funValue.body);
      },
    });
  }

  public evalStmt(stmt: Stmt): void {
    match(stmt, {
      Expr: ({ expr }) => {
        this.evalExpr(expr);
      },
      Let: ({ name, value }) => {
        this.declare(name, this.evalExpr(value));
      },
      Assign: ({ lhs, op, rhs }) => {
        assert(
          lhs.variant === "Variable",
          "lhs of assignment must be a variable"
        );

        switch (op) {
          case "=":
            this.assign(lhs.name, this.evalExpr(rhs));
            break;
          case "+=":
            this.assign(
              lhs.name,
              this.evalExpr(
                Expr.Binary({
                  lhs,
                  op: "+",
                  rhs,
                })
              )
            );
            break;
          case "-=":
            this.assign(
              lhs.name,
              this.evalExpr(
                Expr.Binary({
                  lhs,
                  op: "-",
                  rhs,
                })
              )
            );
            break;
        }
      },
      While: ({ cond, body }) => {
        const childEnv = this.child();

        while (true) {
          const condValue = this.evalExpr(cond);
          assert(condValue.variant === "Bool");

          if (!condValue.$value) {
            break;
          }

          for (const stmt of body) {
            childEnv.evalStmt(stmt);

            if (childEnv.returnValue) {
              this.returnValue = childEnv.returnValue;
              return;
            }
          }

          childEnv.members.clear();
        }
      },
      For: ({ name, iter, body }) => {
        const childEnv = this.child();
        const iterValue = this.evalExpr(iter);
        let iterator: Iterable<Value>;

        switch (iterValue.variant) {
          case "Array":
          case "Tuple":
            iterator = iterValue.elems;
            break;
          case "Str":
            function* strIterator(str: string): Iterable<Value> {
              for (let i = 0; i < str.length; i += 1) {
                yield Value.Str(str[i]);
              }
            }

            iterator = strIterator(iterValue.$value);
            break;
          default:
            throw new Error("for loop expects an array or tuple or string");
        }

        for (const value of iterator) {
          childEnv.declare(name, value);

          for (const stmt of body) {
            childEnv.evalStmt(stmt);

            if (childEnv.returnValue) {
              this.returnValue = childEnv.returnValue;
              return;
            }
          }

          childEnv.members.clear();
        }
      },
      Break: () => {
        throw new Error("break used outside while statement");
      },
      Return: ({ expr }) => {
        this.returnValue = this.evalExpr(expr);
      },
    });
  }
}
