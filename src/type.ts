import { type DataType, constructors, match, matchMany } from "itsamatch";
import { Expr, type BinaryOp, type Stmt, type UnaryOp } from "./ast";
import { NATIVE_FUNCTIONS } from "./interpret";

export type Type = DataType<{
  Var: { ref: TypeVar };
  Fun: { name: string; args: Type[] };
}>;

export const Type = {
  alpha: () => Type.Var(TypeVar.Generic({ id: 0 })),
  beta: () => Type.Var(TypeVar.Generic({ id: 1 })),
  Var: (ref: TypeVar): Type => ({ variant: "Var", ref }),
  Fun: (name: string, args: Type[]): Type => ({
    variant: "Fun",
    name,
    args,
  }),
  Array: (elem: Type): Type => Type.Fun("Array", [elem]),
  Tuple: (elems: Type[] | Type): Type =>
    Array.isArray(elems)
      ? elems.length === 1
        ? elems[0]
        : Type.Fun("Tuple", [list(elems)])
      : Type.Fun("Tuple", [elems]),
  Function: (args: Type[], ret: Type): Type =>
    Type.Fun("Function", [list(args), ret]),
  Bool: Object.freeze<Type>({ variant: "Fun", name: "Bool", args: [] }),
  Num: Object.freeze<Type>({ variant: "Fun", name: "Num", args: [] }),
  Str: Object.freeze<Type>({ variant: "Fun", name: "Str", args: [] }),
  Nil: Object.freeze<Type>({ variant: "Fun", name: "Nil", args: [] }),
  Cons: (head: Type, tail: Type): Type => Type.Fun("Cons", [head, tail]),
  Unit: Object.freeze<Type>({
    variant: "Fun",
    name: "Tuple",
    args: [{ variant: "Fun", name: "Nil", args: [] }],
  }),
  utils: {
    list,
  },
  eq,
  show,
  fresh,
  generalize,
  instantiate,
};

function isList(ty: Type): boolean {
  return match(ty, {
    Var: () => false,
    Fun: ({ name, args }) => {
      if (name === "Nil") {
        return true;
      }

      return name === "Cons" && isList(args[1]);
    },
  });
}

function isPartialList(ty: Type, startsWithCons = false): boolean {
  return match(ty, {
    Var: () => startsWithCons,
    Fun: ({ name, args }) => {
      if (name === "Nil") {
        return true;
      }

      return name === "Cons" && isPartialList(args[1], true);
    },
  });
}

function unlist(ty: Type): Type[] {
  const elems: Type[] = [];

  while (true) {
    if (ty.variant === "Fun") {
      if (ty.name === "Nil") {
        return elems;
      }

      if (ty.name === "Cons") {
        elems.push(ty.args[0]);
        ty = ty.args[1];
        continue;
      }
    }

    throw new Error(`Expected list, got '${show(ty)}'`);
  }
}

function show(ty: Type): string {
  return match(ty, {
    Var: ({ ref }) => TypeVar.show(ref),
    Fun: ({ name, args }) => {
      switch (name) {
        case "Nil":
          return "[]";
        case "Cons":
          if (isList(args[1])) {
            if (args[1].variant === "Fun" && args[1].name === "Nil") {
              return `[${show(args[0])}]`;
            }

            return `[${show(args[0])}, ${unlist(args[1])
              .map(show)
              .join(", ")}]`;
          }

          return `${show(args[0])}::${show(args[1])}`;
        case "Array":
          return `${show(args[0])}[]`;
        case "Tuple":
          if (!isList(args[0])) {
            return `(${show(args[0])})`;
          }

          return `(${unlist(args[0]).map(show).join(", ")})`;
        case "Function": {
          const ret = args[1];

          if (!isList(args[0])) {
            return `Function<${show(args[0])}, ${show(ret)}>`;
          }

          const params = unlist(args[0]);

          if (params.length === 1) {
            return `${show(params[0])} -> ${show(ret)}`;
          }

          return `(${params.map(show).join(", ")}) -> ${show(ret)}`;
        }
        default:
          if (args.length === 0) {
            return name;
          }

          return `${name}<${args.map(show).join(", ")}>`;
      }
    },
  });
}

function list(elems: Type[], tail: Type = Type.Nil): Type {
  return elems.reduceRight((tail, head) => Type.Cons(head, tail), tail);
}

function eq(a: Type, b: Type): boolean {
  return matchMany([a, b], {
    "Var Var": (a, b) => TypeVar.eq(a.ref, b.ref),
    "Fun Fun": (a, b) =>
      a.name === b.name &&
      a.args.length === b.args.length &&
      a.args.every((arg, i) => eq(arg, b.args[i])),
    _: () => false,
  });
}

export type TypeVarId = number;
export type TypeVar = DataType<{
  Unbound: { id: TypeVarId; name?: string; level: number };
  Generic: { id: TypeVarId; name?: string };
  Param: { name: string; ty?: Type };
  Link: { type: Type };
}>;

export const showTypeVarId = (id: number): string => {
  return (
    String.fromCharCode(97 + (id % 26)) +
    (id >= 26 ? String(Math.floor(id / 26)) : "")
  );
};

const context = {
  typeVarId: 0,
};

export const Context = {
  reset: (): void => {
    context.typeVarId = 0;
  },
  freshTypeVarId: (): number => {
    const id = context.typeVarId;
    context.typeVarId += 1;
    return id;
  },
};

export const TypeVar = {
  ...constructors<TypeVar>().get("Unbound", "Generic", "Param", "Link"),
  show: (v: TypeVar): string =>
    match(v, {
      Unbound: ({ id, name }) => name ?? showTypeVarId(id),
      Generic: ({ id, name }) => name ?? "'" + showTypeVarId(id),
      Param: ({ name }) => `'${name}`,
      Link: ({ type }) => Type.show(type),
    }),
  eq: (a: TypeVar, b: TypeVar) =>
    matchMany([a, b], {
      "Unbound Unbound": (a, b) => a.id === b.id,
      "Generic Generic": (a, b) => a.id === b.id,
      "Link Link": (a, b) => eq(a.type, b.type),
      "Param Param": (a, b) => a.name === b.name,
      _: () => false,
    }),
  fresh: (level: number, name?: string): TypeVar =>
    TypeVar.Unbound({ id: Context.freshTypeVarId(), name, level }),
};

function linkTo(self: { ref: TypeVar }, type: Type): void {
  if (self.ref.variant === "Unbound") {
    if (
      !(
        type.variant === "Var" &&
        type.ref.variant === "Unbound" &&
        type.ref.id === self.ref.id
      )
    ) {
      const deref = unlink(type);
      self.ref = TypeVar.Link({ type: deref });
    }
  }
}

function unlink(ty: Type): Type {
  if (ty.variant === "Var" && ty.ref.variant === "Link") {
    return unlink(ty.ref.type);
  }

  return ty;
}

function fresh(level: number, name?: string): Type {
  return Type.Var(TypeVar.fresh(level, name));
}

type Subst = Map<number, Type>;

const Subst = {
  set: (subst: Subst, id: number, ty: Type): void => {
    if (!occurs(id, ty)) {
      subst.set(id, ty);
    }
  },
};

function generalize(ty: Type, level: number): Type {
  return match(ty, {
    Var: ({ ref }) =>
      match(ref, {
        Unbound: ({ id, level: level2, name }) => {
          if (level2 > level) {
            return Type.Var(TypeVar.Generic({ id, name }));
          }

          return ty;
        },
        Generic: () => ty,
        Param: () => ty,
        Link: ({ type }) => generalize(type, level),
      }),
    Fun: ({ name, args }) =>
      Type.Fun(
        name,
        args.map((arg) => generalize(arg, level))
      ),
  });
}

function instantiate(ty: Type, level: number): { ty: Type; subst: Subst } {
  const subst: Subst = new Map();

  const aux = (ty: Type): Type => {
    return match(ty, {
      Var: ({ ref }) =>
        match(ref, {
          Unbound: () => ty,
          Generic: ({ id, name }) => {
            if (subst.has(id)) {
              return subst.get(id)!;
            }

            const freshTy = Type.fresh(level, name);
            Subst.set(subst, id, freshTy);

            return freshTy;
          },
          Param: ({ name }) => {
            throw new Error(`Unresolved type parameter '${name}'`);
          },
          Link: ({ type }) => aux(type),
        }),
      Fun: ({ name, args }) => Type.Fun(name, args.map(aux)),
    });
  };

  return { ty: aux(ty), subst };
}

function occurs(id: number, ty: Type): boolean {
  return match(ty, {
    Var: ({ ref }) =>
      match(ref, {
        Unbound: ({ id: id2 }) => id === id2,
        Generic: ({ id: id2 }) => id === id2,
        Link: ({ type }) => occurs(id, type),
        Param: () => false,
      }),
    Fun: ({ args }) => args.some((arg) => occurs(id, arg)),
  });
}

function occursCheckAdjustLevels(id: number, level: number, ty: Type): void {
  const go = (t: Type): void => {
    match(t, {
      Var: (v) =>
        match(v.ref, {
          Unbound: ({ id: id2, level: level2 }) => {
            if (id === id2) {
              throw new Error("Recursive type");
            }

            if (level2 > level) {
              v.ref = TypeVar.Unbound({ id: id2, level });
            }
          },
          Generic: () => {
            throw new Error(
              "Generic type variables should not appear during unification"
            );
          },
          Param: ({ name }) => {
            throw new Error(`Unresolved type parameter '${name}'`);
          },
          Link: ({ type }) => go(type),
        }),
      Fun: ({ args }) => args.forEach(go),
    });
  };

  go(ty);
}

function unifyVar(v: { ref: TypeVar }, ty: Type, eqs: [Type, Type][]): void {
  match(v.ref, {
    Unbound: ({ id, level }) => {
      if (
        ty.variant === "Var" &&
        ty.ref.variant === "Unbound" &&
        ty.ref.id === id
      ) {
        throw new Error(
          `There should only be one instance of a particular type variable, but found two instances of '${showTypeVarId(
            id
          )}'.`
        );
      }

      occursCheckAdjustLevels(id, level, ty);
      linkTo(v, ty);
    },
    Generic: () => {
      throw new Error(
        "Generic type variables should not appear during unification"
      );
    },
    Param: ({ name }) => {
      throw new Error(`Unresolved type parameter '${name}'`);
    },
    Link: ({ type }) => {
      eqs.push([type, ty]);
    },
  });
}

// returns true if unification succeeded
function unify(a: Type, b: Type): boolean {
  const eqs: [Type, Type][] = [[a, b]];

  while (eqs.length > 0) {
    const [s, t] = eqs.pop()!.map(unlink);
    // console.log(`unify '${show(s)}' with '${show(t)}'`);

    if (Type.eq(s, t)) {
      continue;
    }

    if (s.variant === "Var") {
      unifyVar(s, t, eqs);
    } else if (t.variant === "Var") {
      unifyVar(t, s, eqs);
    } else if (s.variant === "Fun" && t.variant === "Fun") {
      if (s.name !== t.name || s.args.length !== t.args.length) {
        return false;
      }

      for (let i = 0; i < s.args.length; i++) {
        eqs.push([s.args[i], t.args[i]]);
      }
    } else {
      return false;
    }
  }

  return true;
}

type VarInfo = { mut: boolean; ty: Type };

class Scope<T> {
  members = new Map<string, T>();
  parent?: Scope<T>;

  constructor(parent?: Scope<T>) {
    this.parent = parent;
  }

  lookup(name: string): T | undefined {
    return this.members.get(name) ?? this.parent?.lookup(name);
  }

  declare(name: string, value: T): void {
    if (this.members.has(name)) {
      throw new Error(`Variable ${name} already declared`);
    }

    this.members.set(name, value);
  }
}

export class TypeEnv {
  variables: Scope<VarInfo>;
  parent?: TypeEnv;
  letLevel: number;
  functionStack: { ty: Type }[];

  constructor(parent?: TypeEnv) {
    this.parent = parent;
    this.variables = new Scope(parent?.variables);
    this.letLevel = parent?.letLevel ?? 0;
    this.functionStack = [...(parent?.functionStack ?? [])];

    if (parent === undefined) {
      for (const fn of NATIVE_FUNCTIONS) {
        const funTy = Type.Function(fn.signature.args, fn.signature.ret);
        this.variables.declare(fn.name, { mut: false, ty: funTy });
      }
    }
  }

  child(): TypeEnv {
    return new TypeEnv(this);
  }

  private freshType(): Type {
    return Type.fresh(this.letLevel);
  }

  private unify(a: Type, b: Type): void {
    if (!unify(a, b)) {
      throw new Error(`Cannot unify '${show(a)}' with '${show(b)}'`);
    }
  }

  inferExpr(expr: Expr): Type {
    const ty = match(expr, {
      Literal: ({ literal }) =>
        match(literal, {
          Unit: () => Type.Unit,
          Bool: () => Type.Bool,
          Num: () => Type.Num,
          Str: () => Type.Str,
        }),
      Variable: ({ name }) => {
        const info = this.variables.lookup(name);
        if (!info) {
          throw new Error(`Variable '${name}' not found`);
        }

        const instTy = instantiate(info.ty, this.letLevel).ty;
        return instTy;
      },
      Unary: ({ op, expr }) => {
        const exprTy = this.inferExpr(expr);
        const UNARY_OP_TYPE: Record<UnaryOp, Type> = {
          "!": Type.Bool,
          "-": Type.Num,
          "+": Type.Num,
        };

        this.unify(exprTy, UNARY_OP_TYPE[op]);

        return exprTy;
      },
      Binary: ({ lhs, op, rhs }) => {
        const lhsTy = this.inferExpr(lhs);
        const rhsTy = this.inferExpr(rhs);

        const BINARY_OP_TYPE: Record<BinaryOp, [Type, Type, Type]> = {
          "+": [Type.Num, Type.Num, Type.Num],
          "-": [Type.Num, Type.Num, Type.Num],
          "*": [Type.Num, Type.Num, Type.Num],
          "/": [Type.Num, Type.Num, Type.Num],
          mod: [Type.Num, Type.Num, Type.Num],
          "**": [Type.Num, Type.Num, Type.Num],
          "==": [Type.alpha(), Type.alpha(), Type.Bool],
          "!=": [Type.alpha(), Type.alpha(), Type.Bool],
          "<": [Type.Num, Type.Num, Type.Bool],
          ">": [Type.Num, Type.Num, Type.Bool],
          "<=": [Type.Num, Type.Num, Type.Bool],
          ">=": [Type.Num, Type.Num, Type.Bool],
          and: [Type.Bool, Type.Bool, Type.Bool],
          or: [Type.Bool, Type.Bool, Type.Bool],
          "&": [Type.Num, Type.Num, Type.Num],
          "|": [Type.Num, Type.Num, Type.Num],
          "++": [Type.Str, Type.Str, Type.Str],
        };

        const [arg1, arg2, ret] = BINARY_OP_TYPE[op];
        const funTy = Type.instantiate(
          Type.Function([arg1, arg2], ret),
          this.letLevel
        ).ty;

        this.unify(funTy, Type.Function([lhsTy, rhsTy], ret));

        return ret;
      },
      Block: ({ stmts, ret }) => {
        const blockEnv = this.child();

        for (const stmt of stmts) {
          blockEnv.inferStmt(stmt);
        }

        if (ret) {
          return blockEnv.inferExpr(ret);
        } else {
          return Type.Unit;
        }
      },
      Array: ({ elems }) => {
        const elemTy = this.freshType();

        elems.forEach((elem) => {
          this.unify(this.inferExpr(elem), elemTy);
        });

        return Type.Array(elemTy);
      },
      Tuple: ({ elems }) => {
        return Type.Tuple(elems.map((elem) => this.inferExpr(elem)));
      },
      _: () => {
        throw new Error("Not implemented");
      },
      If: ({ cond, then, otherwise }) => {
        const condTy = this.inferExpr(cond);
        this.unify(condTy, Type.Bool);

        const thenTy = this.inferExpr(then);

        if (otherwise) {
          const otherwiseTy = this.inferExpr(otherwise);
          this.unify(thenTy, otherwiseTy);
        } else {
          this.unify(thenTy, Type.Unit);
        }

        return thenTy;
      },
      Fun: ({ args, ret, body }) => {
        const funEnv = this.child();
        const argTys = args.map((arg) => arg.ann ?? this.freshType());
        const returnTy = ret ?? this.freshType();
        const retTyInfo = { ty: returnTy };
        funEnv.functionStack.push(retTyInfo);

        for (let i = 0; i < args.length; i += 1) {
          funEnv.variables.declare(args[i].name, { mut: false, ty: argTys[i] });
        }

        const bodyTy = funEnv.inferExpr(body);
        funEnv.functionStack.pop();

        this.unify(bodyTy, returnTy);

        return Type.Function(argTys, returnTy);
      },
      Call: ({ fun, args }) => {
        const funTy = this.inferExpr(fun);
        const argTys = args.map((arg) => this.inferExpr(arg));
        const retTy = this.freshType();
        const expectedFunTy = Type.Function(argTys, retTy);

        this.unify(funTy, expectedFunTy);

        return retTy;
      },
      ArrayAccess: ({ arr, index }) => {
        const arrTy = this.inferExpr(arr);
        const indexTy = this.inferExpr(index);
        const elemTy = this.freshType();

        this.unify(arrTy, Type.Array(elemTy));
        this.unify(indexTy, Type.Num);

        return elemTy;
      },
      TupleAccess: ({ tuple, index }) => {
        const lhsTy = this.inferExpr(tuple);
        const elems: Type[] = [];

        for (let i = 0; i <= index; i += 1) {
          const elem = this.freshType();
          elems.push(elem);
        }

        const tail = this.freshType();
        const expectedLhsTy = Type.Tuple(Type.utils.list(elems, tail));
        this.unify(lhsTy, expectedLhsTy);
        return elems[index];
      },
    });

    expr.ty = ty;
    return ty;
  }

  inferStmt(stmt: Stmt): void {
    match(stmt, {
      Expr: ({ expr }) => {
        this.inferExpr(expr);
      },
      Let: ({ mut, name, ann, value }) => {
        this.letLevel += 1;
        const rhsTy = this.inferExpr(value);
        this.letLevel -= 1;

        if (ann) {
          this.unify(rhsTy, ann);
        }

        const genTy = mut ? rhsTy : generalize(rhsTy, this.letLevel);
        this.variables.declare(name, { mut, ty: genTy });
      },
      Assign: ({ lhs, op, rhs }) => {
        const lhsTy = this.inferExpr(lhs);
        const rhsTy = this.inferExpr(rhs);

        switch (op) {
          case "=":
            this.unify(lhsTy, rhsTy);
            break;
          case "+=":
          case "-=":
            this.unify(lhsTy, Type.Num);
            this.unify(rhsTy, Type.Num);
            break;
        }
      },
      While: ({ cond, body }) => {
        const condTy = this.inferExpr(cond);
        this.unify(condTy, Type.Bool);

        const bodyEnv = this.child();
        for (const stmt of body) {
          bodyEnv.inferStmt(stmt);
        }
      },
      For: ({ name, iter, body }) => {
        const iterTy = this.inferExpr(iter);
        const elemTy = this.freshType();
        this.unify(iterTy, Type.Array(elemTy));

        const bodyEnv = this.child();
        bodyEnv.variables.declare(name, { mut: false, ty: elemTy });
        for (const stmt of body) {
          bodyEnv.inferStmt(stmt);
        }
      },
      Break: () => {},
      Return: ({ expr }) => {
        if (this.functionStack.length === 0) {
          throw new Error("Return statement used outside of a function body");
        }

        const funReturnTy =
          this.functionStack[this.functionStack.length - 1].ty;
        const exprTy = this.inferExpr(expr);
        this.unify(funReturnTy, exprTy);
      },
    });
  }
}
