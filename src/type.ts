import { type DataType, constructors, match, matchMany } from "itsamatch";
import { Expr, type BinaryOp, type Stmt, type UnaryOp } from "./ast";
import { NATIVE_FUNCTIONS } from "./interpret";

export type Type = DataType<{
  Var: { ref: TypeVar };
  Fun: { name: string; args: Type[] };
  Record: { row: Row };
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
  Record: (row: Row): Type => ({ variant: "Record", row }),
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

export type TypeVarId = number;
export type TypeVar = DataType<{
  Unbound: { id: TypeVarId; level: number };
  Generic: { id: TypeVarId };
  Param: { name: string };
  Link: { type: Type };
}>;

export const TypeVar = {
  ...constructors<TypeVar>().get("Unbound", "Generic", "Param", "Link"),
  show: (v: TypeVar): string =>
    match(v, {
      Unbound: ({ id }) => "!" + showTypeVarId(id),
      Generic: ({ id }) => showTypeVarId(id),
      Param: ({ name }) => `%${name}`,
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
  fresh: (level: number): TypeVar =>
    TypeVar.Unbound({ id: Context.freshTypeVarId(), level }),
};

export type Row = DataType<{
  Empty: {};
  Extend: { name: string; ty: Type; tail: Type };
}>;

export const Row = {
  Empty: (): Row => ({ variant: "Empty" }),
  Extend: (name: string, ty: Type, tail: Type): Row =>
    ({ variant: "Extend", name, ty, tail }),
};

const rowFromEntries = (entries: [string, Type][], tail: Type): Row => {  
  let row: Row = Row.Empty();

  entries.toSorted((a, b) => a[0].localeCompare(b[0])).forEach(([name, ty], i) => {
    const rest = i === 0 ? tail : Type.Record(row);
    row = Row.Extend(name, ty, rest);
  });

  return row;
};

const rowEntries = (row: Row): [string, Type][] => {
  const entries: [string, Type][] = [];

  function aux(row: Row): void {
    match(row, {
      Empty: () => {},
      Extend: ({ name, ty, tail }) => {
        entries.push([name, ty]);
        match(unlink(tail), {
          Record: ({ row }) => aux(row),
          _: () => {},
        });
      },
    });
  }

  aux(row);
  
  return entries;
};

const rowMap = (row: Row, f: (ty: Type) => Type, tail: Type): Row => {
  const entries = rowEntries(row);
  return rowFromEntries(entries.map(([name, ty]) => [name, f(ty)]), tail);
};

function isList(ty: Type): boolean {
  return match(ty, {
    Fun: ({ name, args }) => {
      if (name === "Nil") {
        return true;
      }

      return name === "Cons" && isList(args[1]);
    },
    _: () => false,
  });
}

function showPartialList(ty: Type): string {
  const elems: string[] = [];
  ty = unlink(ty);

  while (true) {
    if (ty.variant === "Fun") {
      if (ty.name === "Nil") {
        return elems.join(", ");
      }

      if (ty.name === "Cons") {
        elems.push(show(ty.args[0]));
        ty = unlink(ty.args[1]);
        continue;
      }
    }

    return [...elems, "..."].join(", ");
  }
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

function list(elems: Type[], tail: Type = Type.Nil): Type {
  return elems.reduceRight((tail, head) => Type.Cons(head, tail), tail);
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
          return `(${showPartialList(args[0])})`;
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
    Record: ({ row }) => {
      const entries = rowEntries(row).sort(([a], [b]) => a.localeCompare(b));
      if (entries.length === 0) {
        return "{}";
      }

      return `{ ${entries.map(([name, ty]) => `${name}: ${show(ty)}`).join(", ")} }`;
    },
  });
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

function linkTo(self: { ref: TypeVar }, type: Type, env: TypeEnv): void {
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

function fresh(level: number): Type {
  return Type.Var(TypeVar.fresh(level));
}

type Subst = Map<number, Type>;

const Subst = {
  set: (subst: Subst, id: number, ty: Type): void => {
    if (!occurs(id, ty)) {
      subst.set(id, ty);
    }
  },
};

function generalize(ty: Type, env: TypeEnv): Type {
  const generalizedParams = new Map<string, Type>();

  const aux = (ty: Type): Type => {
    return match(ty, {
      Var: ({ ref }) =>
        match(ref, {
          Unbound: ({ id, level: level2 }) => {
            if (level2 > env.letLevel) {
              return Type.Var(TypeVar.Generic({ id }));
            }

            return ty;
          },
          Generic: () => ty,
          Param: ({ name }) => {
            if (generalizedParams.has(name)) {
              return generalizedParams.get(name)!;
            }

            const paramTy = env.generics.lookup(name);

            if (paramTy) {
              const genTy = aux(paramTy);
              generalizedParams.set(name, genTy);
              return genTy;
            }

            throw new Error(`generalize: Unresolved type parameter '${name}'`);
          },
          Link: ({ type }) => aux(type),
        }),
      Fun: ({ name, args }) =>
        Type.Fun(
          name,
          args.map((arg) => aux(arg))
        ),
      Record: ({ row }) => {
        return match(row, {
          Extend: ({ name, ty, tail }) => Type.Record(Row.Extend(name, aux(ty), aux(tail))),
          _: () => ty,
        });
      },
    });
  };

  const gen = aux(ty);
  return gen;
}

function instantiate(ty: Type, env: TypeEnv): { ty: Type; subst: Subst } {
  const subst: Subst = new Map();

  const aux = (ty: Type): Type => {
    return match(ty, {
      Var: ({ ref }) =>
        match(ref, {
          Unbound: () => ty,
          Generic: ({ id }) => {
            if (subst.has(id)) {
              return subst.get(id)!;
            }

            const freshTy = env.freshType();
            Subst.set(subst, id, freshTy);

            return freshTy;
          },
          Param: ({ name }) => {
            const paramTy = env.generics.lookup(name);

            if (paramTy) {
              return aux(paramTy);
            } else {
              throw new Error(
                `instantiate: Unresolved type parameter '${name}'`
              );
            }
          },
          Link: ({ type }) => aux(type),
        }),
      Fun: ({ name, args }) => Type.Fun(name, args.map(aux)),
      Record: ({ row }) => {
        return match(row, {
          Empty: () => ty,
          Extend: ({ name, ty, tail }) =>
            Type.Record(Row.Extend(name, aux(ty), aux(tail))),
        });
      },
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
    Record: ({ row }) => rowEntries(row).some(([_, fieldTy]) => occurs(id, fieldTy)),
  });
}

function occursCheckAdjustLevels(id: number, ty: Type, env: TypeEnv): void {
  const aux = (t: Type): void => {
    match(t, {
      Var: (v) =>
        match(v.ref, {
          Unbound: ({ id: id2, level: level2 }) => {
            if (id === id2) {
              throw new Error("Recursive type");
            }

            if (level2 > env.letLevel) {
              v.ref = TypeVar.Unbound({ id: id2, level: env.letLevel });
            }
          },
          Generic: () => {
            throw new Error(
              "Generic type variables should not appear during unification"
            );
          },
          Param: ({ name }) => {
            const paramTy = env.generics.lookup(name);

            if (paramTy) {
              aux(paramTy);
            } else {
              throw new Error(
                `occursCheck: Unresolved type parameter '${name}'`
              );
            }
          },
          Link: ({ type }) => aux(type),
        }),
      Fun: ({ args }) => args.forEach(aux),
      Record: ({ row }) => rowEntries(row).forEach(([_, fieldTy]) => aux(fieldTy)),
    });
  };

  aux(ty);
}

function unifyVar(
  v: { ref: TypeVar },
  ty: Type,
  eqs: [Type, Type][],
  env: TypeEnv
): void {
  match(v.ref, {
    Unbound: ({ id }) => {
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

      occursCheckAdjustLevels(id, ty, env);
      linkTo(v, ty, env);
    },
    Generic: () => {
      throw new Error(
        "Generic type variables should not appear during unification"
      );
    },
    Param: ({ name }) => {
      throw new Error(`unifyVar: Unresolved type parameter '${name}'`);
    },
    Link: ({ type }) => {
      eqs.push([type, ty]);
    },
  });
}

// returns true if unification succeeded
function unify(a: Type, b: Type, env: TypeEnv): boolean {
  const eqs: [Type, Type][] = [[a, b]];

  while (eqs.length > 0) {
    const [s, t] = eqs.pop()!.map(unlink);
    // console.log(`unify '${show(s)}' with '${show(t)}'`);

    if (Type.eq(s, t)) {
      continue;
    }

    console.log(`unify '${show(s)}' with '${show(t)}'`);

    if (s.variant === "Var") {
      unifyVar(s, t, eqs, env);
    } else if (t.variant === "Var") {
      unifyVar(t, s, eqs, env);
    } else if (s.variant === "Fun" && t.variant === "Fun") {
      if (s.name !== t.name || s.args.length !== t.args.length) {
        return false;
      }

      for (let i = 0; i < s.args.length; i++) {
        eqs.push([s.args[i], t.args[i]]);
      }
    } else if (s.variant === 'Record' && t.variant === 'Record') {
      if (s.row.variant === 'Empty' && t.row.variant === 'Empty') {
        // nothing to do
      } else if (s.row.variant === 'Extend' && t.row.variant === 'Extend') {
        const { name: name1, ty: ty1, tail: tail1 } = s.row;
        let isTailUnbound = false;

        if (tail1.variant === 'Var') {
          if (tail1.ref.variant === 'Unbound') {
            isTailUnbound = true;
          }
        }

        const tail2 = rewriteRow(t, name1, ty1, env);

        if (isTailUnbound) {
          if (tail1.variant === 'Var') {
            if (tail1.ref.variant === 'Link') {
              throw new Error("Recursive row types");
            }
          }
        }

        eqs.push([tail1, tail2]);
      }
    } else {
      return false;
    }
  }

  return true;
}

function rewriteRow(row2: Type, field1: string, ty1: Type, env: TypeEnv): Type {
  return match(row2, {
    Record: ({ row }) => {
      return match(row, {
        Empty: () => {
          throw new Error(`Record does not contain field '${field1}'`);
        },
        Extend: ({ name: name2, ty: ty2, tail: tail2 }) => {
          if (name2 === field1) {
            env.unify(ty1, ty2);
            return tail2;
          }

          return Type.Record(Row.Extend(name2, ty2, rewriteRow(tail2, field1, ty1, env)));
        },
      });
    },
    Var: v => {
      return match(v.ref, {
        Unbound: ({ level }) => {
          const tail2 = Type.fresh(level);
          const row2 = Row.Extend(field1, ty1, tail2);
          linkTo(v, Type.Record(row2), env);
          return tail2;
        },
        Link: ({ type }) => rewriteRow(type, field1, ty1, env),
        _: () => {
          throw new Error("Expected record type");
        },
      });
    },
    _: () => {
      throw new Error("Expected record type");
    },
  });
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
  generics: Scope<Type>;
  parent?: TypeEnv;
  letLevel: number;
  functionStack: { ty: Type }[];

  constructor(parent?: TypeEnv) {
    this.parent = parent;
    this.variables = new Scope(parent?.variables);
    this.generics = new Scope(parent?.generics);
    this.letLevel = parent?.letLevel ?? 0;
    this.functionStack = [...(parent?.functionStack ?? [])];

    if (parent === undefined) {
      for (const { name, signature } of NATIVE_FUNCTIONS) {
        const funTy = Type.Function(signature.args, signature.ret);
        this.variables.declare(name, { mut: false, ty: funTy });
      }
    }
  }

  child(): TypeEnv {
    return new TypeEnv(this);
  }

  freshType(): Type {
    return Type.fresh(this.letLevel);
  }

  unify(a: Type, b: Type): void {
    if (!unify(a, b, this)) {
      throw new Error(`Cannot unify '${show(a)}' with '${show(b)}'`);
    }
  }

  instantiate(ty: Type): Type {
    return instantiate(ty, this).ty;
  }

  generalize(ty: Type): Type {
    return generalize(ty, this);
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

        return this.instantiate(info.ty);
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
        const funTy = this.instantiate(Type.Function([arg1, arg2], ret));
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
      Record: ({ entries }) => {
        return Type.Record(rowFromEntries(
          entries.map(({ name, value }) => [name, this.inferExpr(value)]),
          this.freshType(),
        ));
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
      Fun: ({ generics, args, ret, body }) => {
        const funEnv = this.child();

        for (const generic of generics) {
          funEnv.generics.declare(generic, funEnv.freshType());
        }

        const argTys = args.map((arg) => arg.ann ?? funEnv.freshType());
        const returnTy = ret ?? funEnv.freshType();
        const retTyInfo = { ty: returnTy };
        funEnv.functionStack.push(retTyInfo);

        for (let i = 0; i < args.length; i += 1) {
          funEnv.variables.declare(args[i].name, { mut: false, ty: argTys[i] });
        }

        const bodyTy = funEnv.inferExpr(body);
        funEnv.functionStack.pop();

        funEnv.unify(bodyTy, returnTy);

        // ensure type parameters are replaced by their bindings
        return funEnv.instantiate(Type.Function(argTys, returnTy));
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
      RecordAccess: ({ record, name }) => {
        const fieldTy = expr.ty ?? this.freshType();
        const tail = this.freshType();
        const partialRecordTy = Type.Record(Row.Extend(name, fieldTy, tail));
        const recordTy = this.inferExpr(record);
        console.log(show(recordTy), show(partialRecordTy), name);
        this.unify(recordTy, partialRecordTy);
        console.log('final record ty', show(recordTy));
        return fieldTy;
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
        const letEnv = this.child();
        letEnv.letLevel += 1;
        const rhsTy = letEnv.inferExpr(value);
        letEnv.letLevel -= 1;

        if (ann) {
          letEnv.unify(rhsTy, ann);
        }

        const genTy = mut ? rhsTy : letEnv.generalize(rhsTy);
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
