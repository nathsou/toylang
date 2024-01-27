import { type DataType, constructors } from "itsamatch";

export type Type = DataType<{
  Var: { ref: TypeVar };
  Fun: { name: string; args: Type[] };
}>;

export const Type = {
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
};

function list(elems: Type[], tail: Type = Type.Nil): Type {
  return elems.reduceRight((tail, head) => Type.Cons(head, tail), tail);
}

export type TypeVarId = number;
export type TypeVar = DataType<{
  Unbound: { id: TypeVarId; name?: string; level: number };
  Generic: { id: TypeVarId; name?: string };
  Param: { name: string; ty?: Type };
  Link: { type: Type };
}>;

export const TypeVar = {
  ...constructors<TypeVar>().get("Unbound", "Generic", "Param", "Link"),
};
