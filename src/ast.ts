import { type DataType, constructors, matchMany } from "itsamatch";
import type { Type } from "./type";

export type UnaryOp = "+" | "-" | "!";

export type BinaryOp =
  | "+"
  | "-"
  | "*"
  | "/"
  | "mod"
  | "**"
  | "=="
  | "!="
  | "<"
  | "<="
  | ">"
  | ">="
  | "and"
  | "or"
  | "&"
  | "|"
  | "++";

export type AssignmentOp = "=" | "+=" | "-=";

export type Literal = DataType<{
  Unit: {};
  Bool: boolean;
  Num: number;
  Str: string;
}>;

export const Literal = {
  Unit: (): Literal => ({ variant: "Unit" }),
  ...constructors<Literal>().get("Bool", "Num", "Str"),
  eq: (a: Literal, b: Literal) =>
    matchMany([a, b], {
      "Unit Unit": () => true,
      "Bool Bool": Object.is,
      "Num Num": Object.is,
      "Str Str": Object.is,
      _: () => false,
    }),
};

type FunctionArgument = { name: string; ann?: Type };

export type Expr = DataType<{
  Literal: { literal: Literal };
  Variable: { name: string };
  Unary: { op: UnaryOp; expr: Expr };
  Binary: { lhs: Expr; op: BinaryOp; rhs: Expr };
  Block: { stmts: Stmt[]; ret?: Expr };
  If: { cond: Expr; then: Expr; otherwise?: Expr };
  Fun: {
    generics: string[];
    args: FunctionArgument[];
    ret?: Type;
    body: Expr;
  };
  Call: { fun: Expr; args: Expr[] };
  Tuple: { elems: Expr[] };
  Array: { elems: Expr[] };
  Record: { entries: { name: string; value: Expr }[] };
  ArrayAccess: { arr: Expr; index: Expr };
  TupleAccess: { tuple: Expr; index: number };
  RecordAccess: { record: Expr; name: string };
}> & { ty?: Type };

export const Expr = constructors<Expr>().get(
  "Literal",
  "Variable",
  "Unary",
  "Binary",
  "Block",
  "If",
  "Fun",
  "Call",
  "Tuple",
  "Array",
  "Record",
  "ArrayAccess",
  "TupleAccess",
  "RecordAccess",
);

export type Stmt = DataType<{
  Expr: { expr: Expr };
  Let: {
    mut: boolean;
    name: string;
    ann?: Type;
    value: Expr;
  };
  Assign: { lhs: Expr; op: AssignmentOp; rhs: Expr };
  While: { cond: Expr; body: Stmt[] };
  For: { name: string; iter: Expr; body: Stmt[] };
  Return: { expr: Expr };
  Break: {};
}>;

export const Stmt = constructors<Stmt>().get(
  "Expr",
  "Let",
  "Assign",
  "While",
  "For",
  "Return",
  "Break"
);
