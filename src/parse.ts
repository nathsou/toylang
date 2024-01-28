import { constructors, matchMany, type DataType, match } from "itsamatch";
import { Literal, type BinaryOp, type UnaryOp, Expr, Stmt } from "./ast";
import { Type, TypeVar } from "./type";

type Punctuation =
  | "("
  | ")"
  | "{"
  | "}"
  | "["
  | "]"
  | ","
  | ";"
  | ":"
  | "="
  | "+="
  | "-="
  | "->"
  | "_"
  | ".";

type Symbol = UnaryOp | BinaryOp | Punctuation;

const keywords = [
  "module",
  "let",
  "mut",
  "fun",
  "if",
  "else",
  "match",
  "for",
  "while",
  "return",
  "break",
  "type",
  "enum",
  "struct",
  "interface",
  "extend",
  "use",
  "in",
  "declare",
  "import",
  "pub",
  "static",
  "or",
  "and",
  "yield",
] as const;

export type Keyword = (typeof keywords)[number];

export const Keyword = {
  values: new Set<string>(keywords),
  is: (value: string): value is Keyword => Keyword.values.has(value),
};

export type Token = DataType<{
  Literal: { value: Literal };
  Identifier: string;
  Symbol: Symbol;
  Keyword: Keyword;
  EOF: {};
}> & { loc?: { start: number; end: number } };

const Token = {
  ...constructors<Token>().get("Identifier", "Symbol", "Keyword", "EOF"),
  Literal: (value: Literal) => ({ variant: "Literal", value } as const),
  eq: (a: Token, b: Token) =>
    matchMany([a, b], {
      "Literal Literal": (a, b) => Literal.eq(a.value, b.value),
      "Identifier Identifier": Object.is,
      "Symbol Symbol": Object.is,
      "Keyword Keyword": Object.is,
      "EOF EOF": () => true,
      _: () => false,
    }),
};

type Char = string;

export const lex = (source: string): Token[] => {
  let index = 0;
  let startIndex = 0;

  function peek(lookahead = 0): Char {
    return source[index + lookahead];
  }

  function next(): Char {
    const c = peek();

    if (c === "\n" && shouldInsertSemicolon()) {
      tokens.push(Token.Symbol(";"));
    }

    index += 1;

    return c;
  }

  function matches(char: Char): boolean {
    const c = peek();

    if (c === char) {
      next();
      return true;
    }

    return false;
  }

  function isDigit(char: Char): boolean {
    return char >= "0" && char <= "9";
  }

  function isAlpha(char: Char): boolean {
    return (
      (char >= "a" && char <= "z") ||
      (char >= "A" && char <= "Z") ||
      char === "_"
    );
  }

  function isAlphaNumeric(char: Char): boolean {
    return isAlpha(char) || isDigit(char);
  }

  function isWhitespace(char: Char): boolean {
    return char === " " || char === "\r" || char === "\t" || char === "\n";
  }

  function parseNum(): Token {
    while (isDigit(peek())) {
      next();
    }

    if (peek() === "." && isDigit(peek(1))) {
      next();

      while (isDigit(peek())) {
        next();
      }
    }

    const num = Number(source.slice(startIndex, index));
    return Token.Literal(Literal.Num(num));
  }

  function isAtEnd(): boolean {
    return index >= source.length;
  }

  function parseStr(): Token {
    while (peek() !== '"' && !isAtEnd()) {
      if (peek() === "\\") {
        next();
      }

      next();
    }

    if (isAtEnd()) {
      throw new Error("Unterminated string.");
    }

    next();

    const str = source.slice(startIndex + 1, index - 1);
    return Token.Literal(Literal.Str(str));
  }

  function parseIdentifierOrKeyword(): Token {
    while (isAlphaNumeric(peek())) {
      next();
    }

    const identifier = source.slice(startIndex, index);

    switch (identifier) {
      case "true":
        return Token.Literal(Literal.Bool(true));
      case "false":
        return Token.Literal(Literal.Bool(false));
      case "and":
        return Token.Symbol("and");
      case "or":
        return Token.Symbol("or");
      case "mod":
        return Token.Symbol("mod");
      default:
        return Keyword.is(identifier)
          ? Token.Keyword(identifier)
          : Token.Identifier(identifier);
    }
  }

  function shouldInsertSemicolon(): boolean {
    if (tokens.length > 0) {
      return match(tokens[tokens.length - 1], {
        Literal: (lit) => {
          switch (lit.value.variant) {
            case "Bool":
              return true;
            case "Num":
              return true;
            case "Str":
              return true;
            default:
              return false;
          }
        },
        Identifier: () => true,
        Symbol: (symb) => {
          switch (symb) {
            case ")":
              return true;
            case "]":
              return true;
            case "}":
              return true;
            case ">":
              return true;
            default:
              return false;
          }
        },
        Keyword: (kw) => {
          switch (kw) {
            case "return":
              return true;
            case "yield":
              return true;
            case "break":
              return true;
            default:
              return false;
          }
        },
        _: () => false,
      });
    }

    return false;
  }

  function skipWhitespaces() {
    while (isWhitespace(peek())) {
      next();
    }
  }

  function iter(): Token | null {
    skipWhitespaces();
    if (isAtEnd()) return null;
    startIndex = index;

    const char = next();

    switch (char) {
      case "(":
        return Token.Symbol("(");
      case ")":
        return Token.Symbol(")");
      case "{":
        return Token.Symbol("{");
      case "}":
        return Token.Symbol("}");
      case "[":
        return Token.Symbol("[");
      case "]":
        return Token.Symbol("]");
      case ",":
        return Token.Symbol(",");
      case ";":
        return Token.Symbol(";");
      case "+":
        return Token.Symbol(matches("=") ? "+=" : matches("+") ? "++" : "+");
      case "-":
        return Token.Symbol(matches("=") ? "-=" : matches(">") ? "->" : "-");
      case "*":
        return Token.Symbol(matches("*") ? "**" : "*");
      case "/": {
        if (matches("/")) {
          while (peek() !== "\n" && !isAtEnd()) {
            next();
          }

          return iter();
        } else {
          return Token.Symbol("/");
        }
      }
      case "!":
        return Token.Symbol(matches("=") ? "!=" : "!");
      case "=":
        return Token.Symbol(matches("=") ? "==" : "=");
      case "<":
        return Token.Symbol(matches("=") ? "<=" : "<");
      case ">":
        return Token.Symbol(matches("=") ? ">=" : ">");
      case "&":
        return Token.Symbol("&");
      case "|":
        return Token.Symbol("|");
      case ":":
        return Token.Symbol(":");
      case "_":
        return Token.Symbol("_");
      case ".":
        return Token.Symbol(".");
      case '"':
        return parseStr();
      default:
        if (isDigit(char)) {
          return parseNum();
        }

        if (isAlpha(char)) {
          return parseIdentifierOrKeyword();
        }

        throw new Error(`Unexpected character: '${char}'`);
    }
  }

  const tokens: Token[] = [];

  while (true) {
    const token = iter();
    if (token === null) return tokens;
    token.loc = { start: startIndex, end: index };
    tokens.push(token);
  }
};

export const parse = (tokens: Token[]) => {
  let index = 0;

  // ------ meta ------

  function isAtEnd(): boolean {
    return index >= tokens.length;
  }

  function peek(lookahead: number = 0): Token {
    if (index + lookahead >= tokens.length) {
      return Token.EOF({});
    }

    return tokens[index + lookahead];
  }

  function check(...tokens: Token[]): boolean {
    const t = peek();

    for (const token of tokens) {
      if (Token.eq(token, t)) {
        return true;
      }
    }

    return false;
  }

  function matches(...tokens: Token[]): boolean {
    for (const token of tokens) {
      if (check(token)) {
        next();
        return true;
      }
    }

    return false;
  }

  function next() {
    index += 1;
  }

  function consume(
    token: Token,
    error: string = `Expected '${token.variant}'`
  ) {
    if (check(token)) {
      next();
    } else {
      raise(error);
    }
  }

  function consumeIfPresent(token: Token) {
    if (check(token)) {
      next();
    }
  }

  function identifier(): string {
    return match(peek(), {
      Identifier: (name) => {
        next();
        return name;
      },
      _: () => {
        raise("Expected identifier");
      },
    });
  }

  function literal(): Literal {
    return match(peek(), {
      Literal: ({ value }) => {
        next();
        return value;
      },
      _: () => {
        raise("Expected literal");
      },
    });
  }

  function raise(message: string): never {
    throw new Error(`Parse error: ${message}`);
  }

  // sepBy(rule, sep) -> (<rule> (sep <rule>)*)?
  function sepBy<T>(
    rule: () => T,
    separator: Token,
    closingToken = Token.Symbol(")")
  ): T[] {
    let terms: T[] = [];

    do {
      if (check(closingToken)) {
        break;
      }

      terms.push(rule());
    } while (matches(separator));

    consumeIfPresent(Token.Symbol(";"));

    return terms;
  }

  // commas(rule) -> (<rule> (',' <rule>)* ','?)?
  function commas<T>(rule: () => T): T[] {
    return sepBy(rule, Token.Symbol(","));
  }

  function binaryExpr(p: () => Expr, ops: BinaryOp[]): Expr {
    let lhs = p();

    while (true) {
      const token = peek();
      if (
        token.variant === "Symbol" &&
        (ops as Symbol[]).includes(token.$value)
      ) {
        next();
        const rhs = p();
        lhs = Expr.Binary({ lhs, op: token.$value as BinaryOp, rhs });
      } else {
        break;
      }
    }

    return lhs;
  }

  function typeParams(): string[] {
    if (matches(Token.Symbol("<"))) {
      const params = commas(() => {
        const name = identifier();
        if (name[0].toLowerCase() !== name[0]) {
          throw new Error("type parameters must be lowercase");
        }

        return name;
      });
      consume(Token.Symbol(">"));
      return params;
    } else {
      return [];
    }
  }

  function consumeSeparatorIfPresent() {
    if (check(Token.Symbol(",")) || check(Token.Symbol(";"))) {
      next();
    }
  }

  // ------ types ------

  function type(): Type {
    return consType();
  }

  function consType(): Type {
    const lhs = arrayType();

    if (matches(Token.Symbol(":"))) {
      const rhs = type();
      return Type.Cons(lhs, rhs);
    }

    return lhs;
  }

  function arrayType(): Type {
    let lhs = funType();

    while (matches(Token.Symbol("["))) {
      consume(Token.Symbol("]"));
      lhs = Type.Array(lhs);
    }

    return lhs;
  }

  function funType(): Type {
    const args: Type[] = [];
    if (check(Token.Symbol("<"))) {
      typeParams();
    }

    if (matches(Token.Symbol("("))) {
      while (!matches(Token.Symbol(")"))) {
        args.push(type());
        consumeIfPresent(Token.Symbol(","));
      }
    } else {
      args.push(typeList());
    }

    if (matches(Token.Symbol("->"))) {
      const ret = type();
      return Type.Function(args, ret);
    } else {
      return Type.Tuple(args);
    }
  }

  function typeList(): Type {
    if (matches(Token.Symbol("["))) {
      if (matches(Token.Symbol("]"))) {
        return Type.Nil;
      }

      const types = commas(type);
      consume(Token.Symbol("]"));
      return Type.utils.list(types);
    }

    return primaryType();
  }

  function primaryType(): Type {
    const token = peek();

    if (token.variant === "Identifier") {
      if (token.$value[0].toUpperCase() === token.$value[0]) {
        next();
        return constructorType(token.$value);
      } else {
        next();
        return varType(token.$value);
      }
    }

    if (token.variant === "Symbol") {
      if (token.$value === "_") {
        next();
        return Type.Var(TypeVar.Param({ name: "_" }));
      }
    }

    throw new Error("Expected type");
  }

  function constructorType(name: string): Type {
    const args: Type[] = [];

    if (matches(Token.Symbol("<"))) {
      do {
        args.push(type());
      } while (matches(Token.Symbol(",")));

      consume(Token.Symbol(">"));
    }

    return Type.Fun(name, args);
  }

  function varType(name: string): Type {
    return Type.Var(TypeVar.Param({ name }));
  }

  function typeAnnotation(): Type | undefined {
    if (matches(Token.Symbol(":"))) {
      return type();
    }
  }

  // ------ expressions ------

  function expr(): Expr {
    const token = peek();

    if (token.variant === "Keyword") {
      switch (token.$value) {
        case "if":
          next();
          return ifExpr();
        case "fun":
          next();
          return funExpr();
      }
    }

    return logicalOrExpr();
  }

  type FunctionArgument = {
    name: string;
    ann?: Type;
  };

  function functionArgument(): FunctionArgument {
    const name = identifier();
    const ann = typeAnnotation();
    return { name, ann };
  }

  function funExpr(): Expr {
    let args: FunctionArgument[];
    let ret: Type | undefined;
    const generics = typeParams();
    const token = peek();

    if (token.variant === "Symbol" && token.$value === "(") {
      next();
      args = commas(functionArgument);
      consume(Token.Symbol(")"));
      ret = typeAnnotation();
    } else {
      raise("Expected function arguments");
    }

    return Expr.Fun({ generics, args, ret, body: expr() });
  }

  function ifExpr(): Expr {
    const cond = expr();
    const then = logicalOrExpr();

    if (matches(Token.Keyword("else"))) {
      if (matches(Token.Keyword("if"))) {
        return Expr.If({ cond, then, otherwise: ifExpr() });
      } else {
        return Expr.If({ cond, then, otherwise: logicalOrExpr() });
      }
    }

    return Expr.If({ cond, then });
  }

  function logicalOrExpr(): Expr {
    return binaryExpr(logicalAndExpr, ["or"]);
  }

  function logicalAndExpr(): Expr {
    return binaryExpr(equalityExpr, ["and"]);
  }

  function equalityExpr(): Expr {
    return binaryExpr(comparisonExpr, ["==", "!="]);
  }

  function comparisonExpr(): Expr {
    return binaryExpr(additionExpr, ["<", "<=", ">", ">="]);
  }

  function additionExpr(): Expr {
    return binaryExpr(multiplicationExpr, ["+", "-", "++"]);
  }

  function multiplicationExpr(): Expr {
    return binaryExpr(powExpr, ["*", "/", "mod"]);
  }

  function powExpr(): Expr {
    return binaryExpr(unaryExpr, ["**"]);
  }

  function unaryExpr(): Expr {
    const token = peek();

    if (token.variant === "Symbol" && ["-", "+", "!"].includes(token.$value)) {
      next();
      const expr = callExpr();
      return Expr.Unary({ op: token.$value as UnaryOp, expr });
    }

    return callExpr();
  }

  function callExpr(): Expr {
    let lhs = primaryExpr();

    while (true) {
      if (matches(Token.Symbol("."))) {
        const token = peek();

        switch (token.variant) {
          case 'Identifier':
            // universal function call syntax
            next();
            consume(Token.Symbol("("));
            const args = commas(expr);
            consume(Token.Symbol(")"));
            lhs = Expr.Call({
              fun: Expr.Variable({ name: token.$value }),
              args: [lhs, ...args],
            });
            break;

          case "Literal":
            switch (token.value.variant) {
              case "Num": {
                next();
                lhs = Expr.TupleAccess({
                  tuple: lhs,
                  index: token.value.$value,
                });
                break;
              }
            }
            break;
          default:
            raise("Expected identifier or number literal");
        }
      } else if (matches(Token.Symbol("["))) {
        const index = expr();
        consume(Token.Symbol("]"));
        lhs = Expr.ArrayAccess({ arr: lhs, index });
      } else if (matches(Token.Symbol("("))) {
        const args = commas(expr);
        consume(Token.Symbol(")"));
        lhs = Expr.Call({ fun: lhs, args });
      } else {
        break;
      }
    }

    return lhs;
  }

  function primaryExpr(): Expr {
    return match(peek(), {
      Literal: ({ value }) => {
        next();
        return Expr.Literal({ literal: value });
      },
      Identifier: (name) => {
        next();
        return Expr.Variable({ name });
      },
      Symbol: (symb) => {
        switch (symb) {
          case "(":
            return tupleExpr();
          case "{":
            return blockExpr();
          case "[":
            return arrayExpr();
          default:
            raise(`Unexpected symbol '${symb}'`);
        }
      },
      _: () => raise("Expected expression"),
    });
  }

  function tupleExpr(): Expr {
    consume(Token.Symbol("("));
    const elems = commas(expr);
    consume(Token.Symbol(")"));

    switch (elems.length) {
      case 0:
        return Expr.Literal({ literal: Literal.Unit() });
      case 1:
        return elems[0];
      default:
        return Expr.Tuple({ elems });
    }
  }

  function arrayExpr(): Expr {
    consume(Token.Symbol("["));
    const elems = sepBy(expr, Token.Symbol(","), Token.Symbol("]"));
    consume(Token.Symbol("]"));

    return Expr.Array({ elems });
  }

  function blockExpr(): Expr {
    consume(Token.Symbol("{"));
    const stmts: Stmt[] = [];

    while (!matches(Token.Symbol("}"))) {
      stmts.push(stmt());
    }

    if (stmts.length > 0) {
      const last = stmts[stmts.length - 1];
      if (last.variant === "Expr") {
        stmts.pop();
        return Expr.Block({ stmts, ret: last.expr });
      }
    }

    return Expr.Block({ stmts });
  }

  // ------ statements ------

  function stmt(): Stmt {
    return match(peek(), {
      Keyword: (keyword) => {
        switch (keyword) {
          case "let":
          case "mut":
            next();
            return letStmt(keyword === "mut");
          case "fun":
            next();
            return funStmt();
          case "while":
            next();
            return whileStmt();
          case "for":
            next();
            return forStmt();
          case "return":
            next();
            return returnStmt();
          case "break":
            next();
            return breakStmt();
          default:
            return assignmentStmt();
        }
      },
      _: () => assignmentStmt(),
    });
  }

  function funStmt(): Stmt {
    const name = identifier();
    const value = funExpr();
    consumeIfPresent(Token.Symbol(";"));

    return Stmt.Let({
      mut: false,
      name,
      value,
    });
  }

  function letStmt(mut: boolean): Stmt {
    const name = identifier();
    const ann = typeAnnotation();
    consume(Token.Symbol("="));
    const value = expr();
    consumeIfPresent(Token.Symbol(";"));

    return Stmt.Let({
      mut,
      name,
      ann,
      value,
    });
  }

  function statementList(): Stmt[] {
    const stmts: Stmt[] = [];

    consume(Token.Symbol("{"));

    while (!matches(Token.Symbol("}"))) {
      stmts.push(stmt());
    }

    return stmts;
  }

  function whileStmt(): Stmt {
    const cond = expr();
    const body = statementList();
    consumeIfPresent(Token.Symbol(";"));

    return Stmt.While({ cond, body });
  }

  function forStmt(): Stmt {
    const name = identifier();
    consume(Token.Keyword("in"));
    const iter = expr();
    const body = statementList();
    consumeIfPresent(Token.Symbol(";"));

    return Stmt.For({ name, iter, body });
  }

  function returnStmt(): Stmt {
    const value = expr();
    consumeIfPresent(Token.Symbol(";"));

    return Stmt.Return({ expr: value });
  }

  function breakStmt(): Stmt {
    consumeIfPresent(Token.Symbol(";"));

    return Stmt.Break();
  }

  function assignmentStmt(): Stmt {
    const lhs = expr();

    const token = peek();
    if (
      token.variant === "Symbol" &&
      (token.$value === "=" || token.$value === "+=" || token.$value === "-=")
    ) {
      next();

      const value = expr();
      consumeIfPresent(Token.Symbol(";"));
      return Stmt.Assign({ lhs, op: token.$value, rhs: value });
    }

    consumeIfPresent(Token.Symbol(";"));

    return Stmt.Expr({ expr: lhs });
  }

  function statements(): Stmt[] {
    const stmts: Stmt[] = [];

    while (!isAtEnd()) {
      stmts.push(stmt());
    }

    return stmts;
  }

  return { expr, stmt, statements };
};
