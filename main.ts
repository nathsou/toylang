import { Env } from "./src/interpret";
import { lex, parse } from "./src/parse";
import { Type, TypeEnv } from "./src/type";

const path = Bun.argv[2];
const SHOW_TYPES = true;

if (!path) {
  console.error("No source file provided");
} else {
  const source = await Bun.file(path).text();
  const tokens = lex(source);
  const stmts = parse(tokens).statements();
  const typeEnv = new TypeEnv();

  for (const stmt of stmts) {
    typeEnv.inferStmt(stmt);
  }

  if (SHOW_TYPES) {
    for (const [name, { ty }] of typeEnv.variables.members) {
      console.log(`${name}: ${Type.show(ty)}`);
    }

    if (typeEnv.variables.members.size > 0) {
      console.log();
    }
  }

  const env = new Env();

  for (const stmt of stmts) {
    env.evalStmt(stmt);
  }
}
