import { Env } from "./src/interpret";
import { lex, parse } from "./src/parse";

const path = Bun.argv[2];

if (!path) {
    console.error('No source file provided');
} else {
    const source = await Bun.file(path).text();
    const tokens = lex(source);
    const stmts = parse(tokens).statements();
    const env = new Env();
    
    for (const stmt of stmts) {
        env.evalStmt(stmt);
    }
}
