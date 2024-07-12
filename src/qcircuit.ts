import "./qcircuit.scss";
import { Lexer, Parser, Source, Tokenizer } from "./lib";

export default function qcircuit(
  // @ts-expect-error Temporarily unused.
  renderTex: (tex: string) => string,
  tex: string
): string {
  const source = new Source(tex);
  const lexer = new Lexer(source);
  const tokenizer = new Tokenizer(lexer);
  const parser = new Parser(tokenizer);

  // @ts-expect-error Temporarily unused.
  const circuit = parser.parse();

  return "TODO";
}
