import { test } from "vitest";
import { Lexer, Parser, Source, Tokenizer } from "./lib";

test("Lexer", () => {
  const input = String.raw`
  &     & & & & & \mbox{Syndrome Measurement} & & & &
\mbox{Recovery}\\
& \qw & \qw & \ctrl{3} & \qw & \qw & \qw &
\ctrl{5} & \qw & \qw &
  `;

  String.raw`
&     & & & & & \mbox{Syndrome Measurement} & & & &
\mbox{Recovery} \\
& \qw & \qw & \ctrl{3} & \qw & \qw & \qw &
\ctrl{5} & \qw & \qw &
\multigate{2}{\ \mathcal{R}\ } & \qw \\
& \qw & \qw & \qw & \ctrl{2} & \ctrl{3} & \qw &
\qw & \qw & \qw & \ghost{\ \mathcal{R}\ } \qw &
\qw \\
& \qw & \qw & \qw & \qw & \qw & \ctrl{2} & \qw &
\ctrl{3} & \qw & \ghost{\ \mathcal{R}\ } \qw &
\qw \\
& & \lstick{\ket{0}} & \targ \qw & \targ \qw &
\qw & \qw & \qw & \qw & \measure{M_a} &
\control \cw \cwx \\
& & \lstick{\ket{0}} & \qw & \qw & \targ \qw &
\targ \qw & \qw & \qw & \measure{M_b} &
\control \cw \cwx \\
& & \lstick{\ket{0}} & \qw & \qw & \qw & \qw &
\targ \qw & \targ \qw & \measure{M_c}
\gategroup{2}{2}{7}{10}{.8em}{--} &
\control \cw \cwx
}
  `;

  const lexer = new Lexer(new Source(input));

  let token = lexer.lex();
  while (token) {
    console.log(token);
    token = lexer.lex();
  }
});

test.only("Parser", () => {
  const input = String.raw`
&     & & & & & \mbox{Syndrome Measurement} & & & &
\mbox{Recovery} \\
& \qw & \qw & \ctrl{3} & \qw & \qw & \qw &
\ctrl{5} & \qw & \qw &
\multigate{2}{\ \mathcal{R}\ } & \qw \\
& \qw & \qw & \qw & \ctrl{2} & \ctrl{3} & \qw &
\qw & \qw & \qw & \ghost{\ \mathcal{R}\ } \qw &
\qw \\
& \qw & \qw & \qw & \qw & \qw & \ctrl{2} & \qw &
\ctrl{3} & \qw & \ghost{\ \mathcal{R}\ } \qw &
\qw \\
& & \lstick{\ket{0}} & \targ \qw & \targ \qw &
\qw & \qw & \qw & \qw & \measure{M_a} &
\control \cw \cwx \\
& & \lstick{\ket{0}} & \qw & \qw & \targ \qw &
\targ \qw & \qw & \qw & \measure{M_b} &
\control \cw \cwx \\
& & \lstick{\ket{0}} & \qw & \qw & \qw & \qw &
\targ \qw & \targ \qw & \measure{M_c}
\gategroup{2}{2}{7}{10}{.8em}{--} &
\control \cw \cwx
  `;

  const source = new Source(input);
  const lexer = new Lexer(source);
  const tokenizer = new Tokenizer(lexer);
  const parser = new Parser(tokenizer);

  console.log(JSON.stringify(parser.parse().cells, undefined, 2));
});
