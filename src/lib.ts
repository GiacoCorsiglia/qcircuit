//
// HELPERS.
//

type Writeable<T> = {
  -readonly [K in keyof T]: T[K];
};
const writeable = <T>(o: T): Writeable<T> => o;

const last = <T>(array: readonly T[]): T | undefined => {
  const length = array.length;
  return length > 0 ? array[length - 1] : undefined;
};

//
// ERRORS.
//

class QCircuitError extends Error {}

class UnimplementedError extends QCircuitError {}

class AssertionError extends QCircuitError {}

class UnhandledCaseError extends QCircuitError {
  constructor(value: never) {
    super(`Unhandled case: ${String(value)}`);
  }
}

class ParseError extends QCircuitError {}

class UnexpectedTokenError extends ParseError {
  constructor(token: Token) {
    // TODO: Make this look nicer.
    super(`Unexpected token: ${token.type} at position ${token.start}`);
  }
}

class MissingArgumentError extends ParseError {
  constructor(command: Command, n: number, actualToken: Token) {
    const nth =
      n === 0 ? "1st" : n === 1 ? "2nd" : n === 2 ? "3rd" : `${n + 1}th`;

    super(
      `Missing ${nth} argument for "${command.name}" at position ${actualToken.start}\n` +
        `Expected: ${TokenType.OpenBrace}\n` +
        `Received: ${actualToken.type}`
    );
  }
}

class ArgumentEvaluationError extends QCircuitError {}

//
// COMMANDS.
//

abstract class Argument<T> {
  public readonly _type!: T;

  abstract evaluate(value: string): T;
}

class IgnoredArgument extends Argument<null> {
  evaluate(_: string): null {
    return null;
  }
}

class StringArgument extends Argument<string> {
  evaluate(value: string): string {
    return value;
  }
}

class IntegerArgument extends Argument<number> {
  evaluate(value: string): number {
    value = value.trim();
    const parsed = parseInt(value);
    if (Number.isNaN(parsed)) {
      throw new ArgumentEvaluationError(
        `Expected: an integer\nReceived: "${parsed}"`
      );
    }
    return parsed;
  }
}

class LiteralArgument<
  const L extends readonly [string, ...string[]],
> extends Argument<L[number]> {
  private literals: Set<L[number]>;

  constructor(...literals: L) {
    super();
    this.literals = new Set(literals);
  }

  evaluate(value: string): L[number] {
    if (!this.literals.has(value)) {
      throw new ArgumentEvaluationError(
        (this.literals.size > 1
          ? `Expected: One of ${[...this.literals].join(", ")}`
          : `Expected: ${[...this.literals][0]}`) + `\nReceived: "${value}"`
      );
    }
    return value;
  }
}

const ignoredArg = new IgnoredArgument();
const stringArg = new StringArgument();
const integerArg = new IntegerArgument();

type EvaluatedArguments<
  Args extends readonly Argument<unknown>[],
  OArg extends Argument<unknown> | undefined,
> =
  OArg extends Argument<unknown>
    ? [..._EvaluatedArgument<Args>, OArg["_type"]]
    : _EvaluatedArgument<Args>;
type _EvaluatedArgument<Args extends readonly Argument<unknown>[]> = {
  [K in keyof Args]: Args[K]["_type"];
};

interface Command<
  Args extends readonly Argument<unknown>[] = [],
  OArg extends Argument<unknown> | undefined = Argument<unknown> | undefined,
> {
  /**
   * Name of the command with leading backslash
   *
   * @example "\\gate"
   */
  readonly name: string;
  /**
   * List of required argument types.
   *
   * @example \multigate{#1}{#2} has 2 required arguments.
   */
  readonly arguments?: Args;
  /**
   * Whether the command accepts an optional argument.
   */
  readonly optionalArgument?: OArg;
  /**
   * Render function.  Accepts a positional list of arguments, with the optional argument in the last
   */
  readonly render: (...args: EvaluatedArguments<Args, OArg>) => string;
}

const commands = new Map<string, Command>();

const defineCommand = <
  const Args extends readonly Argument<any>[] = [],
  OArg extends Argument<any> | undefined = undefined,
>(
  command: Command<Args, OArg>
) => {
  commands.set(command.name, command as any);
};

// SINGLE CELLS.

// Simple cells.

defineCommand({
  name: "\\gate",
  arguments: [stringArg],
  render(tex) {
    console.log({ tex });
    throw new UnimplementedError();
  },
});

defineCommand({
  name: "\\control",
  render() {
    throw new UnimplementedError();
  },
});

defineCommand({
  name: "\\controlo",
  render() {
    throw new UnimplementedError();
  },
});

defineCommand({
  name: "\\targ",
  render() {
    throw new UnimplementedError();
  },
});

defineCommand({
  name: "\\qswap",
  render() {
    throw new UnimplementedError();
  },
});

defineCommand({
  name: "\\meter",
  render() {
    throw new UnimplementedError();
  },
});

defineCommand({
  name: "\\meterB",
  arguments: [stringArg],
  render(basis) {
    console.log({ basis });
    throw new UnimplementedError();
  },
});

defineCommand({
  name: "\\measure",
  arguments: [stringArg],
  render(label) {
    console.log({ label });
    throw new UnimplementedError();
  },
});

defineCommand({
  name: "\\measureD",
  arguments: [stringArg],
  render(label) {
    console.log({ label });
    throw new UnimplementedError();
  },
});

defineCommand({
  name: "\\measuretab",
  arguments: [stringArg],
  render(label) {
    console.log({ label });
    throw new UnimplementedError();
  },
});

// Ghost cells.

defineCommand({
  name: "\\ghost",
  arguments: [ignoredArg],
  render() {
    throw new UnimplementedError();
  },
});

defineCommand({
  name: "\\pureghost",
  arguments: [ignoredArg],
  render() {
    throw new UnimplementedError();
  },
});

defineCommand({
  name: "\\cghost",
  arguments: [ignoredArg],
  render() {
    throw new UnimplementedError();
  },
});

defineCommand({
  name: "\\nghost",
  arguments: [ignoredArg],
  render() {
    throw new UnimplementedError();
  },
});

// Alignment.

defineCommand({
  name: "\\lstick",
  arguments: [stringArg],
  render(tex) {
    console.log({ tex });
    throw new UnimplementedError();
  },
});

defineCommand({
  name: "\\rstick",
  arguments: [stringArg],
  render(tex) {
    console.log({ tex });
    throw new UnimplementedError();
  },
});

defineCommand({
  name: "\\ustick",
  arguments: [stringArg],
  render(tex) {
    console.log({ tex });
    throw new UnimplementedError();
  },
});

defineCommand({
  name: "\\dstick",
  arguments: [stringArg],
  render(tex) {
    console.log({ tex });
    throw new UnimplementedError();
  },
});

// EXPLICIT WIRE.

// Horizontal.

defineCommand({
  name: "\\qw",
  optionalArgument: integerArg,
  render(to = -1) {
    console.log({ to });
    throw new UnimplementedError();
  },
});

defineCommand({
  name: "\\qwa",
  optionalArgument: integerArg,
  render(to = -1) {
    console.log({ to });
    throw new UnimplementedError();
  },
});

defineCommand({
  name: "\\cw",
  optionalArgument: integerArg,
  render(to = -1) {
    console.log({ to });
    throw new UnimplementedError();
  },
});

defineCommand({
  name: "\\cwa",
  optionalArgument: integerArg,
  render(to = -1) {
    console.log({ to });
    throw new UnimplementedError();
  },
});

// Vertical.

defineCommand({
  name: "\\qwx",
  optionalArgument: integerArg,
  render(to = -1) {
    console.log({ to });
    throw new UnimplementedError();
  },
});

defineCommand({
  name: "\\cwx",
  optionalArgument: integerArg,
  render(to = -1) {
    console.log({ to });
    throw new UnimplementedError();
  },
});

defineCommand({
  name: "\\ctrl",
  arguments: [integerArg],
  render(target) {
    console.log({ target });
    throw new UnimplementedError();
  },
});

defineCommand({
  name: "\\ctrlo",
  arguments: [integerArg],
  render(target) {
    console.log({ target });
    throw new UnimplementedError();
  },
});

defineCommand({
  name: "\\cctrl",
  arguments: [integerArg],
  render(target) {
    console.log({ target });
    throw new UnimplementedError();
  },
});

defineCommand({
  name: "\\cctrlo",
  arguments: [integerArg],
  render(target) {
    console.log({ target });
    throw new UnimplementedError();
  },
});

// Multi.

defineCommand({
  name: "\\barrier",
  arguments: [integerArg],
  // This is a horizontal offset, which wouldn't be bad to support...
  optionalArgument: ignoredArg,
  render(span) {
    console.log({ span });
    throw new UnimplementedError();
  },
});

// Diagonal.

// MULTI CELLS.

defineCommand({
  name: "\\multigate",
  arguments: [integerArg, stringArg],
  render(span, tex) {
    console.log({ span, tex });
    throw new UnimplementedError();
  },
});

defineCommand({
  name: "\\sgate",
  arguments: [integerArg, stringArg],
  render(span, tex) {
    console.log({ span, tex });
    throw new UnimplementedError();
  },
});

defineCommand({
  name: "\\smeterB",
  arguments: [integerArg, stringArg],
  render(span, basis) {
    console.log({ span, basis });
    throw new UnimplementedError();
  },
});

defineCommand({
  name: "\\multimeasure",
  arguments: [integerArg, stringArg],
  render(span, tex) {
    console.log({ span, tex });
    throw new UnimplementedError();
  },
});

defineCommand({
  name: "\\multimeasureD",
  arguments: [integerArg, stringArg],
  render(span, tex) {
    console.log({ span, tex });
    throw new UnimplementedError();
  },
});

// GLOBAL LABELS.

defineCommand({
  name: "\\gategroup",
  arguments: [
    integerArg,
    integerArg,
    integerArg,
    integerArg,
    ignoredArg,
    new LiteralArgument("--", ".", "_}", "^}", "{", "}", "_)", "^)", "(", ")"),
  ],
  render(fromRow, fromCol, toRow, toCol, _, borderStyle) {
    console.log({ fromRow, fromCol, toRow, toCol, borderStyle });
    throw new UnimplementedError();
  },
});

defineCommand({
  name: "\\inputgroup",
  arguments: [integerArg, integerArg, ignoredArg, stringArg],
  render(fromRow, toRow, _, label) {
    console.log({ fromRow, toRow, label });
    throw new UnimplementedError();
  },
});

defineCommand({
  name: "\\inputgroupv",
  arguments: [integerArg, integerArg, ignoredArg, ignoredArg, stringArg],
  render(fromRow, toRow, _a, _b, label) {
    console.log({ fromRow, toRow, label });
    throw new UnimplementedError();
  },
});

defineCommand({
  name: "\\inputgrouph",
  arguments: [integerArg, integerArg, ignoredArg, stringArg, ignoredArg],
  render(fromRow, toRow, _a, label, _b) {
    console.log({ fromRow, toRow, label });
    throw new UnimplementedError();
  },
});

// SPACING.

defineCommand({
  name: "\\push",
  arguments: [stringArg],
  render(tex) {
    console.log({ tex });
    throw new UnimplementedError();
  },
});

defineCommand({
  name: "\\cds",
  arguments: [integerArg, stringArg],
  render(n, tex) {
    console.log({ n, tex });
    throw new UnimplementedError();
  },
});

defineCommand({
  name: "\\mbox",
  arguments: [stringArg],
  render(tex) {
    console.log({ tex });
    throw new UnimplementedError();
  },
});

//
// SOURCE.
//

interface SourceLocation {
  readonly start: number;
  readonly end: number;
}

export class Source {
  readonly length: number;

  constructor(readonly text: string) {
    this.length = text.length;
  }

  read(position: SourceLocation): string;
  read(start: number, end: number): string;
  read(start: SourceLocation | number, end?: number): string {
    if (typeof start === "object") {
      end = start.end;
      start = start.start;
    }

    return this.text.slice(start, end);
  }
}

//
// LEX.
//

class Token implements SourceLocation {
  constructor(
    private readonly source: Source,
    readonly type: TokenType,
    readonly start: number,
    readonly end: number
  ) {}

  get contents() {
    return this.source.read(this);
  }
}

enum TokenType {
  Whitespace = "Whitespace",
  LineBreak = "\\\\",
  OpenBracket = "[",
  CloseBracket = "]",
  OpenBrace = "{",
  CloseBrace = "}",
  Ampersand = "&",
  Command = "Command",
  Text = "Text",
  Comment = "Comment",
  EndOfInput = "EndOfInput",
}

const syntaxLiterals = new Map<string, TokenType>([
  ["\\\\", TokenType.LineBreak],
  ["[", TokenType.OpenBracket],
  ["]", TokenType.CloseBracket],
  ["{", TokenType.OpenBrace],
  ["}", TokenType.CloseBrace],
  ["&", TokenType.Ampersand],
]);

const tokenRegexString = [
  // One or more whitespace characters.
  /[ \r\n\t]+/.source,

  // Special syntax characters.
  /\\\\|\[|\]|\{|\}|&/.source,

  // Command.
  /\\([a-zA-z]+)/.source,
  // Escapes. (Note these have the same structure as command, without any
  // capture groups).
  /\\[%{}&]/.source,

  // Comments start with unescaped "%" and always extend to the end of the line
  // (or file).
  /%[^\r\n]*/.source,
].join("|");

export class Lexer {
  /**
   * Regular expressions are stateful.
   */
  private readonly tokenRegex: RegExp;
  private position: number = 0;
  private nextMatch: RegExpExecArray | null = null;

  constructor(readonly source: Source) {
    this.tokenRegex = new RegExp(tokenRegexString, "g");
  }

  lex(): Token {
    // We may have a lingering match if we lexed a Text token between this match
    // and the prior one.
    const nextMatch = this.nextMatch;
    if (nextMatch) {
      this.nextMatch = null;

      return this.lexMatch(nextMatch);
    }

    const match = this.tokenRegex.exec(this.source.text);

    // We're done!
    if (match === null) {
      // Perhaps we have some unmatched Text.
      if (this.position < this.source.length) {
        return this.lexText(this.source.length);
      }

      return new Token(
        this.source,
        TokenType.EndOfInput,
        this.source.length,
        this.source.length
      );
    }

    // There's a Text token between this match and the prior one.
    if (this.position < match.index) {
      // Stash the match to be returned next.
      this.nextMatch = match;
      // Generate the missing text token.
      return this.lexText(match.index);
    }

    return this.lexMatch(match);
  }

  private lexText(end: number): Token {
    const start = this.position;
    this.position = end;
    return new Token(this.source, TokenType.Text, start, end);
  }

  private lexMatch(match: RegExpExecArray): Token {
    /** The full contents of the match (capture groups start at index 1). */
    const contents = match[0];

    // We're committed to lexing this match.
    // console.assert(this.position === match.index)
    const start = this.position;
    const end = (this.position += contents.length);

    // Perhaps we matched a specific syntax character.
    const syntaxLiteralType = syntaxLiterals.get(contents);
    if (syntaxLiteralType) {
      return new Token(this.source, syntaxLiteralType, start, end);
    }

    // Now that we've checked syntax literals, it's safe to check prefixes.

    if (contents[0] === "\\") {
      // If there is a capture group this is a command; otherwise it's an
      // escaped character (and escaped characters are really just text).
      const type = match[1] ? TokenType.Command : TokenType.Text;
      return new Token(this.source, type, start, end);
    }

    // Comments.
    if (contents[0] === "%") {
      // Preserve comments because we may pass them to the TeX renderer, since
      // sometimes comments are used to suppress new lines in TeX.
      return new Token(this.source, TokenType.Comment, start, end);
    }

    // This is the only remaining option!
    return new Token(this.source, TokenType.Whitespace, start, end);
  }
}

//
// PARSE.
//

export class Tokenizer {
  current: Token;

  constructor(private readonly lexer: Lexer) {
    // Set `current` to the first token.
    this.current = this.lexer.lex();
  }

  private next() {
    return (this.current = this.lexer.lex());
  }

  /**
   * Advances to the next token that isn't of type `type`.
   */
  skip(type: TokenType): void {
    while (this.current.type === type) {
      this.next();
    }
  }

  /**
   * Returns the current token and advances to the next one.
   */
  consume(): Token {
    const current = this.current;
    this.next();
    return current;
  }
}

type Node = CommandNode | TexNode | GroupNode;

enum NodeType {
  Command,
  Tex,
  Group,
}

interface CommandNode {
  readonly type: NodeType.Command;

  readonly command: Command;
  readonly arguments: Node[];
}

interface TexNode {
  readonly type: NodeType.Tex;

  readonly tex: string;
}

interface GroupNode {
  readonly type: NodeType.Group;

  readonly nodes: Node[];
}

interface Cell {
  readonly row: number;
  readonly col: number;
  readonly nodes: Node[];
}

interface Circuit {
  readonly cells: Cell[];
}

export class Parser {
  private circuit: Circuit = {
    cells: [],
  };

  private currentCell: Cell = this.createCell(0, 0);

  constructor(private readonly tokenizer: Tokenizer) {}

  parse(): Circuit {
    while (true) {
      const token = this.tokenizer.current;

      switch (token.type) {
        case TokenType.Ampersand: {
          this.tokenizer.consume();
          this.createCell(this.currentCell.row, this.currentCell.col + 1);
          break;
        }

        case TokenType.LineBreak: {
          this.tokenizer.consume();
          this.createCell(this.currentCell.row + 1, 0);
          break;
        }

        case TokenType.Command: {
          this.pushNode(this.currentCell.nodes, this.parseCommand());
          break;
        }

        case TokenType.Text: {
          this.pushTex(this.currentCell.nodes, this.tokenizer.consume());
          break;
        }

        case TokenType.Whitespace: {
          // Consume the whitespace no matter what.
          const token = this.tokenizer.consume();

          // We can ignore the whitespace unless we're already inside a TeX
          // node, in which case we need to respect spaces between elements.
          // Note this means TeX nodes might end with extraneous spaces, but TeX
          // renderers should ignore that anyway!
          const lastNode = last(this.currentCell.nodes);
          if (lastNode?.type === NodeType.Tex) {
            this.pushTex(this.currentCell.nodes, token);
          }

          break;
        }

        case TokenType.OpenBracket:
        case TokenType.OpenBrace: {
          this.pushNode(this.currentCell.nodes, this.parseGroup(true));
          break;
        }

        // Comments are ignored when at the top level (i.e., they don't need to
        // be passed to the TeX renderer if we find them here).
        case TokenType.Comment: {
          this.tokenizer.consume();
          break;
        }

        case TokenType.CloseBracket:
        case TokenType.CloseBrace: {
          throw new UnexpectedTokenError(token);
        }

        // Yay we did it.
        case TokenType.EndOfInput: {
          return this.circuit;
        }

        default: {
          throw new UnhandledCaseError(token.type);
        }
      }
    }
  }

  private parseGroup(reduce: boolean): Node {
    // Consume the open delimiter.
    const opener = this.tokenizer.consume().type;

    if (opener !== TokenType.OpenBrace && opener !== TokenType.OpenBracket) {
      throw new AssertionError(
        `Expected current token of type ${TokenType.OpenBrace} or ${TokenType.OpenBracket}`
      );
    }

    const closer =
      opener === TokenType.OpenBrace
        ? TokenType.CloseBrace
        : TokenType.OpenBrace;

    const groupNode: GroupNode = {
      type: NodeType.Group,
      nodes: [],
    };
    const nodes = groupNode.nodes;

    while (true) {
      const token = this.tokenizer.current;

      switch (token.type) {
        // Did you close the group?
        case TokenType.CloseBrace:
        case TokenType.CloseBracket: {
          if (token.type === closer) {
            // Consume the closer.
            this.tokenizer.consume();

            // And break out of the loop, returning the GroupNode.

            // Reduce down to just TeX if that's all it is, sometimes.
            if (reduce) {
              if (nodes.length === 1) {
                if (nodes[0].type === NodeType.Tex) {
                  writeable(nodes[0]).tex = `${opener}${nodes[0].tex}${closer}`;
                  return nodes[0];
                }
              }
            }

            return groupNode;
          }
          // Or perhaps you had a stray group delimiter of the wrong type.
          throw new UnexpectedTokenError(token);
        }

        // Oh no, the group was never closed!
        case TokenType.EndOfInput: {
          throw new ParseError(`Unclosed group.  Expected ${closer}`);
        }

        // We've opened another group!
        case TokenType.OpenBrace: {
          this.pushNode(nodes, this.parseGroup(true));
          break;
        }

        case TokenType.Command: {
          this.pushNode(nodes, this.parseCommand());
          break;
        }

        case TokenType.Whitespace:
        case TokenType.LineBreak:
        case TokenType.Ampersand:
        case TokenType.Text:
        case TokenType.Comment: {
          this.pushTex(nodes, this.tokenizer.consume());

          break;
        }

        // Nested bracket groups are not a thing.
        case TokenType.OpenBracket: {
          throw new UnexpectedTokenError(token);
        }

        default: {
          throw new UnhandledCaseError(token.type);
        }
      }
    }
  }

  private parseCommand(): Node {
    // Consume the command token.
    const commandToken = this.tokenizer.consume();

    if (commandToken.type !== TokenType.Command) {
      throw new AssertionError(
        `Expected current token of type ${TokenType.Command}`
      );
    }

    const commandName = commandToken.contents;
    const command = commands.get(commandName);

    // If it's an unknown command, preserve it as normal TeX to be rendered by
    // the TeX renderer.
    // TODO: Get rid of this by allow-listing commands.
    if (!command) {
      return {
        type: NodeType.Tex,
        tex: commandToken.contents,
      };
    }

    // If the command supports an optional argument, try to parse it.
    let optionalArgument: Node | null = null;
    if (command.optionalArgument) {
      // We know the optional argument is specified if the command is followed
      // by "[".
      if (this.tokenizer.current.type === TokenType.OpenBracket) {
        optionalArgument = this.parseGroup(false);
      }
      // Otherwise it's unset.
    }

    // Try to parse required arguments.
    const parsedArguments: Node[] = [];
    if (command.arguments) {
      for (let n = 0; n < command.arguments.length; n++) {
        if (this.tokenizer.current.type !== TokenType.OpenBrace) {
          throw new MissingArgumentError(command, n, this.tokenizer.current);
        }

        parsedArguments.push(this.parseGroup(false));
      }
    }

    if (command.optionalArgument && optionalArgument !== null) {
      parsedArguments.push(optionalArgument);
    }

    return {
      type: NodeType.Command,
      command,
      arguments: parsedArguments,
    };
  }

  private createCell(row: number, col: number): Cell {
    this.currentCell = {
      row,
      col,
      nodes: [],
    };
    this.circuit.cells.push(this.currentCell);
    return this.currentCell;
  }

  private pushTex(nodes: Node[], tex: string | Token) {
    if (tex instanceof Token) {
      tex = tex.contents;
    }

    this.pushNode(nodes, {
      type: NodeType.Tex,
      tex,
    });
  }

  private pushNode(nodes: Node[], node: Node) {
    const lastNode = last(nodes);

    if (node.type === NodeType.Tex && lastNode?.type === NodeType.Tex) {
      writeable(lastNode).tex += node.tex;
    } else {
      return nodes.push(node);
    }
  }
}
