//
// HELPERS.
//

/**
 * Defines a tuple type of length N with elements of type T.
 * https://stackoverflow.com/a/52490977
 */
type Tuple<T, N extends number> = N extends N
  ? number extends N
    ? T[]
    : _TupleOf<T, N, []>
  : never;
type _TupleOf<T, N extends number, R extends unknown[]> = R["length"] extends N
  ? R
  : _TupleOf<T, N, [T, ...R]>;

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

//
// COMMANDS.
//

interface Command<
  NArgs extends number = number,
  OArg extends boolean = boolean,
  TArgs = void,
> {
  /**
   * Name of the command with leading backslash
   *
   * @example "\\gate"
   */
  readonly name: string;
  /**
   * Number of required arguments.
   *
   * @example \multigate{#1}{#2} has 2 required arguments.
   */
  readonly arguments: NArgs;
  /**
   * Whether the command accepts an optional argument.
   */
  readonly optionalArgument: OArg;
  /**
   * A function that accepts the positional arguments and parses them into
   * usable parameters.
   */
  readonly evaluateArguments: OArg extends true
    ? (...args: [...Tuple<string, NArgs>, string | undefined]) => TArgs
    : (...args: Tuple<string, NArgs>) => TArgs;
}

const commands = new Map<string, Command>();

const defineCommand = <
  NArgs extends number = 0,
  OArg extends boolean = false,
  TArgs = void,
>(
  command: Command<NArgs, OArg, TArgs>
) => {
  commands.set(command.name, command as any);
};

// SINGLE CELLS.

// Simple cells.

defineCommand({
  name: "\\gate",
  arguments: 1,
  optionalArgument: false,
  evaluateArguments() {},
});

defineCommand({
  name: "\\control",
  arguments: 0,
  optionalArgument: false,
  evaluateArguments() {},
});

defineCommand({
  name: "\\controlo",
  arguments: 0,
  optionalArgument: false,
  evaluateArguments() {},
});

defineCommand({
  name: "\\targ",
  arguments: 0,
  optionalArgument: false,
  evaluateArguments() {},
});

defineCommand({
  name: "\\qswap",
  arguments: 0,
  optionalArgument: false,
  evaluateArguments() {},
});

defineCommand({
  name: "\\meter",
  arguments: 0,
  optionalArgument: false,
  evaluateArguments() {},
});

defineCommand({
  name: "\\meterB",
  arguments: 1,
  optionalArgument: false,
  evaluateArguments() {},
});

defineCommand({
  name: "\\measure",
  arguments: 1,
  optionalArgument: false,
  evaluateArguments() {},
});

defineCommand({
  name: "\\measureD",
  arguments: 1,
  optionalArgument: false,
  evaluateArguments() {},
});

defineCommand({
  name: "\\measuretab",
  arguments: 1,
  optionalArgument: false,
  evaluateArguments() {},
});

// Ghost cells.

defineCommand({
  name: "\\ghost",
  arguments: 1,
  optionalArgument: false,
  // We can ignore the arguments since this is just a placeholder.
  evaluateArguments(): void {},
});

defineCommand({
  name: "\\pureghost",
  arguments: 1,
  optionalArgument: false,
  evaluateArguments() {},
});

defineCommand({
  name: "\\cghost",
  arguments: 1,
  optionalArgument: false,
  evaluateArguments() {},
});

defineCommand({
  name: "\\nghost",
  arguments: 1,
  optionalArgument: false,
  evaluateArguments() {},
});

// Alignment.

defineCommand({
  name: "\\lstick",
  arguments: 1,
  optionalArgument: false,
  evaluateArguments() {},
});

defineCommand({
  name: "\\rstick",
  arguments: 1,
  optionalArgument: false,
  evaluateArguments() {},
});

defineCommand({
  name: "\\ustick",
  arguments: 1,
  optionalArgument: false,
  evaluateArguments() {},
});

defineCommand({
  name: "\\dstick",
  arguments: 1,
  optionalArgument: false,
  evaluateArguments() {},
});

// EXPLICIT WIRE.

// Horizontal.

defineCommand({
  name: "\\qw",
  arguments: 0,
  optionalArgument: true,
  evaluateArguments() {},
});

defineCommand({
  name: "\\qwa",
  arguments: 0,
  optionalArgument: true,
  evaluateArguments() {},
});

defineCommand({
  name: "\\cw",
  arguments: 0,
  optionalArgument: true,
  evaluateArguments() {},
});

defineCommand({
  name: "\\cwa",
  arguments: 0,
  optionalArgument: true,
  evaluateArguments() {},
});

// Vertical.

defineCommand({
  name: "\\qwx",
  arguments: 0,
  optionalArgument: true,
  evaluateArguments() {},
});

defineCommand({
  name: "\\cwx",
  arguments: 0,
  optionalArgument: true,
  evaluateArguments() {},
});

defineCommand({
  name: "\\ctrl",
  arguments: 1,
  optionalArgument: false,
  evaluateArguments() {},
});

defineCommand({
  name: "\\ctrlo",
  arguments: 1,
  optionalArgument: false,
  evaluateArguments() {},
});

defineCommand({
  name: "\\cctrl",
  arguments: 1,
  optionalArgument: false,
  evaluateArguments() {},
});

defineCommand({
  name: "\\cctrlo",
  arguments: 1,
  optionalArgument: false,
  evaluateArguments() {},
});

// Multi.

defineCommand({
  name: "\\barrier",
  arguments: 1,
  optionalArgument: true,
  evaluateArguments() {},
});

// Diagonal.

// MULTI CELLS.

defineCommand({
  name: "\\multigate",
  arguments: 2,
  optionalArgument: false,
  evaluateArguments() {},
});

defineCommand({
  name: "\\sgate",
  arguments: 2,
  optionalArgument: false,
  evaluateArguments() {},
});

defineCommand({
  name: "\\smeterB",
  arguments: 2,
  optionalArgument: false,
  evaluateArguments() {},
});

defineCommand({
  name: "\\multimeasure",
  arguments: 2,
  optionalArgument: false,
  evaluateArguments() {},
});

defineCommand({
  name: "\\multimeasureD",
  arguments: 2,
  optionalArgument: false,
  evaluateArguments() {},
});

// GLOBAL LABELS.

defineCommand({
  name: "\\gategroup",
  arguments: 6,
  optionalArgument: false,
  evaluateArguments() {},
});

defineCommand({
  name: "\\inputgroup",
  arguments: 4,
  optionalArgument: false,
  evaluateArguments() {},
});

defineCommand({
  name: "\\inputgroupv",
  arguments: 5,
  optionalArgument: false,
  evaluateArguments() {},
});

defineCommand({
  name: "\\inputgrouph",
  arguments: 5,
  optionalArgument: false,
  evaluateArguments() {},
});

// SPACING.

defineCommand({
  name: "\\push",
  arguments: 1,
  optionalArgument: false,
  evaluateArguments() {},
});

defineCommand({
  name: "\\cds",
  arguments: 2,
  optionalArgument: false,
  evaluateArguments() {},
});

defineCommand({
  name: "\\mbox",
  arguments: 1,
  optionalArgument: false,
  evaluateArguments() {},
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

interface CommandNode<
  NArgs extends number = number,
  OArg extends boolean = boolean,
  TArgs = void,
> {
  readonly type: NodeType.Command;

  readonly command: Command<NArgs, OArg, TArgs>;
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
    let optionalArgument: Node | undefined = undefined;
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
    for (let n = 0; n < command.arguments; n++) {
      if (this.tokenizer.current.type !== TokenType.OpenBrace) {
        throw new MissingArgumentError(command, n, this.tokenizer.current);
      }

      parsedArguments.push(this.parseGroup(false));
    }

    if (command.optionalArgument && optionalArgument !== undefined) {
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
