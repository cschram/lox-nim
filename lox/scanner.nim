import std/[options, strutils, strformat]

type
  LiteralKind* = enum
    lkNumber, lkString, lkTrue, lkFalse
  Literal* = ref object
    case kind*: LiteralKind
    of lkNumber:
      numberValue*: float
    of lkString:
      stringValue*: string
    of lkTrue, lkFalse:
      discard

proc newLiteral*(kind: LiteralKind; value: auto): Literal =
  result = Literal(kind: kind)
  case kind
  of lkNumber:
    result.numberValue = value
  of lkString:
    result.stringValue = value
  of lkTrue, lkFalse:
    discard

proc `$`*(l: Literal): string {.inline.} =
  case l.kind
  of lkNumber:
    result = $l.numberValue
  of lkString:
    result = "\"" & l.stringValue & "\""
  of lkTrue:
    result = "true"
  of lkFalse:
    result = "false"

type
  TokenKind* = enum
    # Single-character tokens
    tkLeftParen, tkRightParen, tkLeftBrace, tkRightBrace,
    tkComma, tkDot, tkMinus, tkPlus, tkSemicolon,
    tkSlash, tkStar,
    # One or two character tokens
    tkBang, tkBangEqual, tkEqual, tkEqualEqual,
    tkGreater, tkGreaterEqual, tkLess, tkLessEqual,
    # Literals
    tkIdentifier, tkString, tkNumber,
    # Keywords
    tkAnd, tkClass, tkElse, tkFalse, tkFun, tkFor,
    tkIf, tkNil, tkOr, tkPrint, tkReturn, tkSuper,
    tkThis, tkTrue, tkVar, tkWhile,
    # EOF
    tkEof
  TokenLocation = tuple
    line: int
    column: int
  Token* = ref object
    kind*: TokenKind
    lexeme*: Option[string]
    literal*: Option[Literal]
    location*: TokenLocation

proc `$`*(tk: TokenKind): string {.inline.} =
  result = $tk
  result.removePrefix("tk")

proc `$`*(t: Token): string {.inline.} =
  result = fmt"{t.kind}({$t.lexeme}, {$t.literal})"

type
  SyntaxError* = ref object
    message*: string
    location*: TokenLocation
  ScanResult* = ref object
    tokens*: seq[Token]
    errors*: seq[SyntaxError]
  ScanState = ref object
    source*: string
    tokens*: seq[Token]
    errors*: seq[SyntaxError]
    start*: int
    current*: int
    line*: int
    column*: int

proc isAtEnd(state: ScanState): bool =
  ## Checks if the scanner has reached the end of the source code.
  result = state.current >= state.source.len

proc getLexeme(state: ScanState): string =
  ## Retrieves the current lexeme being scanned.
  result = state.source[state.start ..< state.current]

proc addSyntaxError(state: ScanState; message: string) =
  ## Adds a syntax error to the state's error list.
  state.errors.add(SyntaxError(
    message: message,
    location: (line: state.line, column: state.column)
  ))

proc advance(state: ScanState): char =
  ## Advances the scanner by one character and returns that character.
  result = state.source[state.current]
  state.current += 1
  state.column += 1

proc peek(state: ScanState): char =
  ## Peeks at the current character without advancing the scanner.
  result = state.source[state.current]

proc peekLast(state: ScanState): char =
  ## Returns the previous character scanned.
  result = state.source[state.current - 1]

proc peekNext(state: ScanState): Option[char] =
  ## Peeks at the next character without advancing the scanner.
  if state.current + 1 >= state.source.len:
    result = none(char)
  else:
    result = some(state.source[state.current + 1])

proc isDigit(state: ScanState): bool =
  ## Checks if the given character is a digit.
  if state.peek() in '0'..'9':
    result = true
  elif state.peek() == '.':
    let next = state.peekNext()
    if next.isSome and next.get() in '0'..'9':
      result = true
    else:
      result = false
  else:
    result = false

proc addToken(state: ScanState; kind: TokenKind;
               literal: Option[Literal] = none(Literal)) =
  ## Adds a token of the given kind to the state's token list.
  state.tokens.add(Token(
    kind: kind,
    lexeme: some(state.getLexeme()),
    literal: literal,
    location: (line: state.line, column: state.column)
  ))

proc scanIdentifier(state: ScanState) =
  ## Scans an identifier or keyword from the source code.
  while not state.isAtEnd() and (state.peek().isAlphaNumeric() or state.peek() == '_'):
    discard state.advance()
  let
    kind = case state.getLexeme()
      of "and": tkAnd
      of "class": tkClass
      of "else": tkElse
      of "false": tkFalse
      of "fun": tkFun
      of "for": tkFor
      of "if": tkIf
      of "nil": tkNil
      of "or": tkOr
      of "print": tkPrint
      of "return": tkReturn
      of "super": tkSuper
      of "this": tkThis
      of "true": tkTrue
      of "var": tkVar
      of "while": tkWhile
      else: tkIdentifier
    literal = case kind
      of tkTrue:
        some(newLiteral(lkTrue, true))
      of tkFalse:
        some(newLiteral(lkFalse, false))
      else:
        none(Literal)
  state.addToken(kind, literal)

proc scanNumber(state: ScanState) =
  ## Scans a number literal from the source code.
  while not state.isAtEnd() and state.isDigit():
    discard state.advance()
  if not state.isAtEnd() and state.peek() == '.':
    let next = state.peekNext()
    if next.isSome and next.get().isDigit():
      discard state.advance() # Consume the '.'
      while not state.isAtEnd() and state.peek().isDigit():
        discard state.advance()
  let numberStr = state.getLexeme()
  let numberValue = parseFloat(numberStr)
  state.addToken(tkNumber, some(newLiteral(lkNumber, numberValue)))

proc scanToken(state: ScanState) =
  ## Scans a single token from the source code and adds it to the state's token
  ## list.
  discard

proc scan(source: string): ScanResult =
  ## Scans the given source code and returns the list of tokens and any syntax
  ## errors encountered.
  var state = ScanState(
    source: source,
    tokens: @[],
    errors: @[],
    start: 0,
    current: 0,
    line: 1,
    column: 1
  )
  while not state.isAtEnd():
    state.start = state.current
    state.scanToken()
  state.tokens.add(Token(kind: tkEof, lexeme: none(string),
                           literal: none(Literal),
                           location: (line: state.line, column: state.column)))
  result = ScanResult(tokens: state.tokens, errors: state.errors)
