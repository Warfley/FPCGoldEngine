unit lexer;

{$mode ObjFPC}{$H+}
{$ModeSwitch advancedrecords}
{$ModeSwitch arrayoperators}
{$TypedAddress On}

interface

uses
  SysUtils, Classes, generics.Collections;

type

  { TUTF8Iterator }

  TUTF8Iterator = record
    Data: String;
    ScanHead: SizeInt;

    function GetEnumerator: TUTF8Iterator; inline;
    function GetCurrent: String; inline;
    function MoveNext: Boolean; inline;
    function MovePrevious: Boolean; inline;
    function EOS: Boolean; inline;
    procedure Backtrack(ToIndex: SizeInt);
    function CopyFrom(StartPos: SizeInt): String; inline;
    property Current: String read GetCurrent;
  end;

  TCharset = class
  public
    function CharInSet(const CharValue: String): Boolean; virtual; abstract;
  end;
  TCharsets = class(specialize TObjectList<TCharset>);

  { TStaticCharset }

  TStaticCharset = class(TCharset)
  private
    FCharset: specialize THashSet<String>;
  public
    constructor FromString(const Chars: String);
    destructor Destroy; override;

    function CharInSet(const CharValue: String): Boolean; override;
  end;

  { TCodePointRange }

  TCodePointRange = record
    RangeStart: Word;
    RangeEnd: Word;

    function InRange(CodePoint: Word): Boolean; inline;
  end;
  TCodePointRanges = Array of TCodePointRange;

  { TRangeCharset }

  TRangeCharset = class(TCharset)
  private
    FCodePage: Integer;
    FRanges: TCodePointRanges;
  private
    function DecodeUtf8(const CharValue: String): Word; inline;
  public
    constructor Create(const CodePage: Integer);
    constructor FromArray(const CodePage: Integer; const Ranges: TCodePointRanges);

    function CharInSet(const CharValue: String): Boolean; override;
  end;

  TSymbolType = (stNonTerminal = 0, stTerminal, stSkippable, stEOF,
                 stGroupStart, stGroupEnd, stCommentLine, stError);

  { TGrammarSymbol }

  TGrammarSymbol = record
  private
    GroupReference: Integer;
  public
    Index: Integer;
    SymbolType: TSymbolType;
    Name: String;
    function UnmangledName: String; inline;
  end;
  PGrammarSymbol = ^TGrammarSymbol;
  TGrammarSymbolList = class(specialize TList<TGrammarSymbol>);
  TGrammarSymbolTable = array of TGrammarSymbol;

  TDFALexer = class;

  TDFAEdge = record
    Target: Integer;
    Charset: Integer;
  end;
  TDFAEdges = array of TDFAEdge;

  { TDFAState }

  TDFAState = class
  private
    FDFA: TDFALexer;
    FFinalResult: Integer;
    FEdges: TDFAEdges;
  public
    constructor Create(DFA: TDFALexer; FinalResult: Integer; const Edges: TDFAEdges);

    function GetEdge(const NextChar: String): TDFAState;
    function IsFinal: Boolean; inline;

    property FinalResult: Integer read FFinalResult;
    property Edges[const NextChar: String]: TDFAState read GetEdge; default;
  end;
  TDFAStates = class(specialize TObjectList<TDFAState>);

  TToken = record
    Symbol: PGrammarSymbol;
    StartPos: SizeInt;
    Value: String;
    NestedTokens: Array of TToken;
  end;

  TGroupAdvance = (gaTokenwise = 0, gaCharWise);
  TGroupEnding = (geOpen = 0, geClosed);
  TNestableGroups = Array of Integer;

  TLexicalGroup = record
    Name: String;
    Symbol: Integer;
    StartSymbol: Integer;
    EndSymbol: Integer;
    AdvanceMode: TGroupAdvance;
    EndingMode: TGroupEnding;

    Nestable: TNestableGroups;
  end;
  TLexicalGroups = class(specialize TList<TLexicalGroup>);

  EPreparationError = class(Exception);

  { TDFALexer }

  TDFALexer = class
  private
    FCharSets: TCharsets;
    FStates: TDFAStates;
    FGroups: TLexicalGroups;
    FInitialState: Integer;
    FEOFSymbol: PGrammarSymbol;

  public
    constructor Create;
    destructor Destroy; override;

    procedure AddState(FinalResult: Integer; const Edges: TDFAEdges); inline;
    procedure AddCharset(Charset: TCharset); inline;
    procedure AddGroup(const AGroup: TLexicalGroup);
      inline;
    procedure SetInitialState(InitialState: SizeInt); inline;

    function Match(var Iterator: TUTF8Iterator;
      const SymbolTable: TGrammarSymbolTable): TToken;
    function NextToken(var Iterator: TUTF8Iterator;
      const SymbolTable: TGrammarSymbolTable): TToken;

    procedure Prepare(const SymbolTable: TGrammarSymbolTable);

    property Charsets: TCharsets read FCharSets;
    property Groups: TLexicalGroups read FGroups;
    property States: TDFAStates read FStates;
    property EOFSymbol: PGrammarSymbol read FEOFSymbol;
  end;

  // Exceptions:

  { ELexicalError }

  ELexicalError = class(Exception)
  private
    FInputString: String;
    FErrorPosition: SizeInt;
    FErrorChar: String;
  public
    constructor Create(const AString: String; AToken: TToken);

    property InputString: String read FInputString;
    property ErrorPosition: SizeInt read FErrorPosition;
    property ErrorChar: String read FErrorChar;
  end;

  { EGroupError }

  EGroupError = class(Exception)
  private
    FGroupName: String;
    FInputString: String;
    FStartPos: SizeInt;
  public
    constructor Create(const AString: String; const AGroupName: String;
                       const AStartPos: SizeInt);

    property InputString: String read FInputString;
    property GroupStartPosition: SizeInt read FStartPos;
    property GroupName: String read FGroupName;
  end;


const
  ErrorSymbol = nil;

function IterateUTF8(const AString: String; StartIndex: SizeInt = 0): TUTF8Iterator; inline;
function UTF8CharLen(FirstChar: Char): SizeInt; inline;
function InUTF8Sequence(CheckChar: Char): Boolean; inline;
function CodepointRange(RangeStart: Word; RangeEnd: Word): TCodePointRange; inline;
function DFAEdge(Charset: Integer; Target: Integer): TDFAEdge; inline;
function Symbol(Index: Integer; const Name: String; SymbolType: TSymbolType): TGrammarSymbol;
function Token(Symbol: PGrammarSymbol; Position: SizeInt; const Value: String): TToken; inline;
function Group(const GroupName: String; GroupSymbol: Integer;
               StartSymbol: Integer; EndSymbol: Integer;
               AdvanceMode: TGroupAdvance; EndingMode: TGroupEnding;
               NestableGroups: TNestableGroups): TLexicalGroup;
implementation                                             

function IterateUTF8(const AString: String; StartIndex: SizeInt): TUTF8Iterator;
begin
  Result.Data := AString;
  Result.ScanHead := StartIndex;
end;

function UTF8CharLen(FirstChar: Char): SizeInt;
begin
  Result := 1 + ord((ord(FirstChar) And %11000000) = %11000000)
              + ord((ord(FirstChar) And %11100000) = %11100000)
              + ord((ord(FirstChar) And %11110000) = %11110000);
end;

function InUTF8Sequence(CheckChar: Char): Boolean;
begin
  Result := (ord(CheckChar) And %11000000) = %10000000;
end;

function CodepointRange(RangeStart: Word; RangeEnd: Word): TCodePointRange;
begin
  Result.RangeStart := RangeStart;
  Result.RangeEnd := RangeEnd;
end;

function DFAEdge(Charset: Integer; Target: Integer): TDFAEdge;
begin
  Result.Charset := Charset;
  Result.Target := Target;
end;

function Symbol(Index: Integer; const Name: String; SymbolType: TSymbolType
  ): TGrammarSymbol;
begin
  Result.Index := Index;
  // Name mangling to ensure that symbols of different kinds have different names
  case SymbolType of
  stNonTerminal: 
    Result.Name := '<' + Name + '>';
  stTerminal:
    Result.Name := '''' + Name + '''';
  stSkippable:
    Result.Name := '[' + Name + ']';
  stEOF:      
    Result.Name := '(EOF)';
  stGroupStart:  
    Result.Name := '^' + Name + '^';
  stGroupEnd:    
    Result.Name := 'v' + Name + 'v';
  stCommentLine:
    Result.Name := '/' + Name + '/';
  end;
  Result.SymbolType := SymbolType;
  Result.GroupReference := -1;
end;

function Token(Symbol: PGrammarSymbol; Position: SizeInt; const Value: String): TToken;
begin
  Result.StartPos := Position;
  Result.Symbol := Symbol;
  Result.Value := Value;
  Result.NestedTokens := [];
end;

function Group(const GroupName: String; GroupSymbol: Integer;
  StartSymbol: Integer; EndSymbol: Integer; AdvanceMode: TGroupAdvance;
  EndingMode: TGroupEnding; NestableGroups: TNestableGroups): TLexicalGroup;
begin
  Result.Name := GroupName;
  Result.Symbol := GroupSymbol;
  Result.StartSymbol := StartSymbol;
  Result.EndSymbol := EndSymbol;
  Result.AdvanceMode := AdvanceMode;
  Result.EndingMode := EndingMode;
  Result.Nestable := NestableGroups;
end;

{ EGroupError }

constructor EGroupError.Create(const AString: String; const AGroupName: String;
  const AStartPos: SizeInt);
begin
  FInputString := AString;
  FGroupName := AGroupName;
  FStartPos := AStartPos;

  inherited CreateFmt('Unclosed Group %s at %d', [FGroupName, FStartPos]);
end;

{ ELexicalError }

constructor ELexicalError.Create(const AString: String; AToken: TToken);
begin
  FInputString := AString;
  FErrorChar := AToken.Value;
  FErrorPosition := AToken.StartPos;

  inherited CreateFmt('Unreckognizable Token %s at %d', [FErrorChar, FErrorPosition]);
end;

{ TGrammarSymbol }

function TGrammarSymbol.UnmangledName: String;
begin
  Result := Name.Substring(1, Name.Length - 2).ToLower;
end;

{ TUTF8Iterator }

function TUTF8Iterator.GetEnumerator: TUTF8Iterator;
begin
  Result := Self;
end;

function TUTF8Iterator.GetCurrent: String;
var
  CharLen: SizeInt;
begin
  CharLen := UTF8CharLen(Data[ScanHead]);
  Result := Copy(Data, ScanHead, CharLen);
end;

function TUTF8Iterator.MoveNext: Boolean;
begin
  if ScanHead < 1 then
    Inc(ScanHead)
  else if ScanHead <= Data.Length then
    Inc(ScanHead, UTF8CharLen(Data[ScanHead]));
  Result := ScanHead <= Data.Length;
end;

function TUTF8Iterator.MovePrevious: Boolean;
begin
  if ScanHead > Data.Length then
    ScanHead := Data.Length;
  if ScanHead <= 0 then
    Exit(False);
  Dec(ScanHead);
  while (ScanHead > 0) And InUTF8Sequence(Data[ScanHead]) do
    Dec(ScanHead);
  Result := ScanHead > 0;
end;

function TUTF8Iterator.EOS: Boolean;
begin
  Result := (ScanHead > Data.Length) Or (Data.Length = 0);
end;

procedure TUTF8Iterator.Backtrack(ToIndex: SizeInt);
begin
  ScanHead := ToIndex;
  if InUTF8Sequence(Data[ScanHead]) then
    MovePrevious;
end;

function TUTF8Iterator.CopyFrom(StartPos: SizeInt): String;
begin
  Result := Copy(Data, StartPos, ScanHead - StartPos + 1);
end;

{ TStaticCharset }

constructor TStaticCharset.FromString(const Chars: String);
var
  CharElem: String;
begin 
  inherited Create;
  FCharset := (specialize THashSet<string>).Create;
  for CharElem in IterateUTF8(Chars) do
    FCharset.Add(CharElem);
end;

destructor TStaticCharset.Destroy;
begin
  FCharset.Free;
  inherited Destroy;
end;

function TStaticCharset.CharInSet(const CharValue: String): Boolean;
begin
  Result := FCharset.Contains(CharValue);
end;

{ TCodePointRange }

function TCodePointRange.InRange(CodePoint: Word): Boolean;
begin
 Result := (CodePoint >= RangeStart) And
           (CodePoint <= RangeEnd);
end;

{ TRangeCharset }

function TRangeCharset.DecodeUtf8(const CharValue: String): Word;
var
  ConvLen: SizeUInt;
begin
  // TODO: Handle codepage
  ConvLen := Utf8ToUnicode(PUnicodeChar(@Result), 1, @CharValue[1], CharValue.Length);
  Assert(ConvLen = 1, 'Should be exactly 1 widechar');
end;

constructor TRangeCharset.Create(const CodePage: Integer);
begin
 inherited Create;
 FCodePage := CodePage;
end;

constructor TRangeCharset.FromArray(const CodePage: Integer;
 const Ranges: TCodePointRanges);
begin
  Create(CodePage);
  FRanges := Ranges;
end;

function TRangeCharset.CharInSet(const CharValue: String): Boolean;
var
  CodePoint: Word;
  i: SizeInt;
begin
  CodePoint := DecodeUtf8(CharValue);
  Result := False;
  for i:=0 to Length(FRanges) - 1 do
    if FRanges[i].InRange(CodePoint) then
      Exit(True);
end;   

{ TDFAState }

constructor TDFAState.Create(DFA: TDFALexer; FinalResult: Integer;
  const Edges: TDFAEdges);
begin
  inherited Create;
  FDFA := DFA;
  FFinalResult := FinalResult;
  FEdges := Edges;
end;

function TDFAState.GetEdge(const NextChar: String): TDFAState;
var
  i: Integer;
begin
  Result := nil;
  for i:=0 to Length(FEdges) - 1 do
    if FDFA.FCharSets[FEdges[i].Charset].CharInSet(NextChar) then
      Exit(FDFA.FStates[FEdges[i].Target]);
end;

function TDFAState.IsFinal: Boolean;
begin
  Result := FFinalResult >= 0;
end;

{ TDFALexer }

constructor TDFALexer.Create;
begin
  inherited Create;
  FCharSets := TCharsets.Create;
  FStates := TDFAStates.Create;
  FGroups := TLexicalGroups.Create;
  FEOFSymbol := nil;
  FInitialState := 0;
end;

destructor TDFALexer.Destroy;
begin
  FStates.Free;
  FCharSets.Free;
  FGroups.Free;
  inherited Destroy;
end;

procedure TDFALexer.AddState(FinalResult: Integer; const Edges: TDFAEdges);
begin
  FStates.Add(TDFAState.Create(Self, FinalResult, Edges));
end;

procedure TDFALexer.AddCharset(Charset: TCharset);
begin
  FCharsets.Add(Charset);
end;

procedure TDFALexer.AddGroup(const AGroup: TLexicalGroup);
begin
  FGroups.Add(AGroup);
end;

procedure TDFALexer.SetInitialState(InitialState: SizeInt);
begin
  FInitialState := InitialState;
end;

function TDFALexer.Match(var Iterator: TUTF8Iterator;
                         const SymbolTable: TGrammarSymbolTable): TToken;
var
  CurrentState: TDFAState;
  StartPos: SizeInt;
begin
  // Check for EOF
  if not Iterator.MoveNext then
  begin
    Result := Token(FEOFSymbol, Iterator.ScanHead, '(EOF)');
    Exit;
  end;
  StartPos := Iterator.ScanHead;
  // Default result if we don't match anything
  Result := Token(ErrorSymbol, StartPos, Iterator.GetCurrent);
  // Run DFA
  CurrentState := FStates[FInitialState];
  // Assume that the initial state cannot be a final state, because matching an
  // empty token would result in a never ending lexer
  repeat
    CurrentState := CurrentState[Iterator.GetCurrent];
    if Assigned(CurrentState) and CurrentState.IsFinal then
      Result := Token(@SymbolTable[CurrentState.FinalResult],
                      StartPos, Iterator.CopyFrom(StartPos));
  until not Assigned(CurrentState) or not Iterator.MoveNext;
  // Backtrack will always backtrack to the last UTF-8 char, so -1 can be used
  Iterator.Backtrack(Result.StartPos + Result.Value.Length - 1);
end;

type
  TGroupStackItem = record
    Token: TToken;
    GroupReference: Integer;
  end;
  TGroupStack = specialize TList<TGroupStackItem>;

function TDFALexer.NextToken(var Iterator: TUTF8Iterator;
                             const SymbolTable: TGrammarSymbolTable): TToken;

function GroupStackItem(const StartToken: TToken; GroupReference: Integer): TGroupStackItem; {$IFDEF INLINE}inline;{$ENDIF}
begin
  Result.Token := Token(@SymbolTable[FGroups[GroupReference].Symbol], StartToken.StartPos, StartToken.Value);
  Result.GroupReference := GroupReference;
end;

var
  GroupStack: TGroupStack;
  CurrentGroup: TLexicalGroup;
  IsNestable: Boolean;
  i: Integer;
begin
  Result := Match(Iterator, SymbolTable);

  if not Assigned(Result.Symbol) then
    raise ELexicalError.Create(Iterator.Data, Result);

  if Result.Symbol^.SymbolType <> stGroupStart then
    Exit;

  GroupStack := TGroupStack.Create;
  try
    GroupStack.Add(GroupStackItem(Result, Result.Symbol^.GroupReference));
    while not Iterator.EOS do
    begin
      CurrentGroup := FGroups[GroupStack.Last.GroupReference];
      Result := Match(Iterator, SymbolTable);
      if not Assigned(Result.Symbol) then // Unknown token parsed, skip
        Continue;
      // Check for Beginning of Group
      if Result.Symbol^.SymbolType = stGroupStart then
      begin
        IsNestable := False;
        for i:=0 to Length(CurrentGroup.Nestable) - 1 do
          if CurrentGroup.Nestable[i] = Result.Symbol^.GroupReference then
          begin
            IsNestable := True;
            Break;
          end;
        if IsNestable then
        begin
          GroupStack.Add(GroupStackItem(Result, Result.Symbol^.GroupReference));
          Continue;
        end;
      end;
      // Check if end of current group
      if CurrentGroup.EndSymbol = Result.Symbol^.Index then
      begin
        // Get token from stack
        Result := GroupStack.Last.Token; 
        // Newline hack: Don't consume newline token
        // So we backtrack to before this token
        if Result.Symbol^.UnmangledName = 'newline' then
          Iterator.Backtrack(Result.StartPos - 1);
        // Update token text to include all up to (and including) end Token
        Result.Value := Iterator.CopyFrom(Result.StartPos);
        // Pop: If last, return, if not update new stack
        GroupStack.Delete(GroupStack.Count - 1);
        if GroupStack.Count > 0 then
          GroupStack.Last.Token.NestedTokens += [Result]
        else // Last group element poped, return
          Exit;
        Continue;
      end;

      // If charwise advance, backtrack
      if CurrentGroup.AdvanceMode = gaCharWise then
        Iterator.Backtrack(Result.StartPos);
    end;

    // We only get here when we Reached EOF
    // So we close all open ended groups still open
    while GroupStack.Count > 0 do
    begin
      CurrentGroup := FGroups[GroupStack.Last.GroupReference];
      if CurrentGroup.EndingMode = geOpen then
      begin
        // Get token from stack
        Result := GroupStack.Last.Token;
        // Update token text to include all up to (and including) end Token
        Result.Value := Iterator.CopyFrom(Result.StartPos);
        // Pop: If last, return, if not update new stack
        GroupStack.Delete(GroupStack.Count - 1);
      end
      else
        raise EGroupError.Create(Iterator.Data, CurrentGroup.Name, GroupStack.Last.Token.StartPos);
    end;
  finally
    GroupStack.Free;
  end;
end;

procedure TDFALexer.Prepare(const SymbolTable: TGrammarSymbolTable);
var
  i: Integer;
begin
  // Find EOF Symbol
  FEOFSymbol := nil;
  for i:=0 to Length(SymbolTable) - 1 do
    if SymbolTable[i].SymbolType = stEOF then
    begin
      FEOFSymbol := @SymbolTable[i];
      break;
    end;
  if not Assigned(FEOFSymbol) then
    raise EPreparationError.Create('No EOF found in SymbolTable');

  // Backlink group symbols
  for i:=0 to FGroups.Count - 1 do
  begin
    SymbolTable[FGroups[i].StartSymbol].GroupReference := i;
    SymbolTable[FGroups[i].EndSymbol].GroupReference := i;
  end;
end;

end.

