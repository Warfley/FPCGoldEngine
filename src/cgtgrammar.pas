unit cgtgrammar;

{$mode ObjFPC}{$H+}
{$ModeSwitch arrayoperators}
{$TypedAddress On}

interface

uses
  Classes, SysUtils, lexer, parser, Generics.Collections;

type
  TFieldType = (ftBoolean = 'B',
                ftEmpty   = 'E',
                ftInteger = 'I',
                ftMulti   = 'M',
                ftString  = 'S',
                ftChar    = 'b');

  { TCGTReader }

  TCGTReader = class
  private
    FOwnsStream: Boolean;
    FStream: TStream;
    FRemainingSize: Integer;
  public
    constructor Create(AStream: TStream; OwnsStream: Boolean = True);
    destructor Destroy; override;

    function ReadRawString: String;

    function ReadInteger: Word;
    function ReadBoolean: Boolean;
    function ReadString: String;
    function ReadChar: Char;
    function StartRecord: Integer;
    function SkipField: TFieldType;

    function RecordFinished: Boolean; inline;
  end;

  TRecordType = (rtCharset       = 'C',
                 rtDFAState      = 'D',
                 rtInitialStates = 'I',
                 rtLRState       = 'L',
                 rtParameter     = 'P',
                 rtRule          = 'R', 
                 rtSymbol        = 'S',
                 rtCountsV1      = 'T',
                 rtCharRanges    = 'c',
                 rtGroup         = 'g',
                 rtProperty      = 'p',
                 rtCountsV5      = 't');

  TGrammarVersion = (gv1, gv5);
  TParameter = class(specialize THashMap<string, string>);

  { TCGTGrammar }

  TCGTGrammar = class
  protected
    FVersion: TGrammarVersion;
    FSymbols: TGrammarSymbolTable;
    FParameter: TParameter;
    FDFA: TDFALexer;
    FLALR: TLALRParser;

    procedure Clear;

  private
    function ReadHeader(Reader: TCGTReader): TGrammarVersion;
    function LexNextUnskippable(var Iterator: TUTF8Iterator): TToken; inline;
  public
    constructor Create;
    destructor Destroy; override;

    procedure LoadFromFile(const CGTFile: string);

    procedure Prepare(SymbolList: TGrammarSymbolList);

    function LexToken(var Iterator: TUTF8Iterator): TToken; inline;
    function ParseString(const AString: String; BuildNode: TBuildParseTreeEvent;
                         BuildLeaf: TBuildParseTreeLeafEvent;
                         FreeNodesOnReset: Boolean = True): TObject;

    property Version: TGrammarVersion read FVersion;
    property Symbols: TGrammarSymbolTable read FSymbols;
    property DFA: TDFALexer read FDFA;
    property LALR: TLALRParser read FLALR;
  end;

implementation 

{ TCGTReader }

constructor TCGTReader.Create(AStream: TStream; OwnsStream: Boolean);
begin
  inherited Create;
  FStream := AStream;
  FOwnsStream := OwnsStream;
end;

destructor TCGTReader.Destroy;
begin
  if FOwnsStream then
    FStream.Free;
  inherited Destroy;
end;

function TCGTReader.ReadRawString: String;
var
  WString: WideString;
  NextChar: WideChar = #00;
begin
  WString := '';
  repeat
    if FStream.Read(NextChar, SizeOf(NextChar)) < sizeof(NextChar) then
      raise EReadError.Create('Unexpected EOF');
    if NextChar <> #0000 then
      WString += NextChar;
  until NextChar = #0000;
  Result := UTF8Encode(WString);
end;

function TCGTReader.ReadInteger: Word;
var
  FieldType: TFieldType = ftEmpty;
begin
  Result := 0;
  if FStream.Read(FieldType, 1) < 1 then
     raise EReadError.Create('Unexpected EOF');
  if FieldType <> ftInteger then
    raise EReadError.Create('Expected Integer got ' + char(FieldType));
  if FStream.Read(Result, SizeOf(Result)) < SizeOf(Result) then
     raise EReadError.Create('Unexpected EOF');
  Dec(FRemainingSize);
end;

function TCGTReader.ReadBoolean: Boolean;
var
  FieldType: TFieldType = ftEmpty;
  IntVal: Byte = 0;
begin
  if FStream.Read(FieldType, 1) < 1 then
     raise EReadError.Create('Unexpected EOF');
  if FieldType <> ftBoolean then
    raise EReadError.Create('Expected Boolean got ' + char(FieldType));
  if FStream.Read(IntVal, SizeOf(IntVal)) < SizeOf(IntVal) then
     raise EReadError.Create('Unexpected EOF');
  Result := IntVal = 1;
  Dec(FRemainingSize);
end;

function TCGTReader.ReadString: String;
var
  FieldType: TFieldType = ftEmpty;
begin
  if FStream.Read(FieldType, 1) < 1 then
     raise EReadError.Create('Unexpected EOF');
  if FieldType <> ftString then
    raise EReadError.Create('Expected String got ' + char(FieldType));
  Result := ReadRawString;
  Dec(FRemainingSize);
end;

function TCGTReader.ReadChar: Char;
var
  FieldType: TFieldType = ftEmpty;
begin
  Result := #00;
  if FStream.Read(FieldType, 1) < 1 then
    raise EReadError.Create('Unexpected EOF');
  if FieldType <> ftChar then
    raise EReadError.Create('Expected Byte got ' + char(FieldType));
   if FStream.Read(Result, SizeOf(Result)) < SizeOf(Result) then
     raise EReadError.Create('Unexpected EOF');
  Dec(FRemainingSize);
end;

function TCGTReader.StartRecord: Integer;
var
  FieldType: TFieldType = ftEmpty;
  WordValue: Word = 0;
begin
  if FStream.Read(FieldType, 1) < 1 then
    Exit(-1);
  if FieldType <> ftMulti then
    raise EReadError.Create('Expected Multi got ' + char(FieldType));
  if FStream.Read(WordValue, SizeOf(WordValue)) < SizeOf(WordValue) then
    raise EReadError.Create('Unexpected EOF');
  FRemainingSize := WordValue;
  Result := WordValue;
end;

function TCGTReader.SkipField: TFieldType;
var
  Dummy: Integer = 0;
begin
  Result := ftEmpty;
  if FStream.Read(Result, 1) < 1 then
    raise EReadError.Create('Unexpected EOF');
  case Result of
  ftInteger:
    if FStream.Read(Dummy, SizeOf(Word)) < SizeOf(Word) then
      raise EReadError.Create('Unexpected EOF');
  ftBoolean, ftChar:
    if FStream.Read(Dummy, SizeOf(Byte)) < SizeOf(Byte) then
      raise EReadError.Create('Unexpected EOF');
  ftString:
    ReadRawString;
  ftEmpty:;
  else
    raise EReadError.Create('Unknown Field Type');
  end;
  Dec(FRemainingSize);
end;

function TCGTReader.RecordFinished: Boolean;
begin        
  if FRemainingSize < 0 then
    raise EReadError.Create('Overread Record');
  Result := FRemainingSize = 0;
end;

{ TCGTGrammar }

procedure TCGTGrammar.Clear;
begin
  FDFA.Free;
  FLALR.Free;
  FSymbols := [];
  FParameter.Clear;
  FDFA := TDFALexer.Create;
  FLALR := TLALRParser.Create;
end;

function TCGTGrammar.ReadHeader(Reader: TCGTReader): TGrammarVersion;
var
  Header: String;
begin
  Header := Reader.ReadRawString;
  if Header = 'GOLD Parser Tables/v1.0' then
    Result := gv1  
  else if Header = 'GOLD Parser Tables/v5.0' then
    Result := gv5
  else
    raise EReadError.Create('Unknown file version');
end;

function TCGTGrammar.LexNextUnskippable(var Iterator: TUTF8Iterator): TToken;
begin
  repeat
    Result := FDFA.NextToken(Iterator, FSymbols);
  until Result.Symbol^.SymbolType <> stSkippable;
end;

constructor TCGTGrammar.Create;
begin
  inherited Create;
  FSymbols := [];
  FDFA := TDFALexer.Create;
  FParameter := TParameter.Create;
  FLALR := TLALRParser.Create;
end;

destructor TCGTGrammar.Destroy;
begin  
  FDFA.Free;
  FLALR.Free;
  FParameter.Free;
  inherited Destroy;
end;

procedure ParseParameter(Reader: TCGTReader; Parameters: TParameter);
var
  Name, Author, Version, About: String;
  StartSymbol: Word;
  CaseSensetive: Boolean;
begin
  Name := Reader.ReadString;
  Version := Reader.ReadString;
  Author := Reader.ReadString;
  About := Reader.ReadString;
  CaseSensetive := Reader.ReadBoolean;
  StartSymbol := Reader.ReadInteger;

  Parameters.Add('Name', Name);
  Parameters.Add('Version', Version);
  Parameters.Add('Author', Author);
  Parameters.Add('About', About);
  Parameters.Add('Case Sensetive', CaseSensetive.ToString(True));
  Parameters.Add('Start Symbol', StartSymbol.ToString);
end; 

procedure ParseProperty(Reader: TCGTReader; Parameters: TParameter);
var
  Key, Value: String;
begin
  Reader.SkipField; // Index
  Key := Reader.ReadString;
  Value := Reader.ReadString;
  Parameters.Add(Key, Value);
end;

function ParseCharset(Reader: TCGTReader; NextIndex: Integer): TStaticCharset;
var
  Index: Word;
  CharString: String;
begin
  Index := Reader.ReadInteger;
  if Index <> NextIndex then
    raise EReadError.Create('Records out of order');
  CharString := Reader.ReadString;
  Result := TStaticCharset.FromString(CharString);
end;

function ParseCharRanges(Reader: TCGTReader; NextIndex: Integer): TRangeCharset;
var
  Index, CodePage, RangeEnd, RangeStart: Word;
  Ranges: TCodePointRanges;
  Count, i: Integer;
begin
  Index := Reader.ReadInteger;
  if Index <> NextIndex then
    raise EReadError.Create('Records out of order');
  CodePage := Reader.ReadInteger;
  Count := Reader.ReadInteger;
  Reader.SkipField; // Reserved
          
  Ranges := [];
  SetLength(Ranges, Count);
  for i:=0 to Count - 1 do
  begin
    RangeStart := Reader.ReadInteger;
    RangeEnd := Reader.ReadInteger;
    Ranges[i] := CodepointRange(RangeStart, RangeEnd);
  end;

  Result := TRangeCharset.FromArray(CodePage, Ranges);
end;

function ParseSymbol(Reader: TCGTReader; NextIndex: Integer): TGrammarSymbol;
var
  Index: Word;
  Name: String;
  SymbolType: TSymbolType;
begin
  Index := Reader.ReadInteger;
  if Index <> NextIndex then
    raise EReadError.Create('Records out of order');
  Name := Reader.ReadString;
  SymbolType := TSymbolType(Reader.ReadInteger);
  Result := Symbol(index, Name, SymbolType);
end;

procedure ParseDFAState(Reader: TCGTReader; DFA: TDFALexer);
var            
  Index, FinalResult: Integer;
  Edges: TDFAEdges;
  IsFinal: Boolean;
  charset, target: Word;
begin
  Index := Reader.ReadInteger;
  if Index <> DFA.States.Count then
    raise EReadError.Create('Records out of order');
  IsFinal := Reader.ReadBoolean;
  FinalResult := Reader.ReadInteger;
  if not IsFinal then
    FinalResult := -1;
  Reader.SkipField;

  Edges := [];
  while not Reader.RecordFinished do
  begin
    charset := Reader.ReadInteger;
    target := Reader.ReadInteger;
    Edges += [DFAEdge(charset, target)];
    Reader.SkipField; // Reserved
  end;

  DFA.AddState(FinalResult, Edges);
end;  

function ParseLRState(Reader: TCGTReader; NextIndex: Integer): TLRState;
var
  Index: Integer;
  Transitions: TLRTransitions;
  LookAhead, Value: Word;
  Action: TLRActionType;
begin
  Index := Reader.ReadInteger;
  if Index <> NextIndex then
    raise EReadError.Create('Records out of order');
  Reader.SkipField; // Reserved
  Transitions := [];
  while not Reader.RecordFinished do
  begin
    LookAhead := Reader.ReadInteger;
    Action := TLRActionType(Reader.ReadInteger);
    Value := Reader.ReadInteger;
    Reader.SkipField; // Reserved
    Transitions += [LRTransition(LookAhead, Action, Value)];
  end;

  Result := TLRState.Create(Transitions);
end;

function ParseLRRule(Reader: TCGTReader; NextIndex: Integer): TLRRule;
var
  Index: Integer;
  Consumes: TConsumes;
  Produces: Word;
begin
  Index := Reader.ReadInteger;
  if Index <> NextIndex then
    raise EReadError.Create('Records out of order');

  Produces := Reader.ReadInteger;
  Reader.SkipField; // Reserved

  Consumes := [];
  while not Reader.RecordFinished do
    Consumes += [Reader.ReadInteger];

  Result := LRRule(Produces, Consumes);
end;

function ParseGroup(Reader: TCGTReader; NextIndex: Integer): TLexicalGroup;
var
  Index, GroupSymbol, EndSymbol, StartSymbol: Word;
  Name: String;
  AdvanceMode: TGroupAdvance;
  EndMode: TGroupEnding;
  Count, i: Integer;
  Nestable: TNestableGroups;
begin
  Index := Reader.ReadInteger;
  if Index <> NextIndex then
    raise EReadError.Create('Records out of order');
  Name := Reader.ReadString;
  GroupSymbol := Reader.ReadInteger;
  StartSymbol := Reader.ReadInteger;
  EndSymbol := Reader.ReadInteger;
  AdvanceMode := TGroupAdvance(Reader.ReadInteger);
  EndMode := TGroupEnding(Reader.ReadInteger);
  Reader.SkipField; // RFU

  Count := Reader.ReadInteger;
  Nestable := [];
  SetLength(Nestable, Count);
  for i:=0 to Count - 1 do
    Nestable[i] := Reader.ReadInteger;

  Result := Group(Name, GroupSymbol, StartSymbol, EndSymbol, AdvanceMode, EndMode, Nestable);
end;

procedure TCGTGrammar.LoadFromFile(const CGTFile: string);
var
  Reader: TCGTReader;
  RecSize: Integer;
  RecordType: TRecordType;
  SymbolList: TGrammarSymbolList;
begin
  SymbolList := TGrammarSymbolList.Create;
  try
  Clear;
  Reader := TCGTReader.Create(TFileStream.Create(CGTFile, fmOpenRead));
  try
    FVersion := ReadHeader(Reader);
    While True do
    begin
      RecSize := Reader.StartRecord;
      if RecSize < 0 then
        Break; // EOF
      RecordType := TRecordType(Reader.ReadChar);
      case RecordType of
      rtParameter:
        ParseParameter(Reader, FParameter);
      rtProperty:
        ParseProperty(Reader, FParameter);
      rtCountsV1, rtCountsV5: // Not required in this implementation
        while not Reader.RecordFinished do
          Reader.SkipField;
      rtInitialStates:
      begin
        FDFA.SetInitialState(Reader.ReadInteger);
        Reader.SkipField;
      end;
      rtCharset:
        FDFA.AddCharset(ParseCharset(Reader, FDFA.Charsets.Count));
      rtCharRanges:
        FDFA.AddCharset(ParseCharRanges(Reader, FDFA.Charsets.Count));
      rtSymbol:
        SymbolList.Add(ParseSymbol(Reader, SymbolList.Count));
      rtDFAState:
        ParseDFAState(Reader, FDFA);
      rtLRState:
        FLALR.AddState(ParseLRState(Reader, FLALR.States.Count));
      rtRule:
        FLALR.AddRule(ParseLRRule(Reader, FLALR.Rules.Count));
      rtGroup:
        FDFA.AddGroup(ParseGroup(Reader, FDFA.Groups.Count));
      end;

      if not Reader.RecordFinished then
        raise EReadError.Create('Record not fully read');
    end;
  finally
    Reader.Free;
  end;

    Prepare(SymbolList);
  finally
    SymbolList.Free;
  end;
end;

procedure TCGTGrammar.Prepare(SymbolList: TGrammarSymbolList);
var
  EOFSymbol: Integer = -1;
  CommentStart: Integer = -1;
  CommentEnd: Integer = -1;
  CommentLine: Integer = -1;
  NewLineSymbol: Integer = -1;
  CommentSymbol: Integer = -1;
  i: Integer;
  CLSymbol, Sym: TGrammarSymbol;
begin
  // Ensure EOF Symbol in list
  for i:=0 to SymbolList.Count - 1 do
    if SymbolList[i].SymbolType = stEOF then
    begin
      EOFSymbol := i;
      break;
    end;
  if EOFSymbol < 0 then
    SymbolList.Add(Symbol(SymbolList.Count, '(EOF)', stEOF));

  // Upgrade v1 Groups
  if Version = gv1 then
  begin
    for i:=0 to SymbolList.Count - 1 do
      if SymbolList[i].SymbolType = stGroupStart then
        CommentStart := i
      else if SymbolList[i].SymbolType = stGroupEnd then
        CommentEnd := i
      else if SymbolList[i].SymbolType = stCommentLine then
        CommentLine := i
      else if (SymbolList[i].UnmangledName = 'newline') and
              ((SymbolList[i].SymbolType = stTerminal) Or
               (SymbolList[i].SymbolType = stSkippable)) then
        NewLineSymbol := i
      else if (SymbolList[i].UnmangledName = 'comment') and
              (SymbolList[i].SymbolType = stSkippable) then
        CommentSymbol := i;

    if CommentSymbol < 0 then
      CommentSymbol := SymbolList.Add(Symbol(SymbolList.Count, 'Comment', stSkippable));

    if (CommentStart >= 0) and (CommentEnd >= 0) then
      FDFA.AddGroup(Group('Comment Block', CommentSymbol, CommentStart, CommentEnd,
                          gaCharWise, geClosed, []));
    if (CommentLine >= 0) and (NewLineSymbol>= 0) then
    begin
      CLSymbol := SymbolList[CommentLine];
      CLSymbol.SymbolType := stGroupStart;
      SymbolList[CommentLine] := CLSymbol;
      FDFA.AddGroup(Group('Comment Line', CommentSymbol, CommentLine, NewLineSymbol,
                          gaCharWise, geOpen, []));
    end;
  end;

  // Create SymbolTable
  SetLength(FSymbols, SymbolList.Count);
  for i:=0 to SymbolList.Count - 1 do
    FSymbols[i] := SymbolList[i];

  // Prepare DFA
  FDFA.Prepare(FSymbols);
end;

function TCGTGrammar.LexToken(var Iterator: TUTF8Iterator): TToken;
begin
  Result := FDFA.NextToken(Iterator, FSymbols);
end;

function TCGTGrammar.ParseString(const AString: String;
  BuildNode: TBuildParseTreeEvent; BuildLeaf: TBuildParseTreeLeafEvent;
  FreeNodesOnReset: Boolean): TObject;
var
  Iterator: TUTF8Iterator;
  LookAhead: TToken;
  LALRAction: TLRActionType;
begin
  Iterator := IterateUTF8(AString);
  LALR.Reset;
  LALR.FreeNodes := FreeNodesOnReset;
  LALR.BuildNode := BuildNode;
  LALR.BuildLeaf := BuildLeaf;
  try
    LookAhead := LexNextUnskippable(Iterator);
    While True do
    begin
      LALRAction := FLALR.Step(LookAhead, FSymbols);
      if LALRAction = atShift then
        LookAhead := LexNextUnskippable(Iterator);
      if LALRAction = atAccept then
        break;
    end;
    Result := LALR.ExtractResult;
  finally
    LALR.Reset;
  end;
end;

end.

