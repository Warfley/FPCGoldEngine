unit parser;

{$mode ObjFPC}{$H+}
{$ModeSwitch advancedrecords}
{$TypedAddress On}

interface

uses
  SysUtils, Classes, lexer, Generics.Collections;

type
  EParserNotFinished = class(Exception);
  EUnknownSymbolException = class(Exception);
  EGotoNotFoundError = class(Exception);

  { EParserError }

  EParserError = class(Exception)
  public type
    TParseStackData = array of TObject;
  private
    FLookAhead: TToken;
    FStackData: TParseStackData;
    FFreeNodes: Boolean;
  public
    constructor Create(const Msg: String; const AToken: TToken;
                       const AStackData: TParseStackData;
                       FreeOnDestroy: Boolean);
    destructor Destroy; override;

    property LookAhead: TToken read FLookAhead;
    property StackData: TParseStackData read FStackData write FStackData;
    property FreeNodes: boolean read FFreeNodes write FFreeNodes;
  end;

  { ERuleNotApplicableError }

  ERuleNotApplicableError = class(EParserError)
  private
    FRuleIndex: Integer;
  public
    constructor Create(const Msg: String; const AToken: TToken;
  const AStackData: TParseStackData; FreeOnDestroy: Boolean; ARule: Integer);

    property RuleIndex: Integer read FRuleIndex;
  end;

  TLRActionType = (atShift = 1, atReduce, atGoto, atAccept);
  TLRTransition = record
    LookAhead: Integer;
    Action: TLRActionType;
    Value: Integer;
  end;
  TLRTransitions = array of TLRTransition;

  TConsumes = array of Integer;

  TLRRule = record
    Produces: Integer;
    Consumes: TConsumes;
  end;
  TLRRules = class(specialize TList<TLRRule>);

  TAcceptSymbols = class(specialize THashSet<Integer>);
  TActionTable = class(specialize THashMap<Integer, Integer>);

  { TLRState }

  TLRState = class
  private
    FAcceptSymbols: TAcceptSymbols;
    FShiftTable: TActionTable;
    FReduceTable: TActionTable;
    FGotoTable: TActionTable;
  public
    constructor Create(ATransitions: TLRTransitions);
    destructor Destroy; override;

    property AcceptSymbols: TAcceptSymbols read FAcceptSymbols;
    property ShiftTable: TActionTable read FShiftTable;
    property ReduceTable: TActionTable read FReduceTable;
    property GotoTable: TActionTable read FGotoTable;
  end;
  TLRStates = class(specialize TObjectList<TLRState>);

  // Using Runtime (Inheritance based TObject) Polymorhism
  // Reason: FPC won't allow me to make this generic
  TLRStackItem = record
    State: Integer;
    Symbol: Integer;
    Data: TObject;
  end;

  TLRParserStack = class(specialize TList<TLRStackItem>);
  TParseTreeNodes = array of TObject;

  TBuildParseTreeEvent = function(const Produces: TGrammarSymbol; Consumes: TParseTreeNodes): TObject of object;
  TBuildParseTreeLeafEvent = function(const AToken: TToken): TObject of object;

  { TLALRParser }

  TLALRParser = class
  private
    FStates: TLRStates;
    FRules: TLRRules;
    FInitialState: Integer;

    FBuildNode: TBuildParseTreeEvent;
    FBuildLeaf: TBuildParseTreeLeafEvent;
    FFreeNodes: Boolean;

    FStack: TLRParserStack;

    function PopStack(Count: SizeInt): TParseTreeNodes;
    function ParseTreeError(const Message: String; const LookAhead: TToken): EParserError;
    function RuleNotApplicableError(const Message: String; const LookAhead: TToken; ARule: Integer): ERuleNotApplicableError;
  public
    constructor Create;
    destructor Destroy; override;

    procedure AddState(AState: TLRState); inline;
    procedure AddRule(const ARule: TLRRule); inline;

    procedure SetInitialState(AState: Integer); inline;

    procedure Reset;

    function Step(LookAhead: TToken; const SymbolTable: TGrammarSymbolTable
      ): TLRActionType;

    function ExtractResult: TObject;

    property BuildNode: TBuildParseTreeEvent read FBuildNode write FBuildNode;
    property BuildLeaf: TBuildParseTreeLeafEvent read FBuildLeaf write FBuildLeaf;
    property FreeNodes: Boolean read FFreeNodes write FFreeNodes;
    property States: TLRStates read FStates;
    property Rules: TLRRules read FRules;
  end;

function LRTransition(LookAhead: Integer; Action: TLRActionType; Value: Integer): TLRTransition; inline;
function LRRule(Produces: Integer; const Consumes: TConsumes): TLRRule; inline;
implementation

function LRTransition(LookAhead: Integer; Action: TLRActionType; Value: Integer
  ): TLRTransition;
begin
  Result.Action := Action;
  Result.LookAhead := LookAhead;
  Result.Value := Value;
end;

function LRRule(Produces: Integer; const Consumes: TConsumes): TLRRule;
begin
  Result.Consumes := Consumes;
  Result.Produces := Produces;
end;

{ ERuleNotApplicableError }

constructor ERuleNotApplicableError.Create(const Msg: String;
  const AToken: TToken; const AStackData: TParseStackData;
  FreeOnDestroy: Boolean; ARule: Integer);
begin
  inherited Create(Msg, AToken, AStackData, FreeOnDestroy);
  FRuleIndex := ARule;
end;

{ EParserError }

constructor EParserError.Create(const Msg: String; const AToken: TToken;
  const AStackData: TParseStackData; FreeOnDestroy: Boolean);
begin
  inherited Create(Msg);
  FStackData := AStackData;
  FFreeNodes := FreeOnDestroy;
  FLookAhead := AToken;
end;

destructor EParserError.Destroy;
var
  i: Integer;
begin
  if FFreeNodes then
    for i:=0 to Length(FStackData) - 1 do
      FStackData[i].Free;
  inherited Destroy;
end;

{ TLALRParser }

function TLALRParser.PopStack(Count: SizeInt): TParseTreeNodes;
var
  i: SizeInt;
begin
  Result := [];
  if Count > FStack.Count then
    Count := FStack.Count;
  if Count < 0 then
    Count := 0;
  SetLength(Result, Count);
  for i:=Count - 1 downto 0 do
  begin
    Result[i] := FStack.Last.Data;
    FStack.Delete(FStack.Count - 1);
  end;
end;

function TLALRParser.ParseTreeError(const Message: String;
  const LookAhead: TToken): EParserError;
begin
  Result := EParserError.Create(Message, LookAhead, PopStack(FStack.Count), FFreeNodes);
end;

function TLALRParser.RuleNotApplicableError(const Message: String;
  const LookAhead: TToken; ARule: Integer): ERuleNotApplicableError;
begin
  Result := ERuleNotApplicableError.Create(Message, LookAhead, PopStack(FStack.Count), FFreeNodes, ARule);
end;

constructor TLALRParser.Create;
begin
  inherited Create;
  FStates := TLRStates.Create;
  FRules := TLRRules.Create;
  FInitialState := 0;
  FBuildNode := nil;
  FFreeNodes := True;

  FStack := TLRParserStack.Create;
end;

destructor TLALRParser.Destroy;
begin
  FStates.Free;
  FRules.Free;
  Reset;
  FStack.Free;
  inherited Destroy;
end;

procedure TLALRParser.AddState(AState: TLRState);
begin
  FStates.Add(AState);
end;

procedure TLALRParser.AddRule(const ARule: TLRRule);
begin
  FRules.Add(ARule);
end;

procedure TLALRParser.SetInitialState(AState: Integer);
begin
  FInitialState := AState;
end;

procedure TLALRParser.Reset;
begin
  while FStack.Count > 0 do
  begin
    if FFreeNodes then
      FStack.Last.Data.Free;
    FStack.Delete(FStack.Count - 1);
  end;
end;   

function StackItem(State: Integer; Symbol: Integer; const Data: TObject): TLRStackItem; inline;
begin
  Result.State := State;
  Result.Data := Data;
  Result.Symbol := Symbol;
end;

function TLALRParser.Step(LookAhead: TToken; const SymbolTable: TGrammarSymbolTable): TLRActionType;

var
  CurrentState, NewTop: TLRState;
  NextState: Integer;
  RuleIndex, i: Integer;
  Rule: TLRRule;
  Consumes: TParseTreeNodes;
  NewNode: TObject;
begin
  if not Assigned(LookAhead.Symbol) then
    raise EUnknownSymbolException.Create('Unknown symbol for lookahead token: ' + LookAhead.Value);

  if FStack.Count = 0 then
    CurrentState := FStates[FInitialState]
  else
    CurrentState := FStates[FStack.Last.State];

  if CurrentState.FAcceptSymbols.Contains(LookAhead.Symbol^.Index) then
    Exit(atAccept);

  if CurrentState.FShiftTable.TryGetValue(LookAhead.Symbol^.Index, NextState) then
  begin
    if Assigned(BuildLeaf) then
      NewNode := BuildLeaf(LookAhead)
    else
      NewNode := nil;
    FStack.Add(StackItem(NextState, LookAhead.Symbol^.Index, NewNode));
    Exit(atShift);
  end;

  if not CurrentState.FReduceTable.TryGetValue(LookAhead.Symbol^.Index, RuleIndex) then
    raise ParseTreeError('No action found for Look Ahead: ' + LookAhead.Symbol^.Name, LookAhead);


  Rule := FRules[RuleIndex];

  if FStack.Count < Length(Rule.Consumes) then
    raise RuleNotApplicableError('Not enough elements on the stack' , LookAhead, RuleIndex);

  for i:=0 to Length(Rule.Consumes) - 1 do
    if FStack[FStack.Count - Length(Rule.Consumes) + i].Symbol <> Rule.Consumes[i] then
      raise RuleNotApplicableError('Rule Symbols do not match Symbols, expected: '
                                  + SymbolTable[Rule.Consumes[i]].Name + ' got: '
                                  + SymbolTable[FStack[FStack.Count - Length(Rule.Consumes) + i].Symbol].Name,
                                   LookAhead, RuleIndex);

    Consumes := PopStack(Length(Rule.Consumes));
    if Assigned(BuildNode) then
      NewNode := BuildNode(SymbolTable[Rule.Produces], Consumes)
    else
      NewNode := nil;

    if FStack.Count = 0 then
      NewTop := FStates[FInitialState]
    else
      NewTop := FStates[FStack.Last.State];

    if not NewTop.GotoTable.TryGetValue(Rule.Produces, NextState) then
      raise EGotoNotFoundError.Create('No entry in goto table for ' + Rule.Produces.ToString);

    FStack.Add(StackItem(NextState, Rule.Produces, NewNode));
    Result := atReduce;
end;

function TLALRParser.ExtractResult: TObject;
begin
  if FStack.Count <> 1 then
    raise EParserNotFinished.Create('Parser not finished with an ACCEPT');
  Result := FStack.Last.Data;
  FStack.Clear;
end;

{ TLRState }

constructor TLRState.Create(ATransitions: TLRTransitions);
var
  Transition: TLRTransition;
begin
  inherited Create;
  FAcceptSymbols := TAcceptSymbols.Create;
  FShiftTable := TActionTable.Create;
  FReduceTable := TActionTable.Create;
  FGotoTable := TActionTable.Create;

  for Transition in ATransitions do
    if Transition.Action = atShift then
      FShiftTable.Add(Transition.LookAhead, Transition.Value)
    else if Transition.Action = atReduce then
      FReduceTable.Add(Transition.LookAhead, Transition.Value)
    else if Transition.Action = atGoto then
      FGotoTable.Add(Transition.LookAhead, Transition.Value)
    else if Transition.Action = atAccept then
      FAcceptSymbols.Add(Transition.LookAhead);
end;

destructor TLRState.Destroy;
begin
  FAcceptSymbols.Free;
  FShiftTable.Free;
  FReduceTable.Free;
  FGotoTable.Free;
  inherited Destroy;
end;

end.

