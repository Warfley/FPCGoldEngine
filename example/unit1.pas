unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, ExtCtrls,
  EditBtn, StdCtrls, Buttons, SynEdit, SynEditHighlighter, cgtloader, lexer, parser;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    FileNameEdit1: TFileNameEdit;
    OpenDialog1: TOpenDialog;
    Panel1: TPanel;
    Splitter1: TSplitter;
    SynEdit1: TSynEdit;
    TreeView1: TTreeView;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FGrammar: TCGTGrammar; 
    function LeafNode(const AToken: TToken): TTreeNode;
    function ReduceNode(const Produces: TGrammarSymbol; Consumes: specialize TArray<TTreeNode>): TTreeNode;
    procedure MoveToParent(Node, NewParent: TTreeNode);
  public

  end;

  { TGrammarHighlighter }

  TGrammarHighlighter = class(TSynCustomHighlighter)
  private
    FGrammar: TCGTGrammar;
    FLastMatch: TToken;
    FIterator: TUTF8Iterator;
    FAttributes: Array of TSynHighlighterAttributes;
  public
    constructor WithDFA(AOwner: TComponent; AGrammar: TCGTGrammar);
    destructor Destroy; override;

    procedure SetLine(const NewValue: String; LineNumber: Integer); override;
    procedure Next; override;
    function GetEol: Boolean; override;
    function GetToken: String; override;
    function GetTokenPos: Integer; override;
    function GetTokenKind: integer; override;
    function GetTokenAttribute: TSynHighlighterAttributes; override;
    procedure GetTokenEx(out TokenStart: PChar; out TokenLength: integer);
      override;
    function GetDefaultAttribute(Index: integer): TSynHighlighterAttributes;
  override;
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TGrammarHighlighter }

constructor TGrammarHighlighter.WithDFA(AOwner: TComponent;
  AGrammar: TCGTGrammar);
var
  i: Integer;
begin
  inherited Create(AOwner);
  FGrammar := AGrammar;
  SetLength(FAttributes, 10);
  for i:=0 to Length(FAttributes) - 1 do
  begin
    FAttributes[i] := TSynHighlighterAttributes.Create(i.ToString);
    FAttributes[i].Foreground := Random($FFFFFF);
  end;
end;

destructor TGrammarHighlighter.Destroy;
var
  Attr: TSynHighlighterAttributes;
begin
  for Attr in FAttributes do
    Attr.Free;
  inherited Destroy;
end;

procedure TGrammarHighlighter.SetLine(const NewValue: String;
  LineNumber: Integer);
begin
  inherited SetLine(NewValue, LineNumber);
  FIterator := IterateUTF8(NewValue); 
  FLastMatch := FGrammar.LexToken(FIterator);
end;

procedure TGrammarHighlighter.Next;
begin
  FLastMatch := FGrammar.LexToken(FIterator);
end;

function TGrammarHighlighter.GetEol: Boolean;
begin
  Result := FLastMatch.Symbol^.SymbolType = stEOF;
end;

function TGrammarHighlighter.GetToken: String;
begin
  Result := FLastMatch.Value;
end;

function TGrammarHighlighter.GetTokenPos: Integer;
begin
  Result := FLastMatch.StartPos;
end;

function TGrammarHighlighter.GetTokenKind: integer;
begin
  Result := FLastMatch.Symbol^.Index;
end;

function TGrammarHighlighter.GetTokenAttribute: TSynHighlighterAttributes;
begin
  Result := FAttributes[abs(FLastMatch.Symbol^.Index) mod Length(FAttributes)];
end;

procedure TGrammarHighlighter.GetTokenEx(out TokenStart: PChar; out
  TokenLength: integer);
begin
  TokenStart := @FIterator.Data[FLastMatch.StartPos];
  TokenLength := FLastMatch.Value.Length;
end;

function TGrammarHighlighter.GetDefaultAttribute(Index: integer
  ): TSynHighlighterAttributes;
begin
  Result := FAttributes[0];
end;

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  FGrammar := TCGTGrammar.Create;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FGrammar.Free;
end;

function TForm1.LeafNode(const AToken: TToken): TTreeNode;
begin
  Result := TreeView1.Items.Add(nil, AToken.Symbol^.Name);
end;

function TForm1.ReduceNode(const Produces: TGrammarSymbol; Consumes: specialize
  TArray<TTreeNode>): TTreeNode;
var
  Node: TTreeNode;
begin
  Result := TreeView1.Items.Add(nil, Produces.Name);
  for Node in Consumes do
    MoveToParent(Node, Result);
end;

procedure TForm1.MoveToParent(Node, NewParent: TTreeNode);
var
  NewChild: TTreeNode;
  i: Integer;
begin
  NewChild := TreeView1.Items.AddChild(NewParent, Node.Text);
  while Node.HasChildren do
    MoveToParent(Node.Items[0], NewChild);
  Node.Free;
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  Highlighter: TSynCustomHighlighter;
begin 
  if not OpenDialog1.Execute then Exit;
  SynEdit1.Lines.LoadFromFile(OpenDialog1.FileName);
  FGrammar.LoadFromFile(FileNameEdit1.FileName);
  Highlighter := SynEdit1.Highlighter;
  SynEdit1.Highlighter := TGrammarHighlighter.WithDFA(SynEdit1, FGrammar);
  Highlighter.Free;
  FGrammar.ParseString(SynEdit1.Text, TBuildParseTreeEvent(@ReduceNode), TBuildParseTreeLeafEvent(@LeafNode));
end;

end.

