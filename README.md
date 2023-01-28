# FPC GOLD Engine

This is an engine for the GOLD Parsing System (http://goldparser.org/) written in FreePascal.

See the example for a small program that builds a parse tree and uses the Lexer to Colorize a `TSynEdit`

## Features
* v1.0 and v5.0 support
* event based parse tree construction

## Example Usage:
```pascal
function TMyClass.BuildTreeNode(const Produces: TGrammarSymbol; Consumes: specialize TArray<TObject>): TObject;
begin
  Result := TParseTree.Create(Produces.Name, Consumes);
end;

function TMyClass.BuildLeafNode(const AToken: TToken): TObject;
begin
  Result := TParseTreeLeaf.Create(AToken.Name);
end;

...

  Grammar := TCGTGrammar.Create;
  try
    Grammar.LoadFromFile(FileName);
    ParseTree := Grammar.ParseString(InputString, @BuildTreeNode, @BuildLeafNode);
  finally
    Grammar.Free;
  end;
```

## Documentation
Currently no documentation available specifically for the node engine.

For general engine design and how to construct your own engine check out https://github.com/Warfley/goldengine/docs
