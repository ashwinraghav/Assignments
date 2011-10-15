unit FmEvalMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Math,
  Dialogs, StdCtrls, ExtCtrls, FmEvalCode, FmEvalDisplay,
  Sil, SilTokens, SilLexer, SilEval, SilScript;

const
  CN_PI       = 1001;
  CN_E        = 1002;
  FN_SIN      = 2001;
  FN_MAX      = 2002;
  FN_NOW      = 2003;
  FN_PRINT    = 2004;

type
  TFormTester = class(
    TForm,
    IDispatchable,
    ITokenHandler )
    lbVariables: TListBox;
    Panel1: TPanel;
    edCode: TMemo;
    Panel2: TPanel;
    Matches: TButton;
    Parse: TButton;
    Lexical: TButton;
    Evaluate: TButton;
    Script: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure LexicalClick(Sender: TObject);
    procedure ParseClick(Sender: TObject);
    procedure MatchesClick(Sender: TObject);
    procedure EvaluateClick(Sender: TObject);
    procedure edCodeChange(Sender: TObject);
    procedure lbVariablesKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure ParseScriptClick(Sender: TObject);
    procedure lbVariablesDblClick(Sender: TObject);
  private
    FTokens: ITokenTable;
    FStream: IMemoryStream;
    FDictionary: ILexDictionary;
    FLexer: ITokenLexer;
    FEval: IEvaluator;
    FReading: Boolean;
    FFormCode: TFormCode;
    FFormDisplay: TFormDisplay;
    procedure FnSin(var Msg: RFunctionMsg); message FN_SIN;
    procedure FnMax(var Msg: RFunctionMsg); message FN_MAX;
    procedure FnNow(var Msg: RFunctionMsg); message FN_NOW;
    procedure FnPrint(var Msg: RFunctionMsg); message FN_PRINT;
  protected
    function GetTokenValue(const Token: IToken): Variant;
    procedure SetTokenValue(const Token: IToken; const Value: Variant);
  end;

var
  FormTester: TFormTester;

implementation

uses
  SilSfEvalTokens,
  SilSfScriptParser, SilStScript;

{$R *.dfm}


function TokToStr(const Tok: IToken): string;
var
  Lexema: string;
begin
  if Assigned(Tok) then
  begin
    Lexema := Tok.Lexema;
    if Length(Lexema) > 0 then
      Lexema := Str.Quoted(Str.ReplaceControlChars(@Lexema[1], Length(Lexema)), '"');
    Result := Sil.Str.Format('%-20s %-10s: [%s] (%s)', [Tok.Name, Lexema, Token.ToStr(Tok.Symbol), Sil.Vart.ToStr(Tok.Value)]);
    if Token.ClassOf(Tok.Symbol) = tcGroup then
      Sil.Str.Add(Result, TokToStr(Tok.Linked), sLineBreak + '>> ');
  end else
    Result := '';
end;

procedure TFormTester.FormCreate(Sender: TObject);
begin
  FFormCode := TFormCode.Create(Self);
  FFormDisplay := TFormDisplay.Create(Self);
  FTokens := SilLexer.Tool.TokenTable();

  with SilEval.Tool do
  begin
    AddConstant(FTokens, 'pi',      System.PI,      CN_PI,            tvFloat);
    AddConstant(FTokens, 'e',       System.Exp(1),  CN_E,             tvFloat);
    AddFunction(FTokens, 'sin',     Self,           FN_SIN,     nil,  tvFloat);
    AddFunction(FTokens, 'max',     Self,           FN_MAX,     nil,  tvAny);
    AddFunction(FTokens, 'now',     Self,           FN_NOW,     nil,  tvDateTime);
    AddFunction(FTokens, 'print',   Self,           FN_PRINT,   nil,  tvVoid);
    //AddId      (FTokens, 'main',    Self);
  end;
    
  FStream := Sil.MemoryStream.Create();
  FDictionary := SilSfEvalTokens.DefineTokens(SilLexer.Dictionary.Create());
  FLexer := SilLexer.Tool.TokenLexer(FStream, FTokens, FDictionary);
  if Sil.Os.FileSystem.Exists('data.txt') then
  begin
    FReading := True;
    try
      edCode.Lines.LoadFromFile('data.txt');
    finally
      FReading := False;
    end;
  end;
  if Sil.Os.FileSystem.Exists('variables.txt') then
  begin
    FReading := True;
    try
      lbVariables.Items.LoadFromFile('variables.txt');
    finally
      FReading := False;
    end;
  end;
end;

procedure TFormTester.FormDestroy(Sender: TObject);
begin
  FStream := nil;
  FLexer := nil;
  FDictionary := nil;
end;

procedure TFormTester.MatchesClick(Sender: TObject);
var
  List: ILexMatches;
  Enum: IEnumerator;
  Item: ILexMatch;
  Text: string;
  Lexema: string;
  Len: Integer;
begin
  FFormCode.edMatches.Lines.BeginUpdate;
  try
    FFormCode.edMatches.Clear;

    Text := edCode.SelText;
    if Sil.Str.IsEmpty(Text) then
      Text := edCode.Text;
    Text := Text + ^Z;

    while Length(Text) > 0 do
    begin

      if FDictionary.Definitions.GetMatches(Text, List, True) then
      begin

        Len := 0;
        while List.Enumerate(Enum, Item) do
        begin
          Len := Item.Len;
          Lexema := Item.Lexema;
          if Length(Lexema) > 0 then
            Lexema := Str.Quoted(Str.ReplaceControlChars(@Lexema[1], Length(Lexema)), '"');
          with Token.Data(Item.Definition.Symbol) do
            FFormCode.edMatches.Lines.Add(Format('%-20s: %-10.10s (%d) [%s]', [Item.Definition.Name, Lexema, Item.Len, Token.ToStr(Item.Definition.Symbol)]));
        end;

        FFormCode.edMatches.Lines.Add('-----------');

        Text := Str.Copy(Text, Len + 1);

      end else
      begin
        FFormCode.edMatches.Lines.Add(Text);
        Text := '';
      end;

    end;
  finally
    FFormCode.edMatches.Lines.EndUpdate;
    FFormCode.Show;
  end;
end;

procedure TFormTester.LexicalClick(Sender: TObject);
var
  Text: string;
begin
  FFormCode.edMatches.Lines.BeginUpdate;
  try
    FFormCode.edMatches.Clear;

    Text := edCode.SelText;
    if Sil.Str.IsEmpty(Text) then
      Text := edCode.Text;
    Text := Text + ^Z;

    Sil.MemoryStream.Store(FStream, Text);
    FLexer.Reset;

    while FLexer.Current <> nil do
    begin
      FFormCode.edMatches.Lines.Add(TokToStr(FLexer.Current));
      FFormCode.edMatches.Lines.Add('');
      FLexer.Next;
    end;
    
  finally
    FFormCode.edMatches.Lines.EndUpdate;
    FFormCode.Show;
  end;
end;

procedure TFormTester.ParseClick(Sender: TObject);
var
  Counter: IPerformanceCounter;
  Text: string;
  Enum: IEvalMachine;
  Item: IEvalInstruction;
  Time: string;
begin
  Counter := Sil.Os.Performance.Create();
  
  FFormCode.edMatches.Lines.BeginUpdate;
  try
    FFormCode.edMatches.Clear;

    Text := edCode.SelText;
    if Sil.Str.IsEmpty(Text) then
      Text := edCode.Text;

    Counter.GetTime(True);
    
    FEval := SilEval.Tool.Create(Self, Text, FTokens);

    Time := Sil.Float.ToStr(Sil.Os.Performance.ToMSeconds(Counter, true), 7, 2);
    
    with FEval.Instructions do
      while Enumerate(Enum, Item) do
      begin
        FFormCode.edMatches.Lines.Add(Item.ToStr());
      end;

    Caption := '(' + Time + ' ms)';

  finally
    FFormCode.edMatches.Lines.EndUpdate;
    FFormCode.Show;
  end;
end;

procedure TFormTester.EvaluateClick(Sender: TObject);
var
  Counter: IPerformanceCounter;
  Value: string;
  Time: string;
begin
  FFormDisplay.edDisplay.Clear;

  Counter := Sil.Os.Performance.Create();
  FFormCode.edMatches.Lines.BeginUpdate;
  try
    FFormCode.edMatches.Clear;

    if FEval = nil then ParseClick(nil);

    Counter.GetTime(True);
    Value := Sil.Vart.ToStr(FEval.Execute());
    Time := Sil.Float.ToStr(Sil.Os.Performance.ToMSeconds(Counter, true), 7, 2);
    Caption := Value + '  (' + Time + ' ms)';   
  finally
    FFormCode.edMatches.Lines.EndUpdate;
  end;
end;

procedure TFormTester.edCodeChange(Sender: TObject);
begin
  if not FReading then edCode.Lines.SaveToFile('data.txt');
end;

function TFormTester.GetTokenValue(const Token: IToken): Variant;
begin
  lbVariables.Items.BeginUpdate;
  try
    if lbVariables.Items.IndexOfName(Token.Lexema) < 0 then
      lbVariables.Items.Add(Token.Lexema + '=0');
    Result := lbVariables.Items.Values[Token.Lexema];
  finally
    lbVariables.Items.EndUpdate;
  end;
end;

procedure TFormTester.SetTokenValue(const Token: IToken; const Value: Variant);
begin
  lbVariables.Items.Values[Token.Lexema] := Value;
  lbVariables.Items.SaveToFile('variables.txt');
end;

procedure TFormTester.lbVariablesKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_DELETE:
      with lbVariables do
        if ItemIndex <> -1 then
          Items.Delete(ItemIndex);
    VK_RETURN:
      lbVariablesDblClick(Sender);
    else
      Exit;
  end;
  lbVariables.Items.SaveToFile('variables.txt');
  Key := 0;
end;

procedure TFormTester.lbVariablesDblClick(Sender: TObject);
var
  Value: string;
begin
  with lbVariables do
    if ItemIndex <> -1 then
    begin
      Value := Items.Values[Items.Names[ItemIndex]];
      if InputQuery(Items.Names[ItemIndex], 'valor', Value) then
        Items.Values[Items.Names[ItemIndex]] := Value;
    end;
  lbVariables.Items.SaveToFile('variables.txt');
end;

procedure TFormTester.FnSin(var Msg: RFunctionMsg);
begin
  Msg.Return^ := Sin(Msg.Frame.Params['$1']);
end;

procedure TFormTester.FnMax(var Msg: RFunctionMsg);
var
  Max: Variant;
  Enum: IEnumerator;
  Item: RParameter;
begin
  Max := Sil.Vart.Unassigned;
  with Msg.Frame.Params do
    while Enumerate(Enum, Item) do
      if not Sil.Vart.IsOK(Max) or (Item.Value > Max) then
        Max := Item.Value;

  Msg.Return^ := Max; 
end;

procedure TFormTester.FnNow(var Msg: RFunctionMsg);
begin
  Msg.Return^ := Sil.DateTime.Now();
end;

procedure TFormTester.FnPrint(var Msg: RFunctionMsg);
var
  Enum: IEnumerator;
  Item: RParameter;
  Line: String;
begin
  Line := '';
  with Msg.Frame.Params do
    while Enumerate(Enum, Item) do
      if Sil.Vart.IsOK(Item.Value) then
        Sil.Str.Add(Line, Sil.Vart.ToStr(Item.Value));

  FFormDisplay.edDisplay.Lines.Add(Line);
  FFormDisplay.Show;
end;

procedure TFormTester.ParseScriptClick(Sender: TObject);
var
  Counter: IPerformanceCounter;
  Enum: IEvalMachine;
  Item: IEvalInstruction;
  Text: string;
  Line: string;
  Time: string;
begin
  Counter := Sil.Os.Performance.Create();
  
  FFormCode.edMatches.Lines.BeginUpdate;
  try
    FFormCode.edMatches.Clear;

    Text := edCode.SelText;
    if Sil.Str.IsEmpty(Text) then
      Text := edCode.Text;

    Counter.GetTime(True);
    
    FEval := SilScript.Tool.Create(Self, Text, FTokens);

    Time := Sil.Float.ToStr(Sil.Os.Performance.ToMSeconds(Counter, true), 7, 2);
    Line := '';
    with FEval.Instructions do
      while Enumerate(Enum, Item) do
      begin
        FFormCode.edMatches.Lines.Add(Item.ToStr());
      end;

    Caption := '(' + Time + ' ms)';

  finally
    FFormCode.edMatches.Lines.EndUpdate;
    FFormCode.Show;
  end;
end;

end.
