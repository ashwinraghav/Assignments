unit FmEvalMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Math,
  Dialogs, StdCtrls, ExtCtrls, FmEvalCode, FmEvalDisplay,
  Sil, SilTokens, SilLexer, SilHttp;

type
  TFormTester = class(TForm, IDispatchable)
    Panel1: TPanel;
    edCode: TMemo;
    Panel2: TPanel;
    Matches: TButton;
    Lexical: TButton;
    Parse: TButton;
    Dump: TButton;
    Modify: TButton;
    Response: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure LexicalClick(Sender: TObject);
    procedure MatchesClick(Sender: TObject);
    procedure edCodeChange(Sender: TObject);
    procedure RequestClick(Sender: TObject);
    procedure DumpClick(Sender: TObject);
    procedure ModifyClick(Sender: TObject);
    procedure ResponseClick(Sender: TObject);
  private
    FTokens: ITokenTable;
    FStream: IMemoryStream;
    FDictionary: ILexDictionary;
    FLexer: ITokenLexer;
    FFiler: IHttpFiler;
    FReading: Boolean;
    FFormCode: TFormCode;
    FFormDisplay: TFormDisplay;
    FHttp: IHttpObject;
  protected
    function GetTokenValue(const Token: IToken): Variant;
    procedure SetTokenValue(const Token: IToken; const Value: Variant);
  end;

var
  FormTester: TFormTester;

implementation

uses
  SilSfHttpTokens,
  SilSmHttpRequest, SilSmHttpResponse, SilStHttp, SilSiHttpData;

{$R *.dfm}        

function TokToStr(const Tok: IToken; ShowLexema: Boolean = True): string;
var
  Lexema: string;
begin
  Result := '';
  if Assigned(Tok) then
  begin
    if ShowLexema then
    begin
      Lexema := Tok.Lexema;
      if Length(Lexema) > 0 then
        Lexema := Str.ReplaceControlChars(@Lexema[1], Length(Lexema));
      Str.Add(Result, '''' + Lexema + '''', sLineBreak);
    end;
    Str.Add(Result, '   %-20s [%s] (%s)', [Tok.Name, Token.ToStr(Tok.Symbol), Sil.Vart.ToStr(Tok.Value)], sLineBreak);
    if Token.ClassOf(Tok.Symbol) = tcGroup then
      Sil.Str.Add(Result, TokToStr(Tok.Linked, False), sLineBreak + '>> ');
  end else
    Result := '';
end;

procedure TFormTester.FormCreate(Sender: TObject);
begin
  FFormCode := TFormCode.Create(Self);
  FFormDisplay := TFormDisplay.Create(Self);
  FTokens := SilLexer.Tool.TokenTable();

  FStream := Sil.MemoryStream.Create();
  FDictionary := SilSfHttpTokens.DefineTokens(SilLexer.Dictionary.Create());
  FLexer := SilLexer.Tool.TokenLexer(FStream, FTokens, FDictionary);
  FFiler := SilHttp.Tk.Filer();
  
  if Sil.Os.FileSystem.Exists('data.txt') then
  begin
    FReading := True;
    try
      edCode.Lines.LoadFromFile('data.txt');
    finally
      FReading := False;
    end;
  end;
end;

procedure TFormTester.FormDestroy(Sender: TObject);
begin
  FFiler := nil;
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

    while Length(Text) > 0 do
    begin

      if FDictionary.Definitions.GetMatches(Text, List, True) then
      begin

        Len := 0;
        Lexema := '';
        while List.Enumerate(Enum, Item) do
          with Token.Data(Item.Definition.Symbol) do
            if TokenKind <> tkPrivate then
              begin
                Len := Item.Len;
                if Str.IsEmpty(Lexema) then
                begin
                  Lexema := Item.Lexema;
                  if Length(Lexema) > 0 then
                    Lexema := Str.ReplaceControlChars(@Lexema[1], Length(Lexema));
                  FFormCode.edMatches.Lines.Add('''' + Lexema + '''');
                end;
                FFormCode.edMatches.Lines.Add(Format('%-20s: (%d) [%s]', [Item.Definition.Name, Item.Len, Token.ToStr(Item.Definition.Symbol)]));
              end;

        FFormCode.edMatches.Lines.Add('----------------------------------------');

        Text := Str.Copy(Text, Len + 1);

      end else
      begin
        FFormCode.edMatches.Lines.Add(Text);
        Text := '';
      end;

    end;
  finally
    FFormCode.edMatches.Lines.EndUpdate;
    FFormCode.Visible := True;
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
      FFormCode.edMatches.Lines.Add('----------------------------------------');
      FLexer.Next;
    end;

  finally
    FFormCode.edMatches.Lines.EndUpdate;
    FFormCode.Visible := True;
  end;
end;

procedure TFormTester.RequestClick(Sender: TObject);
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
    FFiler.Read(FStream, IHttpRequest(FHttp));

  finally
    FFormCode.edMatches.Lines.EndUpdate;
    FFormCode.Visible := True;
  end;
end;

procedure TFormTester.ResponseClick(Sender: TObject);
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
    FFiler.Read(FStream, IHttpResponse(FHttp));

  finally
    FFormCode.edMatches.Lines.EndUpdate;
    FFormCode.Visible := True;
  end;
end;

procedure TFormTester.edCodeChange(Sender: TObject);
begin
  if not FReading then edCode.Lines.SaveToFile('data.txt');
end;

function TFormTester.GetTokenValue(const Token: IToken): Variant;
begin
end;

procedure TFormTester.SetTokenValue(const Token: IToken; const Value: Variant);
begin
end;

procedure TFormTester.DumpClick(Sender: TObject);
var
  Stream: IMemoryStream;
begin
  FFormCode.edMatches.Lines.BeginUpdate;
  try
    FFormCode.edMatches.Clear;
    if Assigned(FHttp) then
    begin
      Stream := Sil.MemoryStream.Create();
      FFiler.Write(Stream, FHttp);
      FFormCode.edMatches.Lines.Text := Stream.Memory;
    end;
  finally
    FFormCode.edMatches.Lines.EndUpdate;
    FFormCode.Visible := True;
  end;
end;

procedure TFormTester.ModifyClick(Sender: TObject);
begin
  if Assigned(FHttp) then
    with FHttp as IHttpRequest do
    begin
      Tags.List['Expect'].Value := 'pepe';
      Tags.List['User-Agent'].Value := 'DUMMY (DUMMY de PRUEBA)';
    end;
end;

end.
