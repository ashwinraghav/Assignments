unit FmTestValue;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, StdCtrls,
  Dialogs, Sil, SilTool, SilBeDataType, SilLiDataType;

type
  TFormTestValueMain = class(
    TForm,
    IDataBuffer )
    btSmallint: TButton;
    btAnsiString: TButton;
    edSmallint: TEdit;
    edAnsiString: TEdit;
    btCreate: TButton;
    btDestroy: TButton;
    edValue: TEdit;
    cbOrigin: TComboBox;
    btApply: TButton;
    btMemory: TButton;
    procedure btSmallintClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btAnsiStringClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btCreateClick(Sender: TObject);
    procedure btDestroyClick(Sender: TObject);
    procedure btApplyClick(Sender: TObject);
    procedure btMemoryClick(Sender: TObject);
  private
    FParcial, FTotal: IPerformanceCounter;
    FHandler: IDataHandler;
    FBuffer: string;
    FValue: IVariable;
  private
    procedure DoSave;
  protected // IDataBuffer
    function GetHandler: IDataHandler;
    function GetSize: LongWord;
    function GetMemory: PChar;
  end;

var
  FormTestValueMain: TFormTestValueMain;

implementation

uses
  FShowAllocated,
  SilLkValue,
  SilLgValueClasses;

//SilTiValue
//SilTkValue
//SilTiVariable


//  CDEFGHIJK MN PQR TU WXYZ
//a          l nopqr  u wxyz

{$R *.dfm}

procedure TFormTestValueMain.FormCreate(Sender: TObject);
begin
  TSysForm.Instance();

  FParcial := Sil.Os.Performance.Create;
  FTotal := Sil.Os.Performance.Create;

  with SilTool.Sv.Configuration.Open('TestValue.ini', True).Get('Config', True) do
  begin
    edValue.Text := ReadString('Value', '', True);
    cbOrigin.ItemIndex := ReadInteger('Origin', 8, True);
  end;
end;

procedure TFormTestValueMain.FormDestroy(Sender: TObject);
begin
  DoSave;
  
  btDestroyClick(nil);
  
  FTotal := nil;
  FParcial := nil;

  TSysForm.Close();
end;

procedure TFormTestValueMain.btCreateClick(Sender: TObject);
begin
  FHandler := Sil.Typ.List.Get(TDataType(cbOrigin.ItemIndex));

  SetLength(FBuffer, FHandler.DataType.Size);
  FHandler.Handler.Initialize(PChar(FBuffer));

  FValue := TSilVariable.Create(Self);  
  DoSave;
end;

procedure TFormTestValueMain.btDestroyClick(Sender: TObject);
begin
  FValue := nil;

  if Assigned(FHandler) then
    FHandler.Handler.Finalize(PChar(FBuffer));

  SetLength(FBuffer, 0);
  FHandler := nil;
end;

procedure TFormTestValueMain.btSmallintClick(Sender: TObject);
var
  Data: Smallint;
  Times: array[0 .. 3] of Double;
  Text: string;
begin
  if Assigned(FValue) then
  begin
    FParcial.Reset;
    FTotal.Reset;

    FValue.Smallint.Get(Data);
    Times[1] := FParcial.ToMilliseconds();

    FParcial.Reset;
    Data := FValue.Smallint.Value;
    Times[2] := FParcial.ToMilliseconds();

    FParcial.Reset;
    Data := FValue.Smallint.Read();
    Times[3] := FParcial.ToMilliseconds();

    Times[0] := FTotal.ToMilliseconds();

    Caption := Int.ToStr(Data);

    Sil.Str.Add(Text, 'Total: %f [mS]', [Times[0]], '|');
    Sil.Str.Add(Text, 'Get: %f [mS]', [Times[1]], '|');
    Sil.Str.Add(Text, 'Value: %f [mS]', [Times[2]], '|');
    Sil.Str.Add(Text, 'Read: %f [mS]', [Times[3]], '|');
    edSmallint.Text := Text;
  end;
end;

procedure TFormTestValueMain.btAnsiStringClick(Sender: TObject);
var
  Data: AnsiString;
  Times: array[0 .. 3] of Double;
  Text: string;
begin
  if Assigned(FValue) then
  begin
    FParcial.Reset;
    FTotal.Reset;

    FValue.AnsiString.Get(Data);
    Times[1] := FParcial.ToMilliseconds();

    FParcial.Reset;
    Data := FValue.AnsiString.Value;
    Times[2] := FParcial.ToMilliseconds();

    FParcial.Reset;
    Data := FValue.AnsiString.Read();
    Times[3] := FParcial.ToMilliseconds();

    Times[0] := FTotal.ToMilliseconds();

    Caption := Data;
    Sil.Str.Add(Text, 'Total: %f [mS]', [Times[0]], '|');
    Sil.Str.Add(Text, 'Get: %f [mS]', [Times[1]], '|');
    Sil.Str.Add(Text, 'Value: %f [mS]', [Times[2]], '|');
    Sil.Str.Add(Text, 'Read: %f [mS]', [Times[3]], '|');
    edAnsiString.Text := Text;
  end;
end;

function TFormTestValueMain.GetHandler: IDataHandler;
begin
  Result := FHandler;
end;

function TFormTestValueMain.GetSize: LongWord;
begin
  Result := Length(FBuffer);
end;

function TFormTestValueMain.GetMemory: PChar;
begin
  Result := @FBuffer[1];
end;

procedure TFormTestValueMain.btApplyClick(Sender: TObject);
begin
  FValue.AnsiString.Value := edValue.Text;
end;

procedure TFormTestValueMain.btMemoryClick(Sender: TObject);
begin
  SysForm.Visible := not SysForm.Visible; 
end;

procedure TFormTestValueMain.DoSave;
begin
  with SilTool.Sv.Configuration.Open('TestValue.ini', True).Get('Config', True) do
  begin
    WriteString('Value', edValue.Text);
    WriteInteger('Origin', cbOrigin.ItemIndex);
  end;
end;

end.
