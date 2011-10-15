unit Unit1;

interface

{$include Defines.inc}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls,

  Sil,
  SilLog,
  SilMath,
  SilUrl,
  SilVcl,
  SilData,
  SilAvl,
  SilCoder,
  SilProtocol,
  SilTool,
  SilEval,
  SilLexer,
  SilMM,
  SilNotify,
  SilTokens,
  SilLayer,
  SilHttp,
  SilScript,

  SilLiValue,
  SilLmField,
  SilLmFieldAccess;

type
  TForm1 = class(TForm, IValueStorage, IFieldStore)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
  private
    FValue: String;
    FValue2: String;
  protected // IValueStorage
    procedure Initialize(const Field: IUnknown; out FieldRef: Pointer);
    procedure Finalize(var FieldRef: Pointer);
    procedure InitializeField(const Field: IUnknown; var FieldRef: PUnknown);
    procedure Read(const FieldRef: Pointer; var Buffer; Size: LongWord);
    procedure Write(const FieldRef: Pointer; const Buffer; Size: LongWord);
  protected // IFieldStore
    procedure IFieldStore.Write = FieldStoreWrite;
    procedure IFieldStore.Read = FieldStoreRead;
    procedure FieldStoreWrite(const Buffer; Position, Size: LongWord);
    procedure FieldStoreRead(var Buffer; Position, Size: LongWord);
  public
    { Public declarations }
  end;

  INamedField = interface (IVariable)
    ['{2D5C7C06-E128-4B05-BD79-43081B71A5F5}']
    function GetName: String;
    function GetIsChanged: Boolean;
    function GetIsEmpty: Boolean;
    property Name: String read GetName;
    property IsChanged: Boolean read GetIsChanged;
    property IsEmpty: Boolean read GetIsEmpty;
  end;

  INamedFieldDef = interface (IDataTypeDef)
    ['{781DA899-778C-47A6-96D7-74F13CF12353}']
    function GetName: String;
    procedure SetName(const Value: String);
    function GetIsChanged: Boolean;
    procedure SetIsChanged(Value: Boolean);
    function GetNumber: LongWord;
    procedure SetNumber(Value: LongWord);
    function GetOffset: LongWord;
    procedure SetOffset(Value: LongWord);
    property Name: String read GetName write SetName;
    property IsChanged: Boolean read GetIsChanged write SetIsChanged;
    property Number: LongWord read GetNumber write SetNumber;
    property Offset: LongWord read GetOffset write SetOffset;
  end;

  TNamedField = class (
    // extends
    TSilFieldDynamic,
    // impements
    INamedField,
    INamedFieldDef)
  private
    FName: String;
    FIsChanged: Boolean;
    FNumber: LongWord;
    FOffset: LongWord;
  protected // INamedField
    function GetName: String;
    function GetIsChanged: Boolean;
    function GetIsEmpty: Boolean;
  protected // INamedFieldDef
    procedure SetName(const Value: String);
    procedure SetIsChanged(Value: Boolean);
    function GetNumber: LongWord;
    procedure SetNumber(Value: LongWord);
    function GetOffset: LongWord;
    procedure SetOffset(Value: LongWord);
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

procedure TForm1.FormCreate(Sender: TObject);
var
  f1, f2: INamedFieldDef;
  n1, n2: INamedField;
begin
  // hacerlo aggregated
  SetLength(FValue, 14);  // string(10) + integer(4)

  f1 := TNamedField.Create(Self, ftString, 10);
  f1.Name := 'uno';
  f1.Offset := 0;
  f2 := TNamedField.Create(Self, ftInteger);
  f2.Name := 'dos';
  f2.Offset := 10;

  n1 := f1 as INamedField;
  n2 := f2 as INamedField;
  n1.AsString := '';
  n1.AsString := 'mensaje de prueba';
  n2.AsInteger := 123;
  n1.AsString;
  n2.AsInteger;

  n1 := nil;
  n2 := nil;
  f1 := nil;
  f2 := nil;
end;

procedure TForm1.InitializeField(const Field: IInterface; var FieldRef: PUnknown);
var
  Obj: IUnknown;
begin
  Field.QueryInterface(INamedFieldDef, Obj);
  FieldRef := Pointer(Obj);
end;

procedure TForm1.Read(const FieldRef: Pointer; var Buffer; Size: LongWord);
var
  FieldDef: INamedFieldDef absolute FieldRef;
begin
  if Size > 0 then Move(FValue[1 + FieldDef.Offset], Buffer, Size);
end;

procedure TForm1.Write(const FieldRef: Pointer; const Buffer; Size: LongWord);
var
  FieldDef: INamedFieldDef absolute FieldRef;
begin
  if Size > 0 then Move(Buffer, FValue[1 + FieldDef.Offset], Size);
end;

procedure TForm1.FieldStoreRead(var Buffer; Position, Size: LongWord);
var
  lwSize: LongWord;
begin
  lwSize := Length(FValue2);

  if Size < lwSize then
    lwSize := Size else
    FillChar((PChar(@Buffer) + lwSize)^, Size - lwSize, 0);

  Move(FValue2[1], Buffer, lwSize);
end;

procedure TForm1.FieldStoreWrite(const Buffer; Position, Size: LongWord);
var
  lwSize: LongWord;
begin
  lwSize := Length(FValue2);

  if Size > lwSize then Size := lwSize;
  if Size > 0 then Move(Buffer, FValue2[1], Size);
  if Size < lwSize then FillChar(FValue2[Size + 1], lwSize - Size, 0);
end;

{ TNamedField }

function TNamedField.GetIsChanged: Boolean;
begin
  Result := FIsChanged;
end;

function TNamedField.GetIsEmpty: Boolean;
begin
  Result := false; //Length(FValue) = 0
end;

function TNamedField.GetName: String;
begin
  Result := FName;
end;

function TNamedField.GetOffset: LongWord;
begin
  Result := FOffset;
end;

function TNamedField.GetNumber: LongWord;
begin
  Result := FNumber;
end;

procedure TNamedField.SetIsChanged(Value: Boolean);
begin
  FIsChanged := Value;
end;

procedure TNamedField.SetName(const Value: String);
begin
  FName := Value;
end;

procedure TNamedField.SetOffset(Value: LongWord);
begin
  FOffset := Value;
end;

procedure TNamedField.SetNumber(Value: LongWord);
begin
  FNumber := Value;
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  g: TGuid;
begin
  g := Guid.Create;
  Caption := Guid.ToStr(g);
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  d: IFieldDef;
  f: IFieldAccess;
  t: TDateTime;
  i: Integer;
begin
  FValue2 := Str.ReplicateChar(#32, 10);
  d := TStringFieldAccess.CreateSized('test', 10, Self);
  f := d as IFieldAccess;
  t := DateTime.Now;

  for i := 0 to 100000 do
  begin
    f.AsString := 'test';
    f.AsString;
  end;

  button2.Caption := DateTime.ToStr(DateTime.Now - t, 'hh:nn:ss:zzz');
end;

procedure TForm1.Button3Click(Sender: TObject);
var
  d: INamedFieldDef;
  f: IVariable;
  t: TDateTime;
  i: Integer;
begin
  d := TNamedField.Create(Self, ftString, 10);
  f := d as INamedField;
  t := DateTime.Now;

  for i := 0 to 100000 do
  begin
    f.AsString := 'test';
    f.AsString;
  end;

  button3.Caption := DateTime.ToStr(DateTime.Now - t, 'hh:nn:ss:zzz');
end;

procedure TForm1.Finalize(var FieldRef: Pointer);
begin

end;

procedure TForm1.Initialize(const Field: IInterface;
  out FieldRef: Pointer);
begin

end;

end.

