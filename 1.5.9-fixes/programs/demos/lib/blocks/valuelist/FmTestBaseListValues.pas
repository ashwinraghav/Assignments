unit FmTestBaseListValues;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls,

  Sil;

type
  TFormTestValuelist = class(TForm)
    Display: TMemo;
    cbValueType: TComboBox;
    edValueName: TEdit;
    edValueSize: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    btAddValue: TButton;
    edValue: TEdit;
    Label4: TLabel;
    btEnum: TButton;
    btModifyValue: TButton;
    btDeleteValue: TButton;
    procedure FormCreate(Sender: TObject);
    procedure btAddValueClick(Sender: TObject);
    procedure btEnumClick(Sender: TObject);
    procedure DisplayClear(Sender: TObject);
    procedure btModifyValueClick(Sender: TObject);
    procedure btDeleteValueClick(Sender: TObject);
  private
    FList: IValueList;
  public
    { Public declarations }
  end;

var
  FormTestValuelist: TFormTestValuelist;

implementation

uses SilBtStr, SilLiField, SilLiFiler;

{$R *.DFM}

procedure TFormTestValuelist.FormCreate(Sender: TObject);
var
  k: TDataFieldType;
begin
  for k := Low(k) to High(k) do
    cbValueType.Items.Add(Sil.Enum.Name(TypeInfo(TDataFieldType), Ord(k), 'ft'));

  FList := Sil.List.ValueList(true);
end;

procedure TFormTestValuelist.btAddValueClick(Sender: TObject);
var
  ValueName: string;
  ValueType: TDataFieldType;
  ValueSize: Integer;
begin
  ValueName := edValueName.Text;

  with cbValueType do
    if ItemIndex <> -1 then
      ValueType := TDataFieldType(Sil.Enum.Value(TypeInfo(TDataFieldType), Items[ItemIndex], 'ft')) else
      ValueType := ftString;

  ValueSize := Sil.Str.ToInt(edValueSize.Text, 0);

  FList.New(ValueName, ValueType, ValueSize).AsString := edValue.Text;

  btEnum.Click;
end;

procedure TFormTestValuelist.btModifyValueClick(Sender: TObject);
var
  ValueName: string;
  Value: IFieldAccess;
begin
  ValueName := edValueName.Text;
  Value := FList[ValueName];
  if Assigned(Value) then
    Value.AsString := edValue.Text;
    
  btEnum.Click;
end;

procedure TFormTestValuelist.btDeleteValueClick(Sender: TObject);
var
  ValueName: string;
  Value: IFieldAccess;
begin
  ValueName := edValueName.Text;

  Value := FList[ValueName];
  if Assigned(Value) then
    FList.Remove(Value);
    
  btEnum.Click;
end;

procedure TFormTestValuelist.btEnumClick(Sender: TObject);
var
  f: IFieldAccess;
  i: IEnumerator;
begin
  Display.Lines.Clear;

  with FList do
    while Enumerate(i, f) do
      Display.Lines.Add(
        Sil.Str.Format('#%d: %s: %s(%d) = %s', [
          i.Iteration,
          f.Name,
          Sil.Enum.Name(TypeInfo(TDataFieldType), Ord(f.DataType), 'ft'),
          f.Size,
          f.AsString]));
end;

procedure TFormTestValuelist.DisplayClear(Sender: TObject);
begin
  Display.Lines.Clear;
end;

end.
