unit FmTestBaseMemberList;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs,

  Sil,
  SilLiMemberList, StdCtrls, Grids, ValEdit;

type
  TFormTestBaseMemberList = class(TForm, IUnknown)
    veList: TValueListEditor;
    Enumerate: TButton;
    edValue: TEdit;
    lbMember: TLabel;
    SetValue: TButton;
    ChangeValues: TButton;
    procedure FormCreate(Sender: TObject);
    procedure EnumerateClick(Sender: TObject);
    procedure veListSelectCell(Sender: TObject; ACol, ARow: Integer; var CanSelect: Boolean);
    procedure SetValueClick(Sender: TObject);
    procedure veListSetEditText(Sender: TObject; ACol, ARow: Integer; const Value: String);
    procedure ChangeValuesClick(Sender: TObject);
  private
    FList: IMemberList;
    FAnsiString: string;
    FWideString: WideString;
    FInteger: Integer;
    FInterface: IUnknown;
  end;

  // objeto testigo que sirve para verificar si se ejecutan los finalize(s)
  
  TTestObject = class(TSilObject)
    class function Create: IUnknown;  
    destructor Destroy; override;
  end;

var
  FormTestBaseMemberList: TFormTestBaseMemberList;

implementation

{$R *.dfm}

{ TTestObject }

class function TTestObject.Create: IUnknown;
begin
  Result := inherited Create();
end;

destructor TTestObject.Destroy;
begin
  inherited;
end;

{ TFormTestBaseMemberList } 

procedure TFormTestBaseMemberList.FormCreate(Sender: TObject);
begin
  FList := Sil.List.Members.Create();
  
  Sil.List.Members.Add(FList, FInteger, 'Integer1', -1);
  Sil.List.Members.Add(FList, FAnsiString, 'AnsiString', 'hola');
  Sil.List.Members.Add(FList, FWideString, 'WideString', 'hola');
  Sil.List.Members.Add(FList, FInteger, 'Integer2', -12345);
  Sil.List.Members.Add(FList, FInterface, 'FieldObject', TTestObject.Create());
  Sil.List.Members.Add(FList, nil, 'TestObject', TTestObject.Create());
  Sil.List.Members.Add(FList, nil, 'TypeInfeado', 'nuevo', System.TypeInfo(WideString));
end;

procedure TFormTestBaseMemberList.EnumerateClick(Sender: TObject);
var
  Enum: IEnumerator;
  Item: RMember;
begin
  veList.Strings.BeginUpdate;
  try
    veList.Strings.Clear;
    with FList do
      while Enumerate(Enum, Item) do
        veList.InsertRow(Item.Header.Name, Sil.Vart.ToStr(Item.Value), True);
  finally
    veList.Strings.EndUpdate;
  end;
end;

procedure TFormTestBaseMemberList.veListSelectCell(Sender: TObject; ACol, ARow: Integer; var CanSelect: Boolean);
var
  Params: IParameters;
  Value: Variant;
begin
  Params := FList;
  lbMember.Caption := veList.Cells[0, ARow];
  if Params.Find(lbMember.Caption, Value) then
    edValue.Text := Sil.Vart.ToStr(Value) else
    edValue.Text := veList.Cells[1, ARow];
end;

procedure TFormTestBaseMemberList.SetValueClick(Sender: TObject);
var
  Params: IParameterList;
begin
  Params := FList;
  Params[lbMember.Caption] := edValue.Text;
  Enumerate.Click;
end;

procedure TFormTestBaseMemberList.veListSetEditText(Sender: TObject; ACol, ARow: Integer; const Value: String);
var
  Params: IParameterList;
begin
  Params := FList;
  Params[veList.Cells[0, ARow]] := veList.Cells[1, ARow];
end;

procedure TFormTestBaseMemberList.ChangeValuesClick(Sender: TObject);
begin
  FInteger := Random(10000);
  FAnsiString := 'AnsiString to ' + Sil.Int.ToStr(FInteger);
  FWideString := 'WideString to ' + Sil.Int.ToStr(FInteger);
end;

end.
