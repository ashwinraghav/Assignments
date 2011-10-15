unit FmTestBaseContainerInterface;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls,

  Sil,
  SilContainer;

type
  ITest = interface
    ['{7F8C3934-AE72-402E-A73F-2C5773685D03}']
    function Value: String;
  end;

  TTest = class (TSilObject, ITest)
  private
    FValue: String;
  protected // ITest
    function Value: String;
  public
    constructor Create(const Value: String);
  end;

type
  TForm1 = class(TForm)
    ListBox1: TListBox;
    Button1: TButton;
    Label1: TLabel;
    Label2: TLabel;
    CheckBox1: TCheckBox;
    procedure Button1Click(Sender: TObject);
  private
    FItems: IInterfaceList;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
var
  i: Integer;
  Item: IUnknown;
  p: ITest;
  Enum: IEnumerator;
  Perf: IPerformanceCounter;
begin
  Perf := Sil.OS.Performance.Create;

  listbox1.Clear;
  FItems := SilContainer.Vector.InterfaceList(CheckBox1.Checked);

  FItems.Add(TTest.Create('uno'));
  FItems.Add(TTest.Create('dos'));
  FItems.Add(TTest.Create('cuatro'));

  FItems.Insert(2, TTest.Create('tres'));

  for i := 0 to FItems.Count - 1 do
    listbox1.Items.Add((FItems[i] as ITest).Value);

  p := FItems[1] as ITest;
  Caption := Int.ToStr(FItems.IndexOf(p));

  while FItems.Enumerate(Enum, Item) do
    listbox1.Items.Add((Item as ITest).Value);

  label1.Caption := Str.Format('%d %d', [AllocMemCount, AllocMemSize]);
  label2.Caption := Float.ToStr(Perf.ToMilliseconds);
end;

{ TTest }

constructor TTest.Create(const Value: String);
begin
  inherited Create;
  FValue := Value;
end;

function TTest.Value: String;
begin
  Result := FValue;
end;

end.
