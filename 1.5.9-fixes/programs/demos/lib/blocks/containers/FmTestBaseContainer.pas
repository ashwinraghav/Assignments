unit FmTestBaseContainer;

interface

uses
  Classes, Controls, Forms, StdCtrls, Sil, SilContainer;

type
  TFormTestContainer = class(TForm)
    Memo: TMemo;
    edFirst: TEdit;
    edLast: TEdit;
    edDelta: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Enumerate: TButton;
    ListBox: TListBox;
    Refresh: TButton;
    edItem: TEdit;
    Label4: TLabel;
    Find: TButton;
    Contains: TButton;
    Exchange: TButton;
    Remove: TButton;
    Move: TButton;
    Add: TButton;
    procedure FormCreate(Sender: TObject);
    procedure EnumerateClick(Sender: TObject);
    procedure RefreshClick(Sender: TObject);
    procedure FindClick(Sender: TObject);
    procedure ContainsClick(Sender: TObject);
    procedure ExchangeClick(Sender: TObject);
    procedure RemoveClick(Sender: TObject);
    procedure MoveClick(Sender: TObject);
    procedure AddClick(Sender: TObject);
  private
    FItems: IContainerDynamic;
    procedure DoAdd(Value: Integer);
    function Compare(Data1, Data2: HData; Param: Pointer): Integer;
  end;

var
  FormTestContainer: TFormTestContainer;

implementation

{$R *.dfm}

procedure TFormTestContainer.FormCreate(Sender: TObject);
begin
  FItems := Vector.Create(TypeInfo(Integer), Compare);
  DoAdd(0); // 0
  DoAdd(1); // 1
  DoAdd(2); // 2
  DoAdd(3); // 3
  DoAdd(4); // 4
  DoAdd(5); // 5
  DoAdd(6); // 6
  RefreshClick(nil);
end;

function TFormTestContainer.Compare(Data1, Data2: HData; Param: Pointer): Integer;
begin
  Result := Integer(Data1^) - Integer(Data2^);
end;

procedure TFormTestContainer.DoAdd(Value: Integer);
begin
  FItems.Add(@Value);
end;

procedure TFormTestContainer.EnumerateClick(Sender: TObject);
var
  I: IContainerCursor;
  First, Last: IContainerCursor;
  Delta: Integer;
begin
  Memo.Lines.Clear;

  Delta := Sil.Str.ToInt(edDelta.Text, 1);
  First := FItems.Cursors.Get(Sil.Str.ToInt(edFirst.Text, Integer(HNull)), Delta);
  Last := FItems.Cursors.Get(Sil.Str.ToInt(edLast.Text, Integer(HNull)));

  with Sequence.Create(FItems, First, Last, IContainerCursor) do
    while Get(I) do
      Memo.Lines.Add('[' + Sil.Int.ToStr(I.Item) + ']=' + Sil.Int.ToStr(Integer(FItems.Cursors[I]^)));
end;

procedure TFormTestContainer.RefreshClick(Sender: TObject);
var
  I: IContainerCursor;
begin
  ListBox.Items.Clear;

  with Sequence.Create(FItems) do
    while Get(I) do
      ListBox.Items.Add(Sil.Int.ToStr(Integer(FItems[I.Item]^)))
end;

procedure TFormTestContainer.FindClick(Sender: TObject);
var
  Value: Pointer;
begin
  Memo.Lines.Clear;

  Value := Pointer(Sil.Str.ToInt(edItem.Text));

  with FItems.Cursor(@Value) do
    if IsValid then
      Memo.Lines.Add('Find=' + Int.ToStr(Item)) else
      Memo.Lines.Add('Find=no está');
end;

procedure TFormTestContainer.ContainsClick(Sender: TObject);
var
  Value: Integer;
begin
  Memo.Lines.Clear;


  Value := Sil.Str.ToInt(edItem.Text);

  if FItems.Find(@Value) then
    Memo.Lines.Add('Contains=SI') else
    Memo.Lines.Add('Contains=NO');
end;

procedure TFormTestContainer.AddClick(Sender: TObject);
begin
//
end;

procedure TFormTestContainer.ExchangeClick(Sender: TObject);
begin
  FItems.Items.Exchange(Sil.Str.ToInt(edFirst.Text), Sil.Str.ToInt(edLast.Text));
  RefreshClick(nil);
end;

procedure TFormTestContainer.RemoveClick(Sender: TObject);
begin
  FItems.Items.Delete(Sil.Str.ToInt(edFirst.Text), Sil.Str.ToInt(edLast.Text));
  RefreshClick(nil);
end;

procedure TFormTestContainer.MoveClick(Sender: TObject);
begin
  FItems.Items.Move(Sil.Str.ToInt(edFirst.Text), Sil.Str.ToInt(edLast.Text));
  RefreshClick(nil);
end;

end.
