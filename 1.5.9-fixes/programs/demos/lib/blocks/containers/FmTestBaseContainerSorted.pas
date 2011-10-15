unit FmTestBaseContainerSorted;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Sil, SilContainer;

type
  TFormTestSorted = class(TForm)
    btAdd: TButton;
    ckSorted: TCheckBox;
    edValue: TEdit;
    lbItems: TListBox;
    btRefresh: TButton;
    btClear: TButton;
    btDelete: TButton;
    btSort: TButton;
    btRemove: TButton;
    btRefreshLast: TButton;
    edLapse: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    lbCount: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btAddClick(Sender: TObject);
    procedure btRefreshClick(Sender: TObject);
    procedure btClearClick(Sender: TObject);
    procedure btDeleteClick(Sender: TObject);
    procedure btSortClick(Sender: TObject);
    procedure btRemoveClick(Sender: TObject);
    procedure lbItemsClick(Sender: TObject);
    procedure btRefreshLastClick(Sender: TObject);
    procedure ckSortedClick(Sender: TObject);
  private
    FCounter: IPerformanceCounter;
    FList: IContainerDynamic;
    procedure DoRefresh;
    function DoAdd(const Data: HData): HItem;
  protected // ITypeHandler
    function DoCompareString(Data1, Data2: HData; Param: Pointer): Integer;
  public
    { Public declarations }
  end;

var
  FormTestSorted: TFormTestSorted;

implementation

uses
  SilLfAlgorithms,
  SilLfContainerBase;

{$R *.dfm}

procedure TFormTestSorted.FormCreate(Sender: TObject);
begin
  FCounter := Sil.Os.Performance.Create(); 
  FList := Vector.Create(TypeInfo(string), DoCompareString);
end;

procedure TFormTestSorted.FormDestroy(Sender: TObject);
begin
  FList := nil;
  FCounter := nil;
end;

procedure TFormTestSorted.ckSortedClick(Sender: TObject);
begin
  FCounter.Reset;
  if ckSorted.Checked then
    DoQuickSort(FList);
  edLapse.Text := Sil.Float.ToStr(FCounter.ToMicroseconds(), 6, 3);
end;

procedure TFormTestSorted.btAddClick(Sender: TObject);
var
  Text: string;
begin
  Text := edValue.Text;
  if Sil.Str.IsEmpty(Text) then Text := Sil.Int.PadL(Round(Random * 10000) - 1, 5, '0');
  FCounter.Reset;
  DoAdd(@Text);
  edLapse.Text := Sil.Float.ToStr(FCounter.ToMicroseconds(), 6, 3);
  DoRefresh;
end;

procedure TFormTestSorted.btRefreshClick(Sender: TObject);
begin
  FCounter.Reset;
  DoRefresh;
  edLapse.Text := Sil.Float.ToStr(FCounter.ToMicroseconds(), 6, 3);
end;

procedure TFormTestSorted.btClearClick(Sender: TObject);
begin
  FCounter.Reset;
  FList.Items.Clear;
  edLapse.Text := Sil.Float.ToStr(FCounter.ToMicroseconds(), 6, 3);
  DoRefresh;
end;

procedure TFormTestSorted.DoRefresh;
begin
  lbItems.Items.Clear;
  with FList.Cursors.First do
    while IsValid do
    begin
      lbItems.Items.Add(PAnsiString(FList[Item])^);
      Next;
    end;
  lbCount.Caption := Sil.Int.ToStr(FList.Items.Count);
end;

function TFormTestSorted.DoAdd(const Data: HData): HItem;
begin
  if ckSorted.Checked then
  begin
    DoSearch(FList, Data, Result);
    Result := FList.Insert(Result, Data);
  end else
    Result := FList.Add(Data);
end;

procedure TFormTestSorted.btDeleteClick(Sender: TObject);
begin
  FCounter.Reset;
  with lbItems do
    if ItemIndex <> -1 then
      FList.Items.Delete(ItemIndex);
  edLapse.Text := Sil.Float.ToStr(FCounter.ToMicroseconds(), 6, 3);

  DoRefresh;
end;

procedure TFormTestSorted.btRemoveClick(Sender: TObject);
var
  Text: string;
  Index: HItem;
begin
  Text := edValue.Text;
  FCounter.Reset;
  if ckSorted.Checked then
  begin
    if DoSearch(FList, @Text, Index) then
      FList.Items.Delete(Index);
  end else
    FList.Remove(@Text);

  edLapse.Text := Sil.Float.ToStr(FCounter.ToMicroseconds(), 6, 3);
  DoRefresh;
end;

procedure TFormTestSorted.btSortClick(Sender: TObject);
begin
  FCounter.Reset;
  DoQuickSort(FList);
  edLapse.Text := Sil.Float.ToStr(FCounter.ToMicroseconds(), 6, 3);
  DoRefresh;
end;

procedure TFormTestSorted.lbItemsClick(Sender: TObject);
begin
  with lbItems do
    if ItemIndex <> -1 then
      edValue.Text := Items[ItemIndex];
end;

procedure TFormTestSorted.btRefreshLastClick(Sender: TObject);
begin
  lbItems.Items.Clear;
  FCounter.Reset;
  with FList.Cursors.Last do
    while IsValid do
    begin
      lbItems.Items.Add(PAnsiString(FList[Item])^);
      Next;
    end;
  edLapse.Text := Sil.Float.ToStr(FCounter.ToMicroseconds(), 6, 3);
end;

function TFormTestSorted.DoCompareString(Data1, Data2: HData; Param: Pointer): Integer;
begin
  Result := Sil.Str.TextCompare(PAnsiString(Data1)^, PAnsiString(Data2)^);
end;


initialization
  Randomize;
end.
