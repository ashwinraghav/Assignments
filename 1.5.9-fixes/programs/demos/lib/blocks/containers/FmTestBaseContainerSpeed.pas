unit FmTestBaseContainerSpeed;

{$INCLUDE Defines.inc}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Clipbrd, StdCtrls, ComCtrls, ExtCtrls,
  Dialogs, Sil, SilContainer;

type
  PDataRecord = ^RDataRecord;
  RDataRecord = record
    Text: string;
    Time: TDateTime;
  end;

  TFormTestContainerSpeed = class(TForm)
    btAddNueva: TButton;
    btEnumNueva: TButton;
    btAddVieja: TButton;
    btAddDelphi: TButton;
    btEnumVieja: TButton;
    btEnumDelphi: TButton;
    btClearNueva: TButton;
    btClearVieja: TButton;
    btClearDelphi: TButton;
    btDeleteNueva: TButton;
    btDeleteVieja: TButton;
    btDeleteDelphi: TButton;
    lbAddNueva: TLabel;
    lbAddVieja: TLabel;
    lbAddDelphi: TLabel;
    lbEnumNueva: TLabel;
    lbEnumVieja: TLabel;
    lbEnumDelphi: TLabel;
    lbClearNueva: TLabel;
    lbClearVieja: TLabel;
    lbClearDelphi: TLabel;
    lbDeleteNueva: TLabel;
    lbDeleteVieja: TLabel;
    lbDeleteDelphi: TLabel;
    edIterations: TEdit;
    lvTests: TListView;
    btCopy: TButton;
    btSeqSimple: TButton;
    lbSecuenceNueva: TLabel;
    btClearList: TButton;
    Timer1: TTimer;
    btSeqRange: TButton;
    lbAddString: TLabel;
    lbEnumString: TLabel;
    lbClearString: TLabel;
    btAddString: TButton;
    btEnumString: TButton;
    btClearString: TButton;
    lbCountNueva: TLabel;
    btCountNueva: TButton;
    lbCountVieja: TLabel;
    btCountVieja: TButton;
    lbCountDelphi: TLabel;
    btCountDelphi: TButton;
    lbSortNueva: TLabel;
    lbSortVieja: TLabel;
    lbSortDelphi: TLabel;
    lbSortString: TLabel;
    btSortNueva: TButton;
    btSortVieja: TButton;
    btSortDelphi: TButton;
    btSortString: TButton;
    lbAddIntf: TLabel;
    lbEnumIntf: TLabel;
    lbClearIntf: TLabel;
    lbDeleteIntf: TLabel;
    btAddIntf: TButton;
    btEnumIntf: TButton;
    btClearIntf: TButton;
    btDeleteIntf: TButton;
    btSortIntf: TButton;
    lbSortIntf: TLabel;
    lbSearchNueva: TLabel;
    lbSearchVieja: TLabel;
    lbSearchDelphi: TLabel;
    lbSearchString: TLabel;
    Button1: TButton;
    Button3: TButton;
    Button5: TButton;
    Button6: TButton;
    edValue: TEdit;
    btCountString: TButton;
    lbCountString: TLabel;
    btForEach: TButton;
    btFindFirst: TButton;
    lbAddLinked: TLabel;
    lbEnumLinked: TLabel;
    lbClearLinked: TLabel;
    lbDeleteLinked: TLabel;
    lbCountLinked: TLabel;
    btAddLinked: TButton;
    btEnumLinked: TButton;
    btClearLinked: TButton;
    btDeleteLinked: TButton;
    btCountLinked: TButton;
    lbMemory: TLabel;
    lbResult: TLabel;
    btAddIList: TButton;
    btEnumIList: TButton;
    btSortIList: TButton;
    btSearchIList: TButton;
    lbEnumIList: TLabel;
    lbAddIList: TLabel;
    lbSearchIList: TLabel;
    lbSortIList: TLabel;
    btClearIList: TButton;
    lbClearIList: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure btAddNuevaClick(Sender: TObject);
    procedure btEnumNuevaClick(Sender: TObject);
    procedure btAddViejaClick(Sender: TObject);
    procedure btAddDelphiClick(Sender: TObject);
    procedure btEnumViejaClick(Sender: TObject);
    procedure btEnumDelphiClick(Sender: TObject);
    procedure btClearNuevaClick(Sender: TObject);
    procedure btClearViejaClick(Sender: TObject);
    procedure btClearDelphiClick(Sender: TObject);
    procedure btDeleteNuevaClick(Sender: TObject);
    procedure btDeleteViejaClick(Sender: TObject);
    procedure btDeleteDelphiClick(Sender: TObject);
    procedure edIterationsExit(Sender: TObject);
    procedure btCopyClick(Sender: TObject);
    procedure btSeqSimpleClick(Sender: TObject);
    procedure btClearListClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure btSeqRangeClick(Sender: TObject);
    procedure btAddStringClick(Sender: TObject);
    procedure btClearStringClick(Sender: TObject);
    procedure btCountNuevaClick(Sender: TObject);
    procedure btCountViejaClick(Sender: TObject);
    procedure btCountDelphiClick(Sender: TObject);
    procedure btSortNuevaClick(Sender: TObject);
    procedure btSortViejaClick(Sender: TObject);
    procedure btSortDelphiClick(Sender: TObject);
    procedure btSortStringClick(Sender: TObject);
    procedure btAddIntfClick(Sender: TObject);
    procedure btEnumIntfClick(Sender: TObject);
    procedure btClearIntfClick(Sender: TObject);
    procedure btDeleteIntfClick(Sender: TObject);
    procedure btSortIntfClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure btEnumStringClick(Sender: TObject);
    procedure btCountStringClick(Sender: TObject);
    procedure btForEachClick(Sender: TObject);
    procedure btFindFirstClick(Sender: TObject);
    procedure btAddLinkedClick(Sender: TObject);
    procedure btEnumLinkedClick(Sender: TObject);
    procedure btClearLinkedClick(Sender: TObject);
    procedure btDeleteLinkedClick(Sender: TObject);
    procedure btCountLinkedClick(Sender: TObject);
    procedure btAddIListClick(Sender: TObject);
    procedure btEnumIListClick(Sender: TObject);
    procedure btClearIListClick(Sender: TObject);
  private
    FCounter: IPerformanceCounter;
    FProfiler: IPerformanceCounter;
    FNueva: IContainerDynamic;
    FIntfs: IContainerDynamic;
    FString: IStringsDynamic;
    FVieja: IStringList;
    FDelphi: TStringList;
    FLinked: IContainerDynamic;
    FIList: IInterfaceList;
    N: Integer;
  private
    procedure DoAddTest(N: Integer; Mem: Integer; const Test, Tipo, Value: string);
  protected // ITypeHandler
    function DoCompareRecord(Data1, Data2: HData; Param: Pointer): Integer;
    function DoCompareInterface(Data1, Data2: HData; Param: Pointer): Integer;
  public
    { Public declarations }
  end;


  // Old-Fashioned (Deprecated now) Type Handler used by lists in Sil.List.xxx
  DataRecordHandler = class(DataHandler)
    class procedure Alloc(var Result: Pointer; const Data: Pointer = nil); override;
    class procedure Dispose(var Value: Pointer; const Data: Pointer = nil); override; 
    class procedure Copy(const Source: Pointer; const Dest: Pointer; const Data: Pointer = nil); override; 
    class function Compare(const Value1; const Value2; Data: Pointer = nil): Integer; override;
    class procedure Clear(var Obj; const Data: Pointer = nil); override;
  end;

var
  FormTestContainerSpeed: TFormTestContainerSpeed;

implementation

uses
  SilLfAlgorithms,
  SilLfContainerBase;

{$R *.dfm}

{ DataRecordHandler }

class procedure DataRecordHandler.Alloc(var Result: Pointer; const Data: Pointer);
begin
  Result := System.New(PDataRecord);
end;

class procedure DataRecordHandler.Dispose(var Value: Pointer; const Data: Pointer);
begin
  System.Dispose(PDataRecord(Value));
end;

class procedure DataRecordHandler.Clear(var Obj; const Data: Pointer);
begin
  System.Finalize(RDataRecord(Obj));
end;

class procedure DataRecordHandler.Copy(const Source, Dest, Data: Pointer);
begin
  PDataRecord(Dest)^ := PDataRecord(Source)^;
end;

class function DataRecordHandler.Compare(const Value1, Value2; Data: Pointer): Integer;
begin
  Result := Sil.Text.Compare(PDataRecord(Value1).Text, PDataRecord(Value2).Text);
end;

{ TForm1 }

function TFormTestContainerSpeed.DoCompareRecord(Data1, Data2: HData; Param: Pointer): Integer;
begin
  Result := Sil.Text.Compare(PDataRecord(Data1).Text, PDataRecord(Data2).Text);
end;

function TFormTestContainerSpeed.DoCompareInterface(Data1, Data2: HData; Param: Pointer): Integer;
begin
  Result := Sil.Int.Iif(Sil.Ref.SameObject(IUnknown(Data1^), IUnknown(Data2^)), 0, -1);
end;

procedure TFormTestContainerSpeed.FormCreate(Sender: TObject);
begin
  FCounter := Sil.Os.Performance.Create();
  FProfiler := Sil.Os.Performance.Create();
  FNueva := Vector.Create(TypeInfo(RDataRecord), DoCompareRecord);
  FIntfs := Vector.Create(TypeInfo(IUnknown), DoCompareInterface);
  FString := Vector.Strings(Handler.Create(TypeInfo(RDataRecord), Compare.Create(DoCompareRecord)));
  FVieja := Sil.List.StringList(False, DataRecordHandler);
  FDelphi := Classes.TStringList.Create();
  FLinked := List.Create(TypeInfo(RDataRecord), DoCompareRecord);
  FIList := Vector.InterfaceList;

  Sil.Os.Process.Current.Priority := ppHigh;
  Sil.Os.Thread.Current.Priority := tpTimeCritical;
end;

procedure TFormTestContainerSpeed.FormDestroy(Sender: TObject);
begin
  FString := nil;
  FNueva := nil;
  FVieja := nil;
  FIntfs := nil;
  FDelphi.Free;
  FLinked := nil;
  FIList := nil;
  FCounter := nil;
  FProfiler := nil;
end;

procedure TFormTestContainerSpeed.btAddNuevaClick(Sender: TObject);
var
  Data: RDataRecord;
  I: Integer;
  M: Integer;
begin
  FNueva.Items.Clear;
  M := AllocMemSize;

  FCounter.Reset;
  for I := 0 to N do
  begin
    Data.Text := Float.ToStr(Random(N));
    FNueva.Add(@Data);
  end;
  M := AllocMemSize - M;

  lbAddNueva.Caption := Sil.Float.ToStr(FCounter.ToSeconds(), 10, 6);
  DoAddTest(N, M, 'ADD', 'NUEVA', lbAddNueva.Caption);
end;

procedure TFormTestContainerSpeed.btAddLinkedClick(Sender: TObject);
var
  Data: RDataRecord;
  I: Integer;
  M: Integer;
begin
  FLinked.Items.Clear;
  M := AllocMemSize;

  FCounter.Reset;
  for I := 0 to N do
  begin
    Data.Text := Float.ToStr(Random(N));
    FLinked.Add(@Data);
  end;
  M := AllocMemSize - M;

  lbAddLinked.Caption := Sil.Float.ToStr(FCounter.ToSeconds(), 10, 6);
  DoAddTest(N, M, 'ADD', 'LINKED', lbAddLinked.Caption);
end;

procedure TFormTestContainerSpeed.btAddViejaClick(Sender: TObject);
var
  Data: RDataRecord;
  I: Integer;
  M: Integer;
begin
  FVieja.Clear;

  FCounter.Reset;

  M := AllocMemSize;
  for I := 0 to N do
  begin
    Data.Text := Float.ToStr(Random(N));
    FVieja.Add(Data.Text, @Data);
  end;
  M := AllocMemSize - M;

  lbAddVieja.Caption := Sil.Float.ToStr(FCounter.ToSeconds(), 10, 6);
  DoAddTest(N, M, 'ADD', 'VIEJA', lbAddVieja.Caption);
end;

procedure TFormTestContainerSpeed.btAddDelphiClick(Sender: TObject);
var
  Data: RDataRecord;
  Temp: PDataRecord;
  I: Integer;
  M: Integer;
begin
  FDelphi.Clear;
  FCounter.Reset;

  M := AllocMemSize;
  for I := 0 to N do
  begin
    Data.Text := Float.ToStr(Random(N));
    Temp := System.New(PDataRecord);
    Temp^ := Data;

    FDelphi.AddObject(Data.Text, Pointer(Temp));
  end;
  M := AllocMemSize - M;

  lbAddDelphi.Caption := Sil.Float.ToStr(FCounter.ToSeconds(), 10, 6);
  DoAddTest(N, M, 'ADD', 'DELPHI', lbAddDelphi.Caption);
end;

procedure TFormTestContainerSpeed.btAddStringClick(Sender: TObject);
var
  Data: RDataRecord;
  I: Integer;
  M: Integer;
begin
  FString.Items.Clear;
  M := AllocMemSize;

  FCounter.Reset;
  for I := 0 to N do
  begin
    Data.Text := Float.ToStr(Random(N));
    FString.Add(Data.Text, @Data);
  end;
  M := AllocMemSize - M;

  lbAddString.Caption := Sil.Float.ToStr(FCounter.ToSeconds(), 10, 6);
  DoAddTest(N, M, 'ADD', 'STRING', lbAddString.Caption);
end;

procedure TFormTestContainerSpeed.btAddIntfClick(Sender: TObject);
var
  Text: string;
  Data: IUnknown;
  I: Integer;
  M: Integer;
begin
  FIntfs.Items.Clear;

  Data := Self;

  FCounter.Reset;
  M := AllocMemSize;
  for I := 0 to N do
  begin
    Text := Float.ToStr(Random(N));
    FIntfs.Add(@Data);
  end;
  M := AllocMemSize - M;

  lbAddIntf.Caption := Sil.Float.ToStr(FCounter.ToSeconds(), 10, 6);
  DoAddTest(N, M, 'ADD', 'INTFs', lbAddIntf.Caption);
end;

procedure TFormTestContainerSpeed.btCountNuevaClick(Sender: TObject);
var
  Data: RDataRecord;
  I: IContainerCursor;
  M: Integer;
begin
  FNueva.Items.Clear;
  M := AllocMemSize;

  FCounter.Reset;

  FNueva.Items.Count := N;

  I := FNueva.Cursors.First;

  while I.IsValid do
  begin
    Data.Text := Int.ToStr(Integer(I.Item));
    Data.Time := Integer(I.Item);
    FNueva.Cursors[I] := @Data;
    I.Next;
  end;
  M := AllocMemSize - M;

  lbCountNueva.Caption := Sil.Float.ToStr(FCounter.ToSeconds(), 10, 6);
  DoAddTest(N, M, 'COUNT', 'NUEVA', lbCountNueva.Caption);
end;

procedure TFormTestContainerSpeed.btCountLinkedClick(Sender: TObject);
var
  Data: RDataRecord;
  I: IContainerCursor;
  M: Integer;
begin
  FLinked.Items.Clear;

  M := AllocMemSize;
  FCounter.Reset;
  
  FLinked.Items.Count := N;

  I := FLinked.Cursors.First;
  while I.IsValid do
  begin
    Data.Text := Int.ToStr(Integer(I.Item));
    Data.Time := Integer(I.Item);
    FLinked.Cursors[I] := @Data;
    I.Next;
  end;
  M := AllocMemSize - M;

  lbCountLinked.Caption := Sil.Float.ToStr(FCounter.ToSeconds(), 10, 6);
  DoAddTest(N, M, 'COUNT', 'LINKED', lbCountLinked.Caption);
end;

procedure TFormTestContainerSpeed.btCountViejaClick(Sender: TObject);
var
  Data: RDataRecord;
  I: Integer;
  M: Integer;
begin
  FVieja.Clear;

  FCounter.Reset;

  M := AllocMemSize;
  FVieja.Capacity := N;
  for I := 0 to N - 1 do
  begin
    Data.Text := Int.ToStr(I);
    //Data.Data := I;
    Data.Time := I;
    FVieja.Add(Data.Text, @Data);
  end;
  M := AllocMemSize - M;

  lbCountVieja.Caption := Sil.Float.ToStr(FCounter.ToSeconds(), 10, 6);
  DoAddTest(N, M, 'COUNT', 'VIEJA', lbCountVieja.Caption);
end;

procedure TFormTestContainerSpeed.btCountDelphiClick(Sender: TObject);
var
  Data: RDataRecord;
  Temp: PDataRecord;
  I: Integer;
  M: Integer;
begin
  FDelphi.Clear;
  FCounter.Reset;

  M := AllocMemSize;
  FDelphi.Capacity := N;
  for I := 0 to N - 1 do
  begin
    Data.Text := Int.ToStr(I);
    //Data.Data := I;
    Data.Time := I;
    Temp := System.New(PDataRecord);
    Temp^ := Data;

    FDelphi.AddObject(Data.Text, Pointer(Temp));
  end;
  M := AllocMemSize - M;

  lbCountDelphi.Caption := Sil.Float.ToStr(FCounter.ToSeconds(), 10, 6);
  DoAddTest(N, M, 'COUNT', 'DELPHI', lbCountDelphi.Caption);
end;

procedure TFormTestContainerSpeed.btCountStringClick(Sender: TObject);
var
  Data: RDataRecord;
  I: IStringPointerDynamic;
  M: Integer;
begin
  FString.Items.Clear;
  M := AllocMemSize;

  FCounter.Reset;

  FString.Items.Count := N;

  with FString.Cursors do
    if First.Clone(IStringPointerDynamic, I) then
      while I.IsValid do
      begin
        Data.Text := Int.ToStr(Integer(I.Item));
        Data.Time := Integer(I.Item);
        I.Put(Data.Text, @Data);
        I.Next;
      end;
  M := AllocMemSize - M;

  lbCountString.Caption := Sil.Float.ToStr(FCounter.ToSeconds(), 10, 6);
  DoAddTest(N, M, 'COUNT', 'STRING', lbCountString.Caption);
end;

procedure TFormTestContainerSpeed.btEnumNuevaClick(Sender: TObject);
var
  Text: string;
  M: Integer;
begin
  FCounter.Reset;

  M := AllocMemSize;

  with FNueva.Cursors.Last do
    while IsValid do
    begin
      Text := PDataRecord(Data).Text;
      Next;
    end;
  M := AllocMemSize - M;

  lbEnumNueva.Caption := Sil.Float.ToStr(FCounter.ToSeconds(), 10, 6);
  DoAddTest(N, M, 'ENUM', 'NUEVA', lbEnumNueva.Caption);
end;

procedure TFormTestContainerSpeed.btEnumLinkedClick(Sender: TObject);
var
  Text: string;
  M: Integer;
begin
  FCounter.Reset;

  M := AllocMemSize;

  with FLinked.Cursors.Last do
    while IsValid do
    begin
      Text := PDataRecord(Data).Text;
      Next;
    end;
  M := AllocMemSize - M;

  lbEnumLinked.Caption := Sil.Float.ToStr(FCounter.ToSeconds(), 10, 6);
  DoAddTest(N, M, 'ENUM', 'LINKED', lbEnumLinked.Caption);
end;

procedure TFormTestContainerSpeed.btForEachClick(Sender: TObject);
  procedure Action(const Item: IContainerPointerStatic);
  var
    Data: PDataRecord;
    Text: string;
  begin
     Data := Item.Data;
     Text := Data.Text;
  end;
var
  M: Integer;
begin
  FCounter.Reset;

  M := AllocMemSize;
  with FNueva.Cursors do ForEach(First, Last, IContainerPointerStatic, Sil.NestedAction.Create(@Action));
  M := AllocMemSize - M;

  lbEnumNueva.Caption := Sil.Float.ToStr(FCounter.ToSeconds(), 10, 6);
  DoAddTest(N, M, 'FOREACH', 'NUEVA', lbEnumNueva.Caption);
end;

procedure TFormTestContainerSpeed.btFindFirstClick(Sender: TObject);
  function Test(const Item: IContainerPointerStatic): Boolean;
  begin
     Result := PDataRecord(Item.Data).Text = '0';
  end;
var
  M: Integer;
  I: IContainerPointerStatic;
begin
  FCounter.Reset;

  M := AllocMemSize;
  with FNueva.Cursors do
    if FindFirst(First, Last, IContainerPointerStatic, Sil.NestedTester.Create(@Test), I) then
      lbResult.Caption := PDataRecord(I.Data).Text;
  M := AllocMemSize - M;

  lbEnumNueva.Caption := Sil.Float.ToStr(FCounter.ToSeconds(), 10, 6);
  DoAddTest(N, M, 'FINDFIRST', 'NUEVA', lbEnumNueva.Caption);
end;

procedure TFormTestContainerSpeed.btEnumViejaClick(Sender: TObject);
var
  Enum: IEnumerator;
  Item: string;
  Data: PDataRecord;
  M: Integer;
begin
  FCounter.Reset;

  M := AllocMemSize;
  with FVieja do
    while Enumerate(Enum, Item) do
    begin
      Data := Ptrs[Enum.Iteration];
      Item := Data.Text;
    end;
  M := AllocMemSize - M;

  lbEnumVieja.Caption := Sil.Float.ToStr(FCounter.ToSeconds(), 10, 6);
  DoAddTest(N, M, 'ENUM', 'VIEJA', lbEnumVieja.Caption);
end;

procedure TFormTestContainerSpeed.btEnumDelphiClick(Sender: TObject);
var
  Item: string;
  Data: PDataRecord;
  I: Integer;
  M: Integer;
begin
  FCounter.Reset;

  M := AllocMemSize;
  for I := 0 to FDelphi.Count - 1 do
  begin
    Item := FDelphi[I];
    Data := Pointer(FDelphi.Objects[I]);
    Item := Data.Text;
  end;
  M := AllocMemSize - M;

  lbEnumDelphi.Caption := Sil.Float.ToStr(FCounter.ToSeconds(), 10, 6);
  DoAddTest(N, M, 'ENUM', 'DELPHI', lbEnumDelphi.Caption);
end;

procedure TFormTestContainerSpeed.btEnumStringClick(Sender: TObject);
var
  Data: PDataRecord;
  Text: string;
  M: Integer;
  I: IStringPointerStatic;
begin
  FCounter.Reset;

  M := AllocMemSize;

  with FString.Cursors do
    if First.Clone(IStringPointerStatic, I) then
      while I.IsValid do
      begin
        Data := I.Data;
        Text := Data.Text;
        I.Next;
      end;
  M := AllocMemSize - M;

  lbEnumString.Caption := Sil.Float.ToStr(FCounter.ToSeconds(), 10, 6);
  DoAddTest(N, M, 'ENUM', 'STRING', lbEnumString.Caption);
end;

procedure TFormTestContainerSpeed.btSeqSimpleClick(Sender: TObject);
var
  I: IContainerPointerStatic;
  Seq: IContainerSequence;
  Item: string;
  M: Integer;
begin
  FCounter.Reset;

  M := AllocMemSize;
  Seq := Sequence.Create(FNueva, IContainerPointerStatic, FNueva.Items.Last, -1);
  try
    with Seq do
      while Get(I) do
      begin
        Item := PDataRecord(I.Data).Text;
      end;
  finally
    Seq := nil;
  end;
  M := AllocMemSize - M;

  lbSecuenceNueva.Caption := Sil.Float.ToStr(FCounter.ToSeconds(), 10, 6);
  DoAddTest(N, M, 'SEQ-SIMPLE', 'NUEVA', lbSecuenceNueva.Caption);
end;

procedure TFormTestContainerSpeed.btSeqRangeClick(Sender: TObject);
var
  I: IContainerCursor;
  Seq: IContainerSequence;
  Data: PDataRecord;
  Item: string;
  M: Integer;
begin
  FCounter.Reset;

  M := AllocMemSize;
  Seq := Sequence.Create(FNueva, FNueva.Cursors.First, FNueva.Cursors.Last, IContainerCursor);
  try
    with Seq do
      while Get(I) do
      begin
        Data := FNueva.Cursors[I];
        Item := Data.Text;
      end;
  finally
    Seq := nil;
  end;
  M := AllocMemSize - M;

  lbSecuenceNueva.Caption := Sil.Float.ToStr(FCounter.ToSeconds(), 10, 6);
  DoAddTest(N, M, 'SEQ-RANGE', 'NUEVA', lbSecuenceNueva.Caption);
end;

procedure TFormTestContainerSpeed.btEnumIntfClick(Sender: TObject);
var
  Cursor: IContainerCursor;
  Data: PUnknown;
  Item: IUnknown;
  M: Integer;
begin
  FCounter.Reset;

  M := AllocMemSize;
  Cursor := FIntfs.Cursors.First;
  while Cursor.IsValid do
  begin
    Data := FIntfs.Cursors[Cursor];
    Item := Data^;
    Cursor.Next;
  end;
  M := AllocMemSize - M;

  lbEnumIntf.Caption := Sil.Float.ToStr(FCounter.ToSeconds(), 10, 6);
  DoAddTest(N, M, 'ENUM', 'INTFS', lbEnumIntf.Caption);
end;

procedure TFormTestContainerSpeed.btClearNuevaClick(Sender: TObject);
var
  M: Integer;
begin
  FCounter.Reset;

  M := AllocMemSize;
  FNueva.Items.Clear;
  M := AllocMemSize - M;

  lbClearNueva.Caption := Sil.Float.ToStr(FCounter.ToSeconds(), 10, 6);
  DoAddTest(N, M, 'CLEAR', 'NUEVA', lbClearNueva.Caption);
end;

procedure TFormTestContainerSpeed.btClearLinkedClick(Sender: TObject);
var
  M: Integer;
begin
  FCounter.Reset;

  M := AllocMemSize;
  FLinked.Items.Clear;
  M := AllocMemSize - M;

  lbClearLinked.Caption := Sil.Float.ToStr(FCounter.ToSeconds(), 10, 6);
  DoAddTest(N, M, 'CLEAR', 'LINKED', lbClearLinked.Caption);
end;

procedure TFormTestContainerSpeed.btClearViejaClick(Sender: TObject);
var
  M: Integer;
begin
  FCounter.Reset;

  M := AllocMemSize;
  FVieja.Clear;
  M := AllocMemSize - M;

  lbClearVieja.Caption := Sil.Float.ToStr(FCounter.ToSeconds(), 10, 6);
  DoAddTest(N, M, 'CLEAR', 'VIEJA', lbClearVieja.Caption);
end;

procedure TFormTestContainerSpeed.btClearDelphiClick(Sender: TObject);
var
  Data: PDataRecord;
  I: Integer;
  M: Integer;
begin
  FCounter.Reset;

  M := AllocMemSize;
  for I := 0 to FDelphi.Count - 1 do
  begin
    Data := Pointer(FDelphi.Objects[I]);
    System.Dispose(Data);
  end;
  FDelphi.Clear;
  M := AllocMemSize - M;

  lbClearDelphi.Caption := Sil.Float.ToStr(FCounter.ToSeconds(), 10, 6);
  DoAddTest(N, M, 'CLEAR', 'DELPHI', lbClearDelphi.Caption);
end;

procedure TFormTestContainerSpeed.btClearStringClick(Sender: TObject);
var
  M: Integer;
begin
  FCounter.Reset;

  M := AllocMemSize;
  FString.Items.Clear;
  M := AllocMemSize - M;

  lbClearString.Caption := Sil.Float.ToStr(FCounter.ToSeconds(), 10, 6);
  DoAddTest(N, M, 'CLEAR', 'STRING', lbClearString.Caption);
end;

procedure TFormTestContainerSpeed.btClearIntfClick(Sender: TObject);
var
  M: Integer;
begin
  FCounter.Reset;

  M := AllocMemSize;
  FIntfs.Items.Clear;
  M := AllocMemSize - M;

  lbClearIntf.Caption := Sil.Float.ToStr(FCounter.ToSeconds(), 10, 6);
  DoAddTest(N, M, 'CLEAR', 'NUEVA', lbClearIntf.Caption);
end;

procedure TFormTestContainerSpeed.btDeleteNuevaClick(Sender: TObject);
var
  M: Integer;
begin
  FCounter.Reset;

  M := AllocMemSize;
  with FNueva.Items do
    Delete(First, Last);
  M := AllocMemSize - M;

  lbDeleteNueva.Caption := Sil.Float.ToStr(FCounter.ToSeconds(), 10, 6);
  DoAddTest(N, M, 'DELETE', 'NUEVA', lbDeleteNueva.Caption);
end;

procedure TFormTestContainerSpeed.btDeleteLinkedClick(Sender: TObject);
var
  M: Integer;
begin
  FCounter.Reset;

  M := AllocMemSize;
  with FLinked.Items do
    Delete(First, Last);
  M := AllocMemSize - M;

  lbDeleteLinked.Caption := Sil.Float.ToStr(FCounter.ToSeconds(), 10, 6);
  DoAddTest(N, M, 'DELETE', 'LINKED', lbDeleteLinked.Caption);
end;

procedure TFormTestContainerSpeed.btDeleteViejaClick(Sender: TObject);
var
  M: Integer;
begin
  FCounter.Reset;

  M := AllocMemSize;
  with FVieja do
    while Count > 0 do
      Delete(Count - 1);
  M := AllocMemSize - M;

  lbDeleteVieja.Caption := Sil.Float.ToStr(FCounter.ToSeconds(), 10, 6);
  DoAddTest(N, M, 'DELETE', 'VIEJA', lbDeleteVieja.Caption);
end;

procedure TFormTestContainerSpeed.btDeleteDelphiClick(Sender: TObject);
var
  Data: PDataRecord;
  M: Integer;
begin
  FCounter.Reset;

  M := AllocMemSize;
  with FDelphi do
    while Count > 0 do
    begin
      Data := Pointer(Objects[Count - 1]);
      Delete(Count - 1);
      System.Dispose(Data);
    end;
  M := AllocMemSize - M;

  lbDeleteDelphi.Caption := Sil.Float.ToStr(FCounter.ToSeconds(), 10, 6);
  DoAddTest(N, M, 'DELETE', 'DELPHI', lbDeleteDelphi.Caption);
end;

procedure TFormTestContainerSpeed.btDeleteIntfClick(Sender: TObject);
var
  M: Integer;
begin
  FCounter.Reset;

  M := AllocMemSize;
  with FIntfs.Items do
    while Count > 0 do
      Delete(HItem(Count - 1));
  M := AllocMemSize - M;

  lbDeleteIntf.Caption := Sil.Float.ToStr(FCounter.ToSeconds(), 10, 6);
  DoAddTest(N, M, 'DELETE', 'INTFS', lbDeleteIntf.Caption);
end;

procedure TFormTestContainerSpeed.btSortNuevaClick(Sender: TObject);
var
  M: Integer;
begin
  FCounter.Reset;

  M := AllocMemSize;
  DoQuickSort(FNueva, 0, FNueva.Items.Count - 1);
  M := AllocMemSize - M;

  lbSortNueva.Caption := Sil.Float.ToStr(FCounter.ToSeconds(), 10, 6);
  DoAddTest(N, M, 'SORT', 'NUEVA', lbSortNueva.Caption);
end;

procedure TFormTestContainerSpeed.Button1Click(Sender: TObject);
var
  Data: RDataRecord;
  M: Integer;
  I: HItem;
begin
  FCounter.Reset;
  Data.Text := edValue.Text;

  M := AllocMemSize;
  lbResult.Caption := Sil.Str.Iif(DoSearch(FNueva, @Data, I), 'True', 'False');
  M := AllocMemSize - M;

  lbSearchNueva.Caption := Sil.Float.ToStr(FCounter.ToSeconds(), 10, 6);
  DoAddTest(N, M, 'SEARCH', 'NUEVA', lbSearchNueva.Caption);
end;

procedure TFormTestContainerSpeed.btSortViejaClick(Sender: TObject);
var
  M: Integer;
begin
  FCounter.Reset;

  M := AllocMemSize;
  Sil.Sort.Default(FVieja);
  M := AllocMemSize - M;

  lbSortVieja.Caption := Sil.Float.ToStr(FCounter.ToSeconds(), 10, 6);
  DoAddTest(N, M, 'SORT', 'VIEJA', lbSortVieja.Caption);
end;

procedure TFormTestContainerSpeed.btSortDelphiClick(Sender: TObject);
var
  M: Integer;
begin
  FCounter.Reset;

  M := AllocMemSize;
  FDelphi.Sort;
  M := AllocMemSize - M;

  lbSortDelphi.Caption := Sil.Float.ToStr(FCounter.ToSeconds(), 10, 6);
  DoAddTest(N, M, 'SORT', 'DELPHI', lbSortDelphi.Caption);
end;

procedure TFormTestContainerSpeed.btSortStringClick(Sender: TObject);
var
  M: Integer;
begin
  FCounter.Reset;

  M := AllocMemSize;
  DoQuickSort(FString.Items as IContainerDynamic, 0, FString.Items.Count - 1);
  M := AllocMemSize - M;

  lbSortString.Caption := Sil.Float.ToStr(FCounter.ToSeconds(), 10, 6);
  DoAddTest(N, M, 'SORT', 'STRING', lbSortString.Caption);
end;

procedure TFormTestContainerSpeed.btSortIntfClick(Sender: TObject);
var
  M: Integer;
begin
  FCounter.Reset;

  M := AllocMemSize;
  DoQuickSort(FIntfs as IContainerDynamic, 0, FIntfs.Items.Count - 1);
  M := AllocMemSize - M;

  lbSortIntf.Caption := Sil.Float.ToStr(FCounter.ToSeconds(), 10, 6);
  DoAddTest(N, M, 'SORT', 'INTFS', lbSortIntf.Caption);
end;

procedure TFormTestContainerSpeed.edIterationsExit(Sender: TObject);
begin
  N := Str.ToInt(edIterations.Text);
end;

procedure TFormTestContainerSpeed.DoAddTest(N: Integer; Mem: Integer; const Test, Tipo, Value: string);
begin
  if N <> 0 then
    with lvTests.Items do
      with Add do
      begin
        Caption := Int.ToStr(N);
        Subitems.Add(Test);
        Subitems.Add(Tipo);
        Subitems.Add(Value);
        Subitems.Add(Int.ToStr(Mem));
      end;
end;

procedure TFormTestContainerSpeed.btCopyClick(Sender: TObject);
var
  I, J: Integer;
  Item: TListItem;
  Line: string;
begin

  with lvTests do
  begin
    Line := '';
    for I := 0 to Items.Count - 1 do
    begin
      Item := Items[I];
      Sil.Str.Add(Line, Item.Caption, sLineBreak);
      for J := 0 to Item.Subitems.Count - 1 do
        Sil.Str.Add(Line, Item.Subitems[J], ccHT);
    end;
    Items.Clear;
  end;
  Clipboard.AsText := Line;
end;

procedure TFormTestContainerSpeed.btClearListClick(Sender: TObject);
begin
  lvTests.Clear;
end;

procedure TFormTestContainerSpeed.Timer1Timer(Sender: TObject);
begin
  lbMemory.Caption := Int.ToStr(AllocMemSize);
end;

procedure TFormTestContainerSpeed.btAddIListClick(Sender: TObject);
var
  Text: string;
  Data: IUnknown;
  I: Integer;
  M: Integer;
begin
  FIList.Clear;

  Data := Self;

  FCounter.Reset;
  M := AllocMemSize;
  for I := 0 to N do
  begin
    Text := Float.ToStr(Random(N));
    FIList.Add(Data);
  end;
  M := AllocMemSize - M;

  lbAddIList.Caption := Sil.Float.ToStr(FCounter.ToSeconds(), 10, 6);
  DoAddTest(N, M, 'ADD', 'ILIST', lbAddIList.Caption);
end;

procedure TFormTestContainerSpeed.btEnumIListClick(Sender: TObject);
var
  Enum: IEnumerator;
  Data, Item: IUnknown;
  M: Integer;
begin
  FCounter.Reset;

  M := AllocMemSize;
  with FIList do
    while Enumerate(Enum, Item) do
    begin
      Data := Item;
    end;

  M := AllocMemSize - M;

  lbEnumIList.Caption := Sil.Float.ToStr(FCounter.ToSeconds(), 10, 6);
  DoAddTest(N, M, 'ENUM', 'ILIST', lbEnumIList.Caption);
end;

procedure TFormTestContainerSpeed.btClearIListClick(Sender: TObject);
var
  M: Integer;
begin
  FCounter.Reset;

  M := AllocMemSize;
  FIList.Clear;
  M := AllocMemSize - M;

  lbClearIList.Caption := Sil.Float.ToStr(FCounter.ToSeconds(), 10, 6);
  DoAddTest(N, M, 'CLEAR', 'ILIST', lbClearIList.Caption);
end;

end.
