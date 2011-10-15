unit FmThreadSort;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, Sil;

type
  TSortKind = (skBubble, skSelection, skQuick);
  TSortBuffer = array of Integer;

const
  CThreadID = EV_FIRST + 1;

const
  CThreadBubble = CThreadID + Ord(skBubble);
  CThreadSelection = CThreadID + Ord(skSelection);
  CThreadQuick = CThreadID + Ord(skQuick);

type
  PSort = ^RSort;
  RSort = record
    Buffer: TSortBuffer;
    Painter: TPaintBox;
  end;

type
  PEvUpdate = ^REvUpdate;
  REvUpdate = record
    Box: TPaintBox;
    I, J, A, B: Integer;
  end;

type
  TFormThreadSort = class(
    TForm,
    IDispatchable,
    IThreadHook )
    btStart: TButton;
    pbBubbleSort: TPaintBox;
    pbSelectionSort: TPaintBox;
    pbQuickSort: TPaintBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    lbCount: TLabel;
    lbTimeBubble: TLabel;
    lbTimeSelection: TLabel;
    lbTimeQuick: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure pbBubbleSortPaint(Sender: TObject);
    procedure pbSelectionSortPaint(Sender: TObject);
    procedure pbQuickSortPaint(Sender: TObject);
    procedure btStartClick(Sender: TObject);
  private
    FStarted: IEvent;
    FSort: array[TSortKind] of RSort;
    FRunning: LongWord;
    FRandomized: Boolean;         
  private
    procedure ThreadBubble(var Msg: RThreadRunMessage); message CThreadID + Ord(skBubble); 
    procedure ThreadQuick(var Msg: RThreadRunMessage); message CThreadID + Ord(skQuick); 
    procedure ThreadSelection(var Msg: RThreadRunMessage); message CThreadID + Ord(skSelection); 
  private
    procedure DoRandomize;
    procedure DoPaint(const Sort: RSort);
    procedure DoSwap(Sort: PSort; I, J: Integer);
    procedure DoUpdate(const Sender: IUnknown; Param: Pointer); overload;               
    procedure DoThreadDone(const Sender: IUnknown; Param: Pointer);
  protected // IThreadHook
    function Initialize(const Thread: IThread): Boolean;
    procedure Finalize(const Thread: IThread);
    procedure Suspended(const Thread: IThread);
    procedure Resumed(const Thread: IThread);
  end;

var
  FormThreadSort: TFormThreadSort;

implementation

{$R *.dfm}

procedure PaintLine(Canvas: TCanvas; I, Len: Integer);
begin
  Canvas.PolyLine([Classes.Point(0, I), Classes.Point(Len, I)]);
end;

{ TThreadSortForm }

procedure TFormThreadSort.FormCreate(Sender: TObject);
begin
  FStarted := Sil.Os.IPC.Event();
  FSort[skSelection].Painter := pbSelectionSort; 
  FSort[skBubble].Painter := pbBubbleSort;
  FSort[skQuick].Painter := pbQuickSort; 
  DoRandomize;
end;

procedure TFormThreadSort.pbBubbleSortPaint(Sender: TObject);
begin
  DoPaint(FSort[skBubble]);
end;

procedure TFormThreadSort.pbSelectionSortPaint(Sender: TObject);
begin
  DoPaint(FSort[skSelection]);
end;

procedure TFormThreadSort.pbQuickSortPaint(Sender: TObject);
begin
  DoPaint(FSort[skQuick]);
end;

procedure TFormThreadSort.btStartClick(Sender: TObject);
begin
  btStart.Enabled := False;

  DoRandomize;

  Sil.Os.Thread.Spawn(CThreadBubble, 'bubble', Self);
  Sil.Os.Thread.Spawn(CThreadSelection, 'selection', Self);
  Sil.Os.Thread.Spawn(CThreadQuick, 'quick', Self);

  FStarted.Signal;
end;

procedure TFormThreadSort.DoThreadDone(const Sender: IUnknown; Param: Pointer);
begin
  btStart.Enabled := True;
  FRandomized := False;
end;

procedure TFormThreadSort.ThreadBubble(var Msg: RThreadRunMessage);
var
  Sort: PSort;
  I, J: Integer;
begin
  FStarted.WaitFor();

  Sort := @FSort[skBubble];

  for I := High(Sort.Buffer) downto Low(Sort.Buffer) do
    for J := Low(Sort.Buffer) to High(Sort.Buffer) - 1 do
      if Sort.Buffer[J] > Sort.Buffer[J + 1] then
        DoSwap(Sort, J, J + 1);
end;

procedure TFormThreadSort.ThreadQuick(var Msg: RThreadRunMessage);
var
  Sort: PSort;

  procedure DoQuickSort(var A: TSortBuffer; iLo, iHi: Integer);
  var
    Lo, Hi, Mid: Integer;
  begin
    Lo := iLo;
    Hi := iHi;
    Mid := A[(Lo + Hi) div 2];
    repeat
      while A[Lo] < Mid do Inc(Lo);
      while A[Hi] > Mid do Dec(Hi);
      if Lo <= Hi then
      begin
        DoSwap(Sort, Lo, Hi);
        Inc(Lo);
        Dec(Hi);
      end;
    until Lo > Hi;
    if Hi > iLo then DoQuickSort(A, iLo, Hi);
    if Lo < iHi then DoQuickSort(A, Lo, iHi);
  end;

begin
  FStarted.WaitFor();

  Sort := @FSort[skQuick];

  DoQuickSort(Sort.Buffer, Low(Sort.Buffer), High(Sort.Buffer));
end;

procedure TFormThreadSort.ThreadSelection(var Msg: RThreadRunMessage);
var
  Sort: PSort;
  I, J: Integer;
begin
  FStarted.WaitFor();

  Sort := @FSort[skSelection];

  for I := Low(Sort.Buffer) to High(Sort.Buffer) - 1 do
    for J := High(Sort.Buffer) downto I + 1 do
      if Sort.Buffer[I] > Sort.Buffer[J] then
        DoSwap(Sort, I, J);
end;

procedure TFormThreadSort.DoPaint(const Sort: RSort);
var
  I: Integer;
begin
  with Sort, Painter do
  begin
    Canvas.Pen.Color := clRed;
    for I := Low(Buffer) to High(Buffer) do PaintLine(Canvas, I, Buffer[I]);
  end;
end;

procedure TFormThreadSort.DoSwap(Sort: PSort; I, J: Integer);
var
  T: Integer;
  U: REvUpdate;
begin
  U.Box := Sort.Painter;
  U.I := I;
  U.J := J;
  U.A := Sort.Buffer[I];
  U.B := Sort.Buffer[J];

  T := Sort.Buffer[J];
  Sort.Buffer[J] := Sort.Buffer[I];
  Sort.Buffer[I] := T;

  Sil.Os.Thread.SyncCall(DoUpdate, @U);
end;

procedure TFormThreadSort.DoUpdate(const Sender: IInterface; Param: Pointer);
var
  Data: PEvUpdate;
begin
  Data := PEvUpdate(Param);
  with Data.Box do
  begin
    Canvas.Pen.Color := clBtnFace;
    PaintLine(Canvas, Data.I, Data.A);
    PaintLine(Canvas, Data.J, Data.B);
    Canvas.Pen.Color := clRed;
    PaintLine(Canvas, Data.I, Data.B);
    PaintLine(Canvas, Data.J, Data.A);
  end;
end;

procedure TFormThreadSort.DoRandomize;
var
  I, N: Integer;
  Data: TSortBuffer;
begin
  if not FRandomized then
  begin
    System.Randomize();

    N := pbBubbleSort.Height;

    SetLength(Data, N);
    lbCount.Caption := Sil.Str.Format('N = %d', [N]);

    for I := Low(Data) to High(Data) do
      Data[I] := System.Random(pbBubbleSort.Width);

    with FSort[skBubble] do
    begin
      SetLength(Buffer, N);
      System.Move(Data[0], Buffer[0], N * SizeOf(Integer));
    end;

    with FSort[skQuick] do
    begin
      SetLength(Buffer, N);
      System.Move(Data[0], Buffer[0], N * SizeOf(Integer));
    end;

    with FSort[skSelection] do
    begin
      SetLength(Buffer, N);
      System.Move(Data[0], Buffer[0], N  * SizeOf(Integer));
    end;

    FRandomized := True;

    Repaint;
  end;
end;

function TFormThreadSort.Initialize(const Thread: IThread): Boolean;
begin
  Sil.Os.Locked.Increment(@FRunning);
  Result := True;
end;

procedure TFormThreadSort.Finalize(const Thread: IThread);
begin
  if Sil.Os.Locked.Decrement(@FRunning) = 0 then
    Sil.Os.Thread.SyncCall(DoThreadDone);
end;

procedure TFormThreadSort.Resumed(const Thread: IThread);
begin
  {do nothing}
end;

procedure TFormThreadSort.Suspended(const Thread: IThread);
begin
  {do nothing}
end;

end.
