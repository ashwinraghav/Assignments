unit FmTestBaseQueues;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls,

  Sil,
  SilCoder, ExtCtrls;

const
  CInterfaces = WM_USER + 1;
  CPointers = WM_USER + 2;

type
  TFormTestQueues = class(
    TForm,
    IDispatchable )
    ListBox1: TListBox;
    Label1: TLabel;
    btAdd: TButton;
    btReset: TButton;
    btAddLoop: TButton;
    btStartStop: TButton;
    tmRefreshMemory: TTimer;
    ckInterfaces: TRadioButton;
    ckPointers: TRadioButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btAddLoopClick(Sender: TObject);
    procedure btAddInterfacesClick(Sender: TObject);
    procedure btResetInterfacesClick(Sender: TObject);
    procedure btAddPointersClick(Sender: TObject);
    procedure btResetPointersClick(Sender: TObject);
    procedure btStartStopClick(Sender: TObject);
    procedure tmRefreshMemoryTimer(Sender: TObject);
    procedure ckModoChanged(Sender: TObject);
  private
    procedure ThreadInterfaces(var Msg); message CInterfaces;
    procedure ThreadPointers(var Msg); message CPointers;
    procedure DoAdd(const Sender: IUnknown; Data: Pointer);
  public
    FCounter: IPerformanceCounter;
    FPointers: IPointerQueue;
    FInterfaces: IInterfaceQueue;
    FThread: IThread;
    FEvent: IEvent;
    FList: IStringList;
    FValue: Integer;
  end;

  ITest = interface
    ['{805C244C-A7E6-47D0-9A35-82759972C393}']
    function Text: String;
  end;

  TTest = class (TSilObject, ITest)
  private
    FText: String;
  protected
    function Text: String;
  public
    constructor Create(const Text: String);
  end;

var
  FormTestQueues: TFormTestQueues;

implementation

uses SilBtStr;

{$R *.dfm}

procedure TFormTestQueues.FormCreate(Sender: TObject);
begin
  FCounter := Sil.OS.Performance.Create;
  FEvent := Sil.OS.Ipc.Event(True, True);
  FPointers := Sil.List.PointerQueue;
  FInterfaces := Sil.List.InterfaceQueue;
  FList := Sil.List.StringList(true);

  ckInterfaces.Checked := True;
end;

procedure TFormTestQueues.FormDestroy(Sender: TObject);
begin
  FPointers.Cancel;
  FInterfaces.Cancel;
  if Assigned(FThread) then
  begin
    FThread.Termination.WaitFor;
    FThread := nil;
  end;
  FEvent := nil;
  FList := nil;
end;

procedure TFormTestQueues.btAddInterfacesClick(Sender: TObject);
var
  item: ITest;
begin
  if not Assigned(FThread) then FThread := Sil.Os.Thread.Spawn(CInterfaces, Self);
  item := TTest.Create(Sil.Int.ToStr(Sil.Int.Inc(FValue)));
  FInterfaces.Put(item);
end;

procedure TFormTestQueues.btResetInterfacesClick(Sender: TObject);
begin
  FValue := 0;
  FInterfaces.Reset;
  listbox1.Clear;
end;

procedure TFormTestQueues.btAddPointersClick(Sender: TObject);
var
  text: string;
  data: PChar;
begin
  if not Assigned(FThread) then FThread := Sil.Os.Thread.Spawn(CPointers, Self);
  text := Sil.Int.ToStr(Sil.Int.Inc(FValue)); 
  data := Sil.Str.New(text);
  FPointers.Put(data);
end;

procedure TFormTestQueues.btResetPointersClick(Sender: TObject);
begin
  FValue := 0;
  FPointers.Reset;
  listbox1.Clear;
end;

procedure TFormTestQueues.btStartStopClick(Sender: TObject);
begin
  if FEvent.IsSignaled then
    FEvent.Reset else
    FEvent.Signal;
  btStartStop.Caption := Sil.Str.IIf(FEvent.IsSignaled, 'Stop', 'Start');
end;

procedure TFormTestQueues.btAddLoopClick(Sender: TObject);
var
  i: integer;
begin
  FCounter.Reset;

  for i := 1 to 1000 do
    btAdd.Click;

  caption := float.tostr(FCounter.ToMilliseconds());
end;

procedure TFormTestQueues.tmRefreshMemoryTimer(Sender: TObject);
begin
  label1.caption := int.tostr(allocmemsize);
end;

procedure TFormTestQueues.ckModoChanged(Sender: TObject);
begin
  btReset.Click;
  
  if ckInterfaces.Checked then
  begin
    btAdd.OnClick := btAddInterfacesClick;
    btReset.OnClick := btResetInterfacesClick;
  end else
  begin
    btAdd.OnClick := btAddPointersClick;
    btReset.OnClick := btResetPointersClick;
  end;
end;

procedure TFormTestQueues.ThreadInterfaces(var Msg);
var
  item: ITest;
begin
  while FInterfaces.Get(ITest, item) do
  try
    Sil.Os.Thread.SyncCall(DoAdd, PChar(item.Text));
    FEvent.WaitFor(INFINITE);
  finally
    item := nil;
  end;
  FThread := nil;
end;

procedure TFormTestQueues.ThreadPointers(var Msg);
var
  ptext: PChar;
begin
  while FPointers.Get(ptext) do
  try
    Sil.Os.Thread.SyncCall(DoAdd, ptext);
    FEvent.WaitFor(INFINITE);
  finally
    FreeMem(ptext);
  end;
  FThread := nil;
end;

procedure TFormTestQueues.DoAdd(const Sender: IInterface; Data: Pointer);
begin
  listbox1.items.add(PChar(Data));
end;

{ TTest }

constructor TTest.Create(const Text: String);
begin
  inherited Create;
  FText := Text;
end;

function TTest.Text: String;
begin
  Result := FText;
end;

end.
