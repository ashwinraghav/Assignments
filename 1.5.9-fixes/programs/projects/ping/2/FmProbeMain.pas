unit FmProbeMain;

interface

uses
  Windows, WinSock, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, ComCtrls,

  Sil,
  SilData,
  SilIcmp, WTaskBar, Menus;

type
  IPingTarget = interface;
  
  TOpenProc = procedure(const TableDef: IDataRowsetDef) of object;
  
  RReplyData = record
    Item: IPingTarget;
    From: ISocketAddress;
    Reply: REcho;
    Elapsed: Double;
  end;

  IOwner = interface (IDispatchable)
    ['{F32955A9-9392-40EC-B0FF-F33575C8499B}']
    function GetIcmp: IIcmpEcho;
    procedure AddReply(const Item: IPingTarget; const From: ISocketAddress; const Reply: REcho; const Elapsed: Double);
    property Icmp: IIcmpEcho read GetIcmp;
  end;

  IPingTarget = interface
    ['{4D9D41B7-020B-44E1-B89E-C595BE265C1D}']
    function GetIsActive: Boolean;
    function GetName: string;
    function GetTarget: ISocketAddress;
    function GetInterval: LongWord;
    procedure Activate;
    procedure Deactivate;
    property IsActive: Boolean read GetIsActive;
    property Name: string read GetName;
    property Target: ISocketAddress read GetTarget;
    property Interval: LongWord read GetInterval;
  end;

const
  EV_REPLY = EV_FIRST + 784;

type
  TEvReply = record
    Msg: Integer;
    Data: ^RReplyData;
    _Dummy: Integer;
    Result: Integer;
  end;

type
  TFormPingMain = class(
    TForm,
    IDispatchable,
    IOwner )
    btAgregar: TButton;
    edTarget: TEdit;
    btEliminar: TButton;
    lvTargets: TListView;
    edInterval: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    btControl: TButton;
    tkTaskbar: TWinTaskbar;
    mnPopup: TPopupMenu;
    itTerminate: TMenuItem;
    itmShow: TMenuItem;
    N1: TMenuItem;
    procedure btAgregarClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btEliminarClick(Sender: TObject);
    procedure lvTargetsDeletion(Sender: TObject; Item: TListItem);
    procedure lvTargetsChange(Sender: TObject; Item: TListItem; Change: TItemChange);
    procedure btControlClick(Sender: TObject);
    procedure itTerminateClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure itmShowClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure IsPasswordOK(Sender: TObject; var CanClose: Boolean);
  private
    FCounter: IPerformanceCounter;
    FDataPath: string;
    FIcmp: IIcmpEcho;
    FMessenger: IMessenger;
    FTarget: IDataRowset;
    FHistorico: IDataRowset;
  private
    procedure EvReply(var Msg: TEvReply); message EV_REPLY;
  private
    procedure DoAddReply(const Data: RReplyData);
    procedure DoAddTarget(const Target: string; Interval: LongWord; Activate: Boolean = True);
    procedure DoAddTargets;
    function DoOpenTable(const Name: String; BuildProc: TOpenProc): IDataRowset;
    procedure DoOpenHistorico(const TableDef: IDataRowsetDef);
    procedure DoOpenTarget(const TableDef: IDataRowsetDef);
    function DoDefineTarget(const Item: IPingTarget): Integer;
    procedure DoDeleteTarget(const Item: IPingTarget);
    procedure DoDefineHistorico(const Data: RReplyData);
    function DoCheckPassword(const APassword: string): Boolean;
    function DoAuthenticate: Boolean;
  protected // IOwner
    function GetIcmp: IIcmpEcho;
    procedure AddReply(const Item: IPingTarget; const From: ISocketAddress; const Reply: REcho; const Elapsed: Double);
  end;

  TPingTarget = class(
    TSilObject,
    IRunnable,
    IPingTarget )
  private
    FOwner: Pointer;
    FName: string;
    FCounter: IPerformanceCounter;
    FThread: IThread;
    FTarget: ISocketAddress;
    FTerminated: IEvent;
    FInterval: LongWord;
    FSequence: Integer;
  private 
    function DoGetOwner: IOwner;
  protected // IRunnable
    procedure Run(const Thread: IThread);
  protected // IPingTarget
    function GetIsActive: Boolean;
    function GetName: string;
    function GetTarget: ISocketAddress;
    function GetInterval: LongWord;
    procedure Activate;
    procedure Deactivate;
  protected
    property Owner: IOwner read DoGetOwner;
  public
    constructor Create(const Owner: IOwner; const Name: string; Interval: LongWord);
    destructor Destroy; override;
  end;

var
  FormPingMain: TFormPingMain;

implementation

uses
  FPassword;

var
  Seq: Integer = 0;

{$R *.dfm}

{ TPingTarget }

constructor TPingTarget.Create(const Owner: IOwner; const Name: string; Interval: LongWord);
begin
  inherited Create;
  FOwner := Pointer(Owner);
  FName := Name;
  FCounter := Sil.Os.Performance.Create();
  FTerminated := Sil.Os.Ipc.Event();
  FInterval := Interval;
  FTarget := Sil.Os.Socket.IP.Create(Name, 0);
end;

destructor TPingTarget.Destroy;
begin
  Deactivate;
  FTarget := nil;
  FTerminated := nil;
  FCounter := nil;
  FOwner := nil;
  inherited;
end;

function TPingTarget.GetIsActive: Boolean;
begin
  Result := Assigned(FThread);
end;

function TPingTarget.GetName: string;
begin
  Result := FName;
end;

function TPingTarget.GetTarget: ISocketAddress;
begin
  Result := FTarget;
end;

function TPingTarget.GetInterval: LongWord;
begin
  Result := FInterval;
end;

procedure TPingTarget.Activate;
begin
  if not Assigned(FThread) then
    FThread := Sil.Os.Thread.Spawn(Self);
end;

procedure TPingTarget.Deactivate;
begin
  if Assigned(FThread) then
  begin
    FTerminated.Signal;
    FThread.Termination.WaitFor(INFINITE, True);
    FThread := nil;
  end;
end;

procedure TPingTarget.Run(const Thread: IThread);
var
  Socket: ISocketClient;
  Dest: ISocketAddress;
  Query, Reply: REcho;
  Elapsed: Double;
begin
  Sil.Trace.Enter(Self, 'Run'); 

  Socket := Sil.Os.Socket.CreateClient(stRaw, spICMP);
  Socket.Parameters.ReadTimeout := 2000;

  while FTerminated.WaitFor(FInterval) <> wrSignaled do
  try
    Dest := FTarget;

    Query.Id := (GetCurrentProcessId() shl 8) + Thread.ThreadID;
    Query.Sequence := Sil.Os.Locked.Increment(FSequence);
    Query.Data := Sil.Str.Replicate('HOLA!', 10);

    FCounter.Reset();

    Reply := Owner.Icmp.Ping(Socket, Dest, Query);

    Elapsed := FCounter.ToMilliseconds();

    if not Assigned(Dest) then
    begin
      Reply.Id := Query.Id;
      Reply.Sequence := Query.Sequence;
    end;

    Owner.AddReply(Self, Dest, Reply, Elapsed);  
  except
    Sil.Trace.Exception();
  end;
  Sil.Trace.Leave;
end;

function TPingTarget.DoGetOwner: IOwner;
begin
  Result := IOwner(FOwner);
end;

{ TFormPingMain }

procedure TFormPingMain.FormCreate(Sender: TObject);
begin
  FDataPath := Sil.Os.FileSystem.AddSlash(Sil.Os.Process.Current.Info.Path);
  FTarget := DoOpenTable('maquinas.data', DoOpenTarget);
  FHistorico := DoOpenTable('historico.data', DoOpenHistorico);
  FMessenger := Sil.Os.Messenger.Global;
  FCounter := Sil.Os.Performance.Create();
  FIcmp := SilIcmp.Command.Echo.Create;
  DoAddTargets;
  tkTaskbar.ShowIcon := True;
end;

procedure TFormPingMain.FormDestroy(Sender: TObject);
begin
  tkTaskbar.ShowIcon := False;
  FIcmp := nil;
  FCounter := nil;
  FMessenger := nil;
  FTarget := nil;
  FHistorico := nil;
end;

procedure TFormPingMain.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caNone;
  Hide;
end;

procedure TFormPingMain.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_ESCAPE:  Hide;
    else        Exit;
  end;
  Key := 0;
end;

procedure TFormPingMain.itTerminateClick(Sender: TObject);
begin
  if DoAuthenticate() then
    Application.Terminate;
end;

procedure TFormPingMain.itmShowClick(Sender: TObject);
begin
  Show;
end;

procedure TFormPingMain.btAgregarClick(Sender: TObject);
begin
  DoAddTarget(edTarget.Text, Sil.Str.ToInt(edInterval.Text, 1000));
end;

procedure TFormPingMain.btEliminarClick(Sender: TObject);
var
  Instance: IPingTarget;
  Item: TListItem;
begin
  Item := lvTargets.Selected; 
  if Assigned(Item) then
  try
    if Sil.Ref.Extract(Item.Data, IPingTarget, Instance) then
    try
      DoDeleteTarget(Instance);
    finally
      Instance := nil;
    end;
  finally
    Item.Delete;
  end;
end;

procedure TFormPingMain.lvTargetsChange(Sender: TObject; Item: TListItem; Change: TItemChange);
var
  Instance: IPingTarget;
begin
  if Assigned(Item) then
    if Sil.Ref.Extract(Item.Data, IPingTarget, Instance) then
    try
      btControl.Caption := Sil.Str.Iif(Instance.IsActive, '&Desactivar', '&Activar');
    finally
      Instance := nil;
    end;
end;

procedure TFormPingMain.btControlClick(Sender: TObject);
var
  Item: TListItem;
  Instance: IPingTarget;
begin
  Item := lvTargets.Selected;

  if Assigned(Item) then
    if Sil.Ref.Extract(Item.Data, IPingTarget, Instance) then
    try
      if Instance.IsActive then
        Instance.Deactivate else
        Instance.Activate;
      lvTargetsChange(lvTargets, Item, ctState);
      DoDefineTarget(Instance);
    finally
      Instance := nil;
    end;
end;

procedure TFormPingMain.lvTargetsDeletion(Sender: TObject; Item: TListItem);
var
  Instance: IPingTarget;
begin
  if Assigned(Item) then
    if Sil.Ref.Extract(Item.Data, IPingTarget, Instance) then
    try
      Instance.Deactivate;
    finally
      Item.Data := Sil.Ref.Release(Item.Data);
      Instance := nil;
    end;
end;

procedure TFormPingMain.IsPasswordOK(Sender: TObject; var CanClose: Boolean);
begin
  with Sender as TPasswordDlg do
    if ModalResult = mrOK then
    begin
      CanClose := DoCheckPassword(Password.Text);
      if not CanClose then
      begin
        Password.Text := '';
        MessageDlg('Ha ingresado una password inválida', mtError, [mbOK], 0);
        Password.SetFocus();
      end;
    end;
end;

function TFormPingMain.GetIcmp: IIcmpEcho;
begin
  Result := FIcmp;
end;

procedure TFormPingMain.DoAddReply(const Data: RReplyData);
var
  Item: TListItem;
begin
  Item := lvTargets.FindCaption(0, Data.Item.Name, False, True, False);
  if not Assigned(Item) then
  begin
    Item := lvTargets.Items.Add();
    Item.Caption := Data.Item.Name;
    Item.SubItems.Add('');
    Item.SubItems.Add('');
    Item.SubItems.Add('');
    Item.SubItems.Add('');
  end;

  Item.SubItems[0] := Sil.Int.ToStr(Data.Reply.Sequence);
  Item.SubItems[1] := Data.Item.Target.Host;
  Item.SubItems[2] := Data.Item.Target.Format;

  if Assigned(Data.From) then
    Item.SubItems[3] := Sil.Float.Fmt(Data.Elapsed, '#0.0#') else
    Item.SubItems[3] := 'timeout';

  DoDefineHistorico(Data);  
end;

procedure TFormPingMain.DoAddTarget(const Target: string; Interval: LongWord; Activate: Boolean);
var
  Instance: IPingTarget;
  Item: TListItem;
begin
  Instance := TPingTarget.Create(Self, Target, Interval);

  Item := lvTargets.Items.Add();
  Item.Caption := Instance.Name;
  Item.Data := Sil.Ref.AddRef(Instance);
  Item.SubItems.Add('');
  Item.SubItems.Add('');
  Item.SubItems.Add('');
  Item.SubItems.Add('');

  if Activate then Instance.Activate;
  DoDefineTarget(Instance);
end;

procedure TFormPingMain.DoAddTargets;
begin
  FTarget.ActiveIndexName := 'target';
  FTarget.First;
  while not FTarget.IsEof do
  begin
    DoAddTarget(FTarget.Fields['target'].AsString, FTarget.Fields['interval'].AsLongWord, FTarget.Fields['active'].AsBoolean);
    FTarget.Next;
  end;
end;

function TFormPingMain.DoOpenTable(const Name: String; BuildProc: TOpenProc): IDataRowset;
var
  TableDef: IDataRowsetDef;
  FileName: String;
begin
  FileName := FDataPath + Name;
  Sil.Trace.Enter('TData.DoOpenTable', [FileName]);

  if not Sil.OS.FileSystem.Exists(FileName) then
  begin
    TableDef := SilData.Tk.CreateFile(FileName, fmAccessReadWrite, fmShareReadWrite);
    BuildProc(TableDef);
    TableDef.Build(Name);
    Result := TableDef.Rowset;
  end else
    Result := SilData.Tk.OpenFile(FileName, fmAccessReadWrite, fmShareReadWrite);

  Sil.Trace.Leave;
end;

procedure TFormPingMain.DoOpenHistorico(const TableDef: IDataRowsetDef);
begin
  with TableDef.Fields do
  begin
    CreateItem('timestamp'  , ftLargeInt  );
    CreateItem('fecha'      , ftDateTime  );
    CreateItem('target'     , ftInteger   );
    CreateItem('secuencia'  , ftInteger   );
    CreateItem('delay'      , ftFloat     );
    CreateItem('timedout'   , ftBoolean   );
  end;

  with TableDef.Indexes do
  begin
    CreateItem('timestamp'  , 'timestamp'       , [ixUnique]);
    CreateItem('fecha'      , 'fecha'           );
    CreateItem('target'     , 'target'          );
    CreateItem('secuencia'  , 'target,secuencia');
    CreateItem('timedout'   , 'target,timedout' );
  end;
end;

procedure TFormPingMain.DoOpenTarget(const TableDef: IDataRowsetDef);
begin
  with TableDef.Fields do
  begin
    CreateItem('id'         , ftInteger   );
    CreateItem('target'     , ftString    ,   80);
    CreateItem('nombre'     , ftString    ,  128);
    CreateItem('address'    , ftLongWord  );
    CreateItem('interval'   , ftLongWord  );
    CreateItem('active'     , ftBoolean   );
  end;

  with TableDef.Indexes do
  begin
    CreateItem('id'         , 'id'        , [ixUnique]);
    CreateItem('target'     , 'target'    , [ixIgnoreCase]);
    CreateItem('nombre'     , 'nombre'    );
    CreateItem('address'    , 'address'   );
    CreateItem('active'     , 'active'    );
  end;
end;

function TFormPingMain.DoDefineTarget(const Item: IPingTarget): Integer;
var
  Values: IDataRowsetFind;
  RecordNo: LongWord;
  ActiveIndex: String;
begin
  Sil.Trace.Enter(Self, 'DoDefineTarget', [Item.Name]);

  ActiveIndex := FTarget.ActiveIndexName;
  RecordNo := FTarget.CurrentRecord;

  try
    FTarget.ActiveIndexName := 'target';

    Values := FTarget.FindValues;
    Values.Add(Item.Name);

    if not Values.Find then
    begin
      FTarget.ActiveIndexName := 'id';

      if not FTarget.IsEmpty then
      begin
        FTarget.Last;
        Result := FTarget.Fields['id'].AsInteger + 1;
      end else
        Result := 1;

      FTarget.Append;
      FTarget.Fields['id'].AsInteger := Result;
      FTarget.Fields['target'].AsString := Item.Name;
      FTarget.Fields['nombre'].AsString := Item.Target.Host;
      FTarget.Fields['address'].AsLongWord := Item.Target.Address;
      FTarget.Fields['interval'].AsLongWord := Item.Interval;
      FTarget.Fields['active'].AsBoolean := Item.IsActive;
      FTarget.Post;
    end else
    begin
      Result := FTarget.Fields['id'].AsInteger;
      FTarget.Edit;
      FTarget.Fields['nombre'].AsString := Item.Target.Host;
      FTarget.Fields['address'].AsLongWord := Item.Target.Address;
      FTarget.Fields['interval'].AsLongWord := Item.Interval;
      FTarget.Fields['active'].AsBoolean := Item.IsActive;
      FTarget.Post;
    end;

  finally
    FTarget.ActiveIndexName := ActiveIndex;
    FTarget.CurrentRecord := RecordNo;
  end;

  Sil.Trace.Leave;
end;

procedure TFormPingMain.DoDeleteTarget(const Item: IPingTarget);
var
  Values: IDataRowsetFind;
  RecordNo: LongWord;
  ActiveIndex: String;
begin
  Sil.Trace.Enter(Self, 'DoDeleteTarget', [Item.Name]);

  ActiveIndex := FTarget.ActiveIndexName;
  RecordNo := FTarget.CurrentRecord;

  try
    FTarget.ActiveIndexName := 'target';

    Values := FTarget.FindValues;
    Values.Add(Item.Name);

    if Values.Find then
      FTarget.Delete;

  finally
    FTarget.ActiveIndexName := ActiveIndex;
    FTarget.CurrentRecord := RecordNo;
  end;

  Sil.Trace.Leave;
end;

procedure TFormPingMain.DoDefineHistorico(const Data: RReplyData);
var
  RecordNo: LongWord;
  ActiveIndex: String;
  TimeStamp: TDateTime;
begin
  Sil.Trace.Enter(Self, 'DoDefineHistorico');

  ActiveIndex := FHistorico.ActiveIndexName;
  RecordNo := FHistorico.CurrentRecord;

  try
    FHistorico.Append;
    TimeStamp := Sil.DateTime.Now;
    FHistorico.Fields['timestamp'].AsLargeInt := Sil.DateTime.ToMilisecs(TimeStamp);
    FHistorico.Fields['fecha'].AsDateTime := TimeStamp;
    FHistorico.Fields['target'].AsInteger := DoDefineTarget(Data.Item);
    FHistorico.Fields['secuencia'].AsInteger := Data.Reply.Sequence;
    FHistorico.Fields['timedout'].AsBoolean := not Assigned(Data.From);
    FHistorico.Fields['delay'].AsFloat := Data.Elapsed;
    FHistorico.Post;

  finally
    FHistorico.ActiveIndexName := ActiveIndex;
    FHistorico.CurrentRecord := RecordNo;
  end;

  Sil.Trace.Leave;
end;

function TFormPingMain.DoCheckPassword(const APassword: string): Boolean;
begin
  Result := (APassword = 'clotilde');
end;

function TFormPingMain.DoAuthenticate: Boolean;
begin
  with TPasswordDlg.create(self) do
  try
    OnCloseQuery := IsPasswordOK;
    ShowModal;
    Result := (ModalResult = mrOK);
  finally
    Free;
  end;
end;

procedure TFormPingMain.AddReply(const Item: IPingTarget; const From: ISocketAddress; const Reply: REcho; const Elapsed: Double);
var
  Data: RReplyData;
begin
  Data.Item := Item; 
  Data.From := From;
  Data.Reply := Reply;
  Data.Elapsed := Elapsed;
  FMessenger.Send(Self, EV_REPLY, @Data, 0);
end;

procedure TFormPingMain.EvReply(var Msg: TEvReply);
begin
  DoAddReply(Msg.Data^);
end;

end.
