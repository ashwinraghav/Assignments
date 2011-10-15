unit FmOuchMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, VirtualTrees, ComCtrls, ExtCtrls, StdCtrls, SilVkCustomControl,
  SilVkControl, SilVmCustomButton, SilVmButton,

  Sil, SilTool, UiOuch, UiProtocol, UiClient;

type
  TOuchMain = class(
    TForm,
    IDispatchable,
    IReferenceable,
    IOuchApplication,
    IOuchEngineUpdateEvent,
    IOuchContactsEvents )
    pnStatus: TPanel;
    pnView: TPanel;
    pnEvents: TPanel;
    spBottom: TSplitter;
    pnClientArea: TPanel;
    pnToolbar: TPanel;
    SilAnimatedButton1: TSilButton;
    SilAnimatedButton2: TSilButton;
    btCrear: TSilButton;
    btLogon: TSilButton;
    Bevel1: TBevel;
    Bevel2: TBevel;
    Bevel3: TBevel;
    pnMessage: TPanel;
    lbMessage: TLabel;
    Panel1: TPanel;
    lbStatus: TLabel;
    lbUser: TLabel;
    OpenDialog: TOpenDialog;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btLogonClick(Sender: TObject);
    procedure btCrearClick(Sender: TObject);
    procedure OpenDialogFolderChange(Sender: TObject);
  private
    FWindows: IOuchWindowList;
    FOuchView: IOuchView;
    FOuchEvents: IOuchEventView;
    FEngine: IOuchEngine;
    FTraybar: IOuchTraybar;
    FOpenDialogPath: String;
  private
    procedure EvConnected(var Rec); message EV_OUCH_CONNECTED;
    procedure EvDisconnected(var Rec); message EV_OUCH_DISCONNECTED;
    procedure EvLoggedOn(var Rec); message EV_OUCH_LOGGEDON;
  protected // IReferenceable
    function GetInstance: Pointer;
  protected // IOuchApplication
    function GetView: IOuchView;
    function GetLog: IOuchEventView;
    function GetEngine: IOuchEngine;
    function GetWindows: IOuchWindows;
    procedure OnStatusChanged(const Sender: IOuchEngine);
    procedure OnMessage(const Msg: IOuchMessage);
    procedure Compose(const Contacts: IOuchAccounts);
    procedure FileTransfer(const Contacts: IOuchAccounts);
  protected // IOuchEngineUpdateEvents
    procedure OnUpdateNotification(const Event: ROuchEvUpdateNotification);
  protected // IOuchContactsEvents
    procedure OnUserAdd(const User: IOuchAccount);
    procedure OnUserRemove(const User: IOuchAccount);
    procedure OnUserChanged(const User: IOuchAccount);
  end;

var
  OuchMain: TOuchMain;

implementation

uses
  UtLog,
  UcOuch,
  UdOuch,
  UtOuch,
  UmOuchWindows,
  FrOuchView,
  FrOuchEvents,
  FmOuchMessage,
  FmOuchLogon,
  DmTrayBar;

{$R *.dfm}

procedure TOuchMain.FormCreate(Sender: TObject);
begin
  GlobalLog.Initialize;
  FWindows := TOuchWindowList.Create(Self);
  FOuchView := TFrameOuchView.Create(Self, pnView);
  FOuchEvents := TFrameOuchEvents.Create(Self, pnEvents);
  FTraybar := TDataTrayBar.Create(Self);
  FOuchEvents.Add('Arranca ' + Sil.Os.Module.Current.Info.Version.FullName, 'inicio');
  FEngine := Ouch.Engine(Self);
end;

procedure TOuchMain.FormDestroy(Sender: TObject);
begin
  if Assigned(FWindows) then FWindows.Clear;
  FEngine := nil;
  FTraybar := nil;
  FOuchEvents := nil;
  FOuchView := nil;
  FWindows := nil;
  GlobalLog.Finalize;
end;

function TOuchMain.GetInstance: Pointer;
begin
  Result := Self;
end;

function TOuchMain.GetView: IOuchView;
begin
  Result := FOuchView;
end;

function TOuchMain.GetLog: IOuchEventView;
begin
  Result := FOuchEvents;
end;

function TOuchMain.GetEngine: IOuchEngine;
begin
  Result := FEngine;
end;

function TOuchMain.GetWindows: IOuchWindows;
begin
  Result := FWindows;
end;

procedure TOuchMain.OnStatusChanged(const Sender: IOuchEngine);
begin
  lbStatus.Caption := ccSPC + Ouch.StateToStr(Sender.State);
  lbStatus.Hint := lbStatus.Caption; 
  if Sender.State = esLoggedOn then
    lbUser.Caption := ccSPC + Sender.Account.Nick else
    lbUser.Caption := ccSPC + '<nadie>';
  lbUser.Hint := lbUser.Caption; 
  FOuchEvents.Add(Ouch.StateToStr(Sender.State), 'estado');
end;

procedure TOuchMain.OnMessage(const Msg: IOuchMessage);
var
  View: IOuchMessageView;
begin
  if FWindows.Create(TFormOuchMessage, IOuchMessageView, View) then
    View.Show(Msg);
end;

procedure TOuchMain.OnUpdateNotification(const Event: ROuchEvUpdateNotification);
begin
  FOuchEvents.Add(Event.Status.Message, 'update');
  lbMessage.Caption := Ouch.StageToStr(Event.Status.Stage);   
  if Event.Status.Stage = usChange then
    if MessageDlg(Event.Status.Message + sLineBreak + SUpdateQuestion, mtConfirmation, [mbYes, mbNo], 0) = idYES then
      Application.Terminate;
end;

procedure TOuchMain.Compose(const Contacts: IOuchAccounts);
var
  View: IOuchMessageView;
begin
  if FWindows.Create(TFormOuchMessage, IOuchMessageView, View) then
    View.Compose(FEngine.Factory.Message(nil, Contacts));
end;

procedure TOuchMain.FileTransfer(const Contacts: IOuchAccounts);
var
  Enum: IEnumerator;
  Item: IOuchAccount;
  FileList: IFileInfoList;
  i: Integer;
begin
  if Str.IsEmpty(FOpenDialogPath) then
    FOpenDialogPath := Sil.OS.Process.Current.Info.Path;

  OpenDialog.InitialDir := FOpenDialogPath;

  if OpenDialog.Execute then
  begin
    FileList := Sil.OS.FileSystem.FileInfoList;

    for i := 0 to OpenDialog.Files.Count - 1 do
      FileList.Add(Sil.OS.FileSystem.GetInfo(OpenDialog.Files[i]));

    while Contacts.Enumerate(Enum, Item) do
      FEngine.Client.RequestFileTransfer(Item.ID, FileList);
  end;
end;

procedure TOuchMain.OnUserAdd(const User: IOuchAccount);
begin
end;

procedure TOuchMain.OnUserRemove(const User: IOuchAccount);
begin
end;

procedure TOuchMain.OnUserChanged(const User: IOuchAccount);
begin
end;

procedure TOuchMain.EvConnected(var Rec);
begin
  try
    FEngine.Logon;
  except on Ex: Exception do
    FOuchEvents.Add(Ex.Message, 'logon');
  end;
end;

procedure TOuchMain.EvDisconnected(var Rec);
begin
end;

procedure TOuchMain.EvLoggedOn(var Rec);
begin
  FEngine.Update;
end;

procedure TOuchMain.btLogonClick(Sender: TObject);
var
  Form: IOuchLogonWindow;
begin
  if FWindows.Create(TFormOuchLogon, IOuchLogonWindow, Form, True) then
  begin
    Form.LogonOnly := False;
    Form.Show;
  end;
end;

procedure TOuchMain.btCrearClick(Sender: TObject);
var
  Form: IOuchLogonWindow;
begin
  if FWindows.Create(TFormOuchLogon, IOuchLogonWindow, Form, True) then
  begin
    Form.LogonOnly := True;
    Form.Show;
  end;
end;

procedure TOuchMain.OpenDialogFolderChange(Sender: TObject);
begin
  FOpenDialogPath := Sil.OS.Process.Current.CurrentPath;
end;

end.

