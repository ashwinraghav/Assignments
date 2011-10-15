unit FmTestMain;

interface

uses
  SysUtils, Classes, Controls, Forms, StdCtrls, ExtCtrls, ComCtrls, Dialogs,

  Sil, SilVCL,

  UiOuchProtocol,
  UiOuchEngine;

type
  TForm1 = class(
    TForm,
    IUnknown,
    IOuchConnectionEvents,
    IOuchSessionEvents )
    Panel1: TPanel;
    edHost: TEdit;
    edPort: TEdit;
    Label1: TLabel;
    btConnect: TButton;
    btDisconnect: TButton;
    Panel2: TPanel;
    Label2: TLabel;
    edSessionID: TEdit;
    Label3: TLabel;
    edUserID: TEdit;
    btLogin: TButton;
    btLogout: TButton;
    edUser: TEdit;
    Label4: TLabel;
    Label5: TLabel;
    edPassword: TEdit;
    Events: TMemo;
    Objects: TTreeView;
    btQueryGroups: TButton;
    edGroupName: TEdit;
    edGroupID: TEdit;
    edGroupParent: TEdit;
    edOnlineUserName: TEdit;
    edOnlineUserID: TEdit;
    btInfo: TButton;
    edOnlineAddress: TEdit;
    edOnlinePort: TEdit;
    btChangeStatus: TButton;
    btSendMessage: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btConnectClick(Sender: TObject);
    procedure btDisconnectClick(Sender: TObject);
    procedure btLoginClick(Sender: TObject);
    procedure btLogoutClick(Sender: TObject);
    procedure btQueryGroupsClick(Sender: TObject);
    procedure ObjectsDeletion(Sender: TObject; Node: TTreeNode);
    procedure ObjectsChange(Sender: TObject; Node: TTreeNode);
    procedure ObjectsDblClick(Sender: TObject);
    procedure btInfoClick(Sender: TObject);
    procedure btChangeStatusClick(Sender: TObject);
    procedure btSendMessageClick(Sender: TObject);
  private
    FLibrary: ISharedLibrary;
    FLocalUser: IOuchLocalUser;
    FEngine: IOuchEngine;
    FConnection: IOuchConnection;
    FSession: IOuchSession;
    FGroups: IOuchGroups;
    FGroupsNode: TTreeNode;
    FUsersNode: TTreeNode;
    procedure DoLogReply(const Sender: IUnknown; const Ref);
    procedure DoFillGroups(const Sender: IUnknown; const Ref);
    procedure DoSessionJoin(const Sender: IUnknown; const Ref);
    procedure DoFillUsers(const Data: ROuchSessionJoin);
    //@ function DoLoadUser(const ID: TGUID): IOuchLocalUser;
  protected
    procedure OnConnectionClosed(const Event: ROuchConnectionEvent);
    procedure OnConnectionEstablished(const Event: ROuchConnectionEvent);
    procedure OnConnectionFailed(const Event: ROuchConnectionEvent);
  protected
    procedure OnSessionEstablished(const Event: ROuchSessionReply);
    procedure OnSessionFailed(const Event: ROuchSessionReply);
    procedure OnSessionError(const Event: ROuchSessionReply);
    procedure OnSessionGroups(const Event: ROuchSessionGroups);
    procedure OnSessionJoin(const Event: ROuchSessionJoin);
    procedure OnSessionClosed(const Event: ROuchSessionEvent);
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses FmUserInfo, SilStConfiguration;

{$R *.DFM}

procedure TForm1.FormCreate(Sender: TObject);
var
  EngineFunc: TOuchEngineFunction;
begin
  FLibrary := Sil.Os.SharedLibrary.Load(KOuchEngineLibraryName);
  FLibrary.Bind(KOuchEngineFunctionName, EngineFunc);
  FEngine := EngineFunc();
  FGroupsNode := Objects.Items.Add(nil, 'groups');
  FUsersNode := Objects.Items.Add(nil, 'users');
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  if FSession <> nil then FSession := nil;
  if FConnection <> nil then FConnection.Disconnect();
  FEngine := nil;
  FLibrary := nil;
end;

procedure TForm1.btConnectClick(Sender: TObject);
var
  Param: IParameterList;
begin
  Param := Sil.List.Parameters();
  Param['Server.Address'] := edHost.Text;
  Param['Server.Port'] := edPort.Text;
  Param['Server.ReadTimeout'] := 1000;
  Param['Server.WriteTimeout'] := 1000;
  Param['Server.RetryInterval'] := 5000;
  Param['Server.RetryCount'] := 10;
  Param['Local.Port'] := 32123;
  Param['Local.ReadTimeout'] := 1000;
  Param['Local.WriteTimeout'] := 1000;
  FEngine.Connect(Param, Self);
end;

procedure TForm1.btDisconnectClick(Sender: TObject);
begin
  if Assigned(FConnection) then
    FConnection.Disconnect();
end;

procedure TForm1.btLoginClick(Sender: TObject);
var
  User: TGUID;
begin
  if Sil.Str.NotEmpty(edUser.Text) then
    User := Sil.GUID.FromStr(edUser.Text) else
    User := Sil.GUID.Null;
  FLocalUser := FEngine.Toolkit.NewLocalUser(User, 'name@dominio.com.ar', 'Uno', 'Usuario Uno', edPassword.Text);
  if Assigned(FConnection) then
    FConnection.Logon(FLocalUser, Self);
end;

procedure TForm1.btLogoutClick(Sender: TObject);
begin
  if Assigned(FSession) then
    FSession.Close();
end;

procedure TForm1.btQueryGroupsClick(Sender: TObject);
begin
  if Assigned(FSession) then
    FSession.Query(Self);
end;

procedure TForm1.OnConnectionClosed(const Event: ROuchConnectionEvent);
begin
  FConnection := nil;
  VCL.SetProp.Value(Self, 'Caption', 'desconectado');
end;

procedure TForm1.OnConnectionEstablished(const Event: ROuchConnectionEvent);
begin
  FConnection := Event.Sender;
  VCL.SetProp.Value(Self, 'Caption', 'conexión establecida');
end;

procedure TForm1.OnConnectionFailed(const Event: ROuchConnectionEvent);
begin
  VCL.SetProp.Value(Self, 'Caption', 'Falló la conexión');
end;

procedure TForm1.OnSessionEstablished(const Event: ROuchSessionReply);
begin
  FSession := Event.Sender;
  VCL.SetProp.Value(edSessionID, 'Text', Sil.GUID.ToStr(FSession.Data.Handle));
  VCL.SetProp.Value(edUserID,    'Text', Sil.GUID.ToStr(FSession.Data.User.ID));
  VCL.SetProp.Value(Self, 'Caption', 'sesión establecida');
  Event.Thread.SyncCall(DoLogReply, Event.Reply);
end;

procedure TForm1.OnSessionError(const Event: ROuchSessionReply);
begin
  VCL.SetProp.Value(Self, 'Caption', 'Error');
  Event.Thread.SyncCall(DoLogReply, Event.Reply);
end;

procedure TForm1.OnSessionFailed(const Event: ROuchSessionReply);
begin
  VCL.SetProp.Value(Self, 'Caption', 'Falló el inicio de la sesión');
  Event.Thread.SyncCall(DoLogReply, Event.Reply);
end;

procedure TForm1.OnSessionClosed(const Event: ROuchSessionEvent);
begin
  VCL.SetProp.Value(edSessionID, 'Text', '');
  VCL.SetProp.Value(edUserID,    'Text', '');
  VCL.SetProp.Value(Self, 'Caption', 'sesion terminada');
  FSession := nil;
end;

procedure TForm1.OnSessionGroups(const Event: ROuchSessionGroups);
begin
  FGroups := Event.Groups;
  Event.Thread.SyncCall(DoFillGroups, Event.Groups);
  Event.Thread.SyncCall(DoLogReply, Event.Reply);
end;

procedure TForm1.OnSessionJoin(const Event: ROuchSessionJoin);
begin
  Event.Thread.SyncCall(DoSessionJoin, Event);
  Event.Thread.SyncCall(DoLogReply, Event.Reply);
end;

(*)function TForm1.DoLoadUser(const ID: TGUID): IOuchLocalUser;
begin

end;(*)

procedure TForm1.DoLogReply(const Sender: IInterface; const Ref);
begin
  Events.Lines.Add(ROuchReply(Ref).Comment);
end;

procedure TForm1.DoFillGroups(const Sender: IInterface; const Ref);
var
  Enum: IEnumerator;
  Parent, Group: IOuchGroupData;
  Item: TTreeNode;
begin
  Objects.Items.BeginUpdate;
  try
    FGroupsNode.DeleteChildren;
    if Assigned(FGroups) then with FGroups do
      while Enumerate(Enum, Group) do
      begin
        if FGroups.Find(Group.Parent, IOuchGroupData, Parent) then
          Item := Objects.Items.AddChild(Parent.Ptr, Group.Name) else
          Item := Objects.Items.AddChild(FGroupsNode, Group.Name);
        Group.Ptr := Item;
        Item.Data := Sil.Ref.AddRef(Group);
      end;
  finally
    Objects.Items.EndUpdate;
  end;
end;


procedure TForm1.DoSessionJoin(const Sender: IInterface; const Ref);
var
  Data: ROuchSessionJoin absolute Ref;
begin

  DoFillUsers(Data);
  
end;

procedure TForm1.DoFillUsers(const Data: ROuchSessionJoin);
var
  Enum: IEnumerator;
  User: IOuchUserData;
  Item: TTreeNode;
begin
  Objects.Items.BeginUpdate;
  try
    FUsersNode.DeleteChildren;
    with Data.Group.Users do
      while Enumerate(Enum, User) do
      begin
        Item := Objects.Items.AddChild(FUsersNode, User.Nick);
        User.Ptr := Item;
        Item.Data := Sil.Ref.AddRef(User);
      end;
  finally
    Objects.Items.EndUpdate;
  end;
end;

procedure TForm1.ObjectsDeletion(Sender: TObject; Node: TTreeNode);
begin
  if Assigned(Node) then
    Node.Data := Sil.Ref.Release(Node.Data);
end;

procedure TForm1.ObjectsChange(Sender: TObject; Node: TTreeNode);
var
  Group: IOuchGroupData;
  User: IOuchUserData;
begin
  if Sil.Ref.Extract(Node.Data, IOuchGroupData, Group) then
  begin
    edGroupName.Text := Group.Name;
    edGroupID.Text := Sil.GUID.ToStr(Group.ID);
    edGroupParent.Text := Sil.GUID.ToStr(Group.Parent);
  end;
  if Sil.Ref.Extract(Node.Data, IOuchUserData, User) then
  begin
    edOnlineUserName.Text := User.Nick;
    edOnlineUserID.Text := Sil.GUID.ToStr(User.ID);
    edOnlineAddress.Text := User.Address;
    edOnlinePort.Text := Sil.Int.ToStr(User.Port);
  end;
end;

procedure TForm1.ObjectsDblClick(Sender: TObject);
var
  Group: IOuchGroupData;
begin
  if Assigned(Objects.Selected) then
  begin
    if Sil.Ref.Extract(Objects.Selected.Data, IOuchGroupData, Group) then
    begin
      FSession.Join(Group, 'password', nil);
    end;
  end;
end;

procedure TForm1.btInfoClick(Sender: TObject);
begin
  if Assigned(FSession) then
    with TFormUserInfo.Create(nil) do
    try
      lbID.Caption := Sil.GUID.ToStr(FSession.Data.User.ID);
      edNick.Text := FSession.Data.User.Nick;
      edFullName.Text := FSession.Data.User.Mail;
      edMail.Text := FSession.Data.User.Mail;
      edPassword.Text := FSession.Data.User.Password; 
      edConfirm.Text := FSession.Data.User.Password; 
      if (ShowModal() = mrOK) and FSession.Data.User.Edit then
      begin
        FSession.Data.User.Nick := edNick.Text;
        FSession.Data.User.FullName := edFullName.Text;
        FSession.Data.User.Mail := edMail.Text;
        FSession.Data.User.Mail := edMail.Text;
        FSession.Data.User.Apply;
      end;
    finally
      Free;
    end;
end;

procedure TForm1.btChangeStatusClick(Sender: TObject);
var
  S: string;

begin
  S := 'Online';
  if InputQuery('Nuevo estado', 'Estado', S) then
    FSession.Transport.Notify(TOuchUserStatus(Sil.Enum.Value(TypeInfo(TOuchUserStatus), S, 'us')));
end;

procedure TForm1.btSendMessageClick(Sender: TObject);
var
  User: IOuchUserData;
  Recp: IOuchUserList;
  Senders: IOuchUserList;
  Msg: IOuchMessage;
begin
  if Assigned(Objects.Selected) then
  begin
    if Sil.Ref.Extract(Objects.Selected.Data, IOuchUserData, User) then
    begin
      Recp := FEngine.Toolkit.NewUserList();
      Recp.Add(User);

      Senders := FEngine.Toolkit.NewUserList();
      Senders.Add(FLocalUser);
      
      Msg := FEngine.Toolkit.NewMessage(Sil.DateTime.Now, Sil.GUID.Create, Recp, Senders, 'PEDORRO', 1234);
      FSession.Transport.SendMessage(Msg);
    end;
  end;

//
end;

end.

