unit FmOuchMessage;

interface

uses
  Windows, Classes, Controls, Forms, StdCtrls, ExtCtrls, SilVkCustomControl, SilVkControl,
  SilVmCustomButton, SilVmButton,

  Sil, SilVcl, UiOuch, FkOuchBase;

type
  TMessageMode = (
      mmUnknown,
      mmReception,
      mmCompose
    );
   
  TFormOuchMessage = class(
    TFormOuchBase,
    IOuchView,
    IOuchMessageView,
    IOuchContactsEvents )
    pnMemo: TPanel;
    edMessage: TMemo;
    pnButtons: TPanel;
    pnRecvHeader: TPanel;
    edFrom: TEdit;
    lbFrom: TLabel;
    btClose: TSilButton;
    lbDate: TLabel;
    edDate: TEdit;
    lbTime: TLabel;
    edTime: TEdit;
    pnSendHeader: TPanel;
    lbTo: TLabel;
    edTo: TEdit;
    btRecvReply: TSilButton;
    btSendEnviar: TSilButton;
    ckSendBlindCopy: TCheckBox;
    procedure btCloseClick(Sender: TObject);
    procedure btRecvReplyClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure btSendEnviarClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FMessage: IOuchMessage;
    FMode: TMessageMode;
  private
    procedure DoSetMode(const Value: TMessageMode);
    procedure DoSetMsg(const Value: IOuchMessage);
    procedure DoSetReception(const Msg: IOuchMessage);
    procedure DoSetCompose(const Msg: IOuchMessage);
    procedure DoUpdateRecipients;
    procedure DoUpdateMessage;
    function DoUsersToStr(const Users: IOuchAccounts): string;
  protected // IOuchView
    procedure BeginUpdate;
    procedure EndUpdate;
  protected // IOuchContactsEvents
    procedure OnUserAdd(const User: IOuchAccount);
    procedure OnUserRemove(const User: IOuchAccount);
    procedure OnUserChanged(const User: IOuchAccount);
  protected // IOuchMessageView
    procedure Show(const Msg: IOuchMessage);
    procedure Compose(const Msg: IOuchMessage);
  public
    property Msg: IOuchMessage read FMessage write DoSetMsg;
    property Mode: TMessageMode read FMode write DoSetMode;
  end;

implementation

uses
  Dialogs,

  UdOuch, SilLtList;

{$R *.dfm}

{ TFormOuchMessage }

procedure TFormOuchMessage.FormCreate(Sender: TObject);
begin
  inherited;
  FMessage := Application.Engine.Factory.Message();
end;

procedure TFormOuchMessage.FormDestroy(Sender: TObject);
begin
  FMessage := nil;
  inherited;
end;

procedure TFormOuchMessage.BeginUpdate;
begin
end;

procedure TFormOuchMessage.EndUpdate;
begin
end;

procedure TFormOuchMessage.btCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TFormOuchMessage.btRecvReplyClick(Sender: TObject);
var
  NewMsg: IOuchMessage;
begin
  if Mode = mmReception then
  begin
    NewMsg := Application.Engine.Factory.Message();
    NewMsg.From := Application.Engine.Account; 
    NewMsg.Recipients.Add(Msg.From);
    NewMsg.ReplyTo := Msg;
    Compose(NewMsg);
  end;
end;

procedure TFormOuchMessage.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if (ModalResult = mrOK) and (Mode = mmCompose) and Sil.Str.IsEmpty(edMessage.Text) then
    CanClose := MessageDlg(SAskSendEmptyMessage, mtConfirmation, [mbYes, mbNo], 0) = idYES;
end;

procedure TFormOuchMessage.btSendEnviarClick(Sender: TObject);
begin
  if Sil.Str.IsEmpty(edMessage.Text) then
    if MessageDlg(SAskSendEmptyMessage, mtConfirmation, [mbYes, mbNo], 0) <> IDYES then
      Exit;

  Msg.Time := Sil.Date.Now();
  Msg.Text := edMessage.Text;
  Msg.BlindCopy := ckSendBlindCopy.Checked;

  Application.Engine.Send(Msg);

  Close;
end;

procedure TFormOuchMessage.Show(const Msg: IOuchMessage);
begin
  Self.Mode := mmReception;
  Self.Msg := Msg;
  Self.Visible := True;
end;

procedure TFormOuchMessage.Compose(const Msg: IOuchMessage);
begin
  Self.Mode := mmCompose;
  Self.Msg := Msg;
  Self.Visible := True;
end;

procedure TFormOuchMessage.DoSetMsg(const Value: IOuchMessage);
begin
  if Assigned(FMessage) then Sil.Sink.Disconnect(FMessage, Self);
  FMessage := Value;
  if Assigned(FMessage) then
  begin
    Sil.Sink.Connect(FMessage, Self);
    if FMode = mmReception then
      DoSetReception(FMessage) else
      DoSetCompose(FMessage);
  end;
end;

procedure TFormOuchMessage.DoSetMode(const Value: TMessageMode);
begin
  if Value <> FMode then
  begin
    FMode := Value;

    btRecvReply.Visible := (Value = mmReception);
    btRecvReply.Default := (Value = mmReception);
    pnRecvHeader.Visible := (Value = mmReception);
    pnSendHeader.Visible := (Value = mmCompose);
    btSendEnviar.Visible := (Value = mmCompose);
    btSendEnviar.Default := (Value = mmCompose);
    ckSendBlindCopy.Visible := (Value = mmCompose);
    edMessage.ReadOnly := (Value <> mmCompose);

  end;
end;

procedure TFormOuchMessage.DoSetReception(const Msg: IOuchMessage);
begin
  edDate.Text := Sil.DateTime.ToStr(Msg.Time, 'ddd dd/mmm/yyyy');
  edTime.Text := Sil.DateTime.ToStr(Msg.Time, 'HH:nn:ss');
  edFrom.Text := Sil.Str.Format('%s', [Msg.From.Nick]);
  edMessage.Lines.Text := Msg.Text;
end;

procedure TFormOuchMessage.DoSetCompose(const Msg: IOuchMessage);
begin
  DoUpdateMessage;
  DoUpdateRecipients;
end;

procedure TFormOuchMessage.OnUserAdd(const User: IOuchAccount);
begin
  DoUpdateRecipients;
end;

procedure TFormOuchMessage.OnUserChanged(const User: IOuchAccount);
begin
end;

procedure TFormOuchMessage.OnUserRemove(const User: IOuchAccount);
begin
  DoUpdateRecipients;
end;

procedure TFormOuchMessage.DoUpdateRecipients;
begin
  edTo.Text := DoUsersToStr(FMessage.Recipients);
end;

procedure TFormOuchMessage.DoUpdateMessage;
var
  Msg: IOuchMessage;
  Lines: IStringList;
  Enum: IEnumerator;
  Line: string;
begin
  edMessage.Clear;

  if Assigned(FMessage.ReplyTo) then
  begin
    Msg := FMessage.ReplyTo;

    Lines := Sil.List.StringList(Msg.Text, sLineBreak);

    edMessage.Lines.Add('');
    edMessage.Lines.Add(Msg.From.Nick + ' escribió:');

    while Lines.Enumerate(Enum, Line) do
      edMessage.Lines.Add('> ' + Line);
  end;

  edMessage.SelectAll;
end;

function TFormOuchMessage.DoUsersToStr(const Users: IOuchAccounts): string;
var
  Enum: IEnumerator;
  Item: IOuchAccount;
begin
  Result := '';
  while Users.Enumerate(Enum, Item) do
    Sil.Str.Add(Result, Item.Nick, '; ');
end;

end.

