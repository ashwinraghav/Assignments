unit FmOuchLogon;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, FrOuchButtons, ExtCtrls, StdCtrls,

  Sil, SilVcl, UiOuch, FkOuchBase;

type
  TFormOuchLogon = class(
    TFormOuchBase,
    IOuchLogonData,
    IOuchLogonWindow,
    IOuchEngineConnectedEvent,
    IOuchEngineDisconnectedEvent )
    Button: TFrameOuchButtons;
    pnClientArea: TPanel;
    edNick: TEdit;
    edPassword: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    pnControls: TPanel;
    CheckBox1: TCheckBox;
    procedure CancelClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure AcceptClick(Sender: TObject);
  private
    FLogonOnly: Boolean;
  private
    procedure DoClean;
  protected // IOuchLogonWindow
    function GetData: IOuchLogonData;
    function GetLogonOnly: Boolean;
    procedure SetLogonOnly(Value: Boolean);
  protected // IOuchLogonData
    function GetNick: String;
    function GetPassword: String;
  protected // IOuchEngineConnectedEvent
    procedure OnEngineConnected(const Sender: IOuchEngine);
  protected // IOuchEngineDisconnectedEvent
    procedure OnEngineDisconnected(const Sender: IOuchEngine);
  end;

implementation

{$R *.dfm}

{ TFormOuchLogon }

procedure TFormOuchLogon.FormCreate(Sender: TObject);
begin
  inherited;
  Sil.Sink.Connect(Application.Engine, Self);
  FLogonOnly := True; 
  Button.btAceptar.Default := True;
end;

procedure TFormOuchLogon.FormDestroy(Sender: TObject);
begin
  Sil.Sink.Disconnect(Application.Engine, Self);
  inherited;
end;

procedure TFormOuchLogon.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  inherited;
  try
    if (ModalResult = mrOK) then
    begin
      Application.Engine.Setup(edNick.Text, edPassword.Text, not FLogonOnly);
      Application.Engine.Logon;
    end;
  except on Ex: Exception do
    begin
      DoClean;
      MessageDlg(Ex.Message, mtError, [mbOK], 0);
      CanClose := False;
    end;
  end;
end;

procedure TFormOuchLogon.AcceptClick(Sender: TObject);
begin
  inherited;
  SilVcl.Vcl.Comp.Enable(False, Self);
  Application.Engine.Reset;
end;

procedure TFormOuchLogon.CancelClick(Sender: TObject);
begin
  inherited;
  ModalResult := mrCancel;
  Close;
end;

function TFormOuchLogon.GetData: IOuchLogonData;
begin
  Result := Self;
end;

function TFormOuchLogon.GetLogonOnly: Boolean;
begin
  Result := FLogonOnly;
end;

procedure TFormOuchLogon.SetLogonOnly(Value: Boolean);
begin
  FLogonOnly := Value;
end;

function TFormOuchLogon.GetNick: String;
begin
  Result := edNick.Text;
end;

function TFormOuchLogon.GetPassword: String;
begin
  Result := edPassword.Text;
end;

procedure TFormOuchLogon.DoClean;
begin
  edPassword.Text := '';
  ActiveControl := edNick;
end;

procedure TFormOuchLogon.OnEngineConnected(const Sender: IOuchEngine);
begin
  SilVcl.Vcl.Comp.Enable(True, Self);
  try
    ModalResult := mrOk;
    Close;
  except
    Sil.Trace.Exception('logon');
  end;
end;

procedure TFormOuchLogon.OnEngineDisconnected(const Sender: IOuchEngine);
begin

end;

end.
