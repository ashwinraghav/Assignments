unit FmOuchHistory;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, SilVkCustomControl,
  SilVkControl, SilVmCustomButton, SilVmButton,

  Sil, UiOuch, FkOuchBase, FrOuchHistory;

type
  TFormOuchHistory = class(
    TFormOuchBase,
    IOuchHistoryWindow )
    pnButtons: TPanel;
    pnHeading: TPanel;
    Label1: TLabel;
    edAccount: TEdit;
    pnClientArea: TPanel;
    btClose: TSilButton;
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btCloseClick(Sender: TObject);
  private
    FFrame: TFrameOuchHistory;
  private
    procedure DoClearAccount;
    procedure DoShowAccount;
  protected // IOuchHistoryWindow
    function GetAccount: IOuchAccount;
    procedure SetAccount(const Value: IOuchAccount);
  end;

implementation

{$R *.dfm}

{ TFormOuchHistory }

procedure TFormOuchHistory.FormCreate(Sender: TObject);
begin
  inherited;
  FFrame := TFrameOuchHistory.Create(Owner, pnClientArea, nil);
end;

procedure TFormOuchHistory.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FFrame);
  inherited;
end;

function TFormOuchHistory.GetAccount: IOuchAccount;
begin
  Result := FFrame.Account;
end;

procedure TFormOuchHistory.SetAccount(const Value: IOuchAccount);
begin
  DoClearAccount;
  FFrame.Account := Value;
  DoShowAccount;
end;

procedure TFormOuchHistory.DoClearAccount;
begin
  if Assigned(FFrame.Account) then
  begin
    edAccount.Text := '';
  end;
end;

procedure TFormOuchHistory.DoShowAccount;
begin
  if Assigned(FFrame.Account) then
  begin
    edAccount.Text := FFrame.Account.Nick;
  end;
end;

procedure TFormOuchHistory.btCloseClick(Sender: TObject);
begin
  Close;
end;

end.
