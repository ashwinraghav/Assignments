unit FmUserInfo;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, button, StdCtrls;

type
  TFormUserInfo = class(TForm)
    edNick: TEdit;
    edFullName: TEdit;
    edMail: TEdit;
    Aceptar: TButtonEx;
    Cancelar: TButtonEx;
    lbID: TLabel;
    edPassword: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    edConfirm: TEdit;
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

procedure TFormUserInfo.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  CanClose := (ModalResult <> mrOK) or (edPassword.Text = edConfirm.Text);
  if not CanClose then
    MessageDlg('El password no coincide con la confirmacion', mtError, [mbOK], 0);
end;

end.
