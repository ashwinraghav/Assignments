unit FmTestFirebirdPassword;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, SilVkCustomControl, SilVkControl, SilVmCustomPanel,
  SilVmGradientPanel, StdCtrls, SilVmCustomButton, SilVmImgButton,
  SilVm3dButton;

type
  TFormTestFirebirdPassword = class(TForm)
    pnBackgroung: TSilGradientPanel;
    pnBottom: TSilGradientPanel;
    edUserName: TEdit;
    edPassword: TEdit;
    edDatabase: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    btOK: TSil3dButton;
    btCancel: TSil3dButton;
  end;

implementation

{$R *.dfm}

end.
