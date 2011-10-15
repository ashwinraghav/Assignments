unit FrOuchButtons;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, 
  Dialogs, ExtCtrls, SilVkCustomControl, SilVkControl,
  SilVmCustomButton, SilVmButton;

type
  TFrameOuchButtons = class(TFrame)
    btCerrar: TSilButton;
    btAceptar: TSilButton;
    pnButtons: TPanel;
    Bevel1: TBevel;
    Bevel2: TBevel;
  end;

implementation

{$R *.dfm}

end.
