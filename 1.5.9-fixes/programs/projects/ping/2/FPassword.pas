{********************************************************************************
 *                             SIDERCA/APRE                                     *
 *                     Servidor de Comunicaciones V2                            *
 ********************************************************************************}

unit FPassword;

{$INCLUDE Defines.inc}

interface

uses Windows, SysUtils, Classes, Graphics, Forms, Controls, StdCtrls, 
  Buttons;

type
  TPasswordDlg = class(TForm)
    Label1: TLabel;
    Password: TEdit;
    OKBtn: TButton;
    CancelBtn: TButton;
  private
  public
  end;

implementation

{$R *.DFM}

end.
 
