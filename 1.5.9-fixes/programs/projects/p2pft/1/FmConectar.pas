unit FmConectar;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TfoConectar = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    edAddress: TEdit;
    edPort: TEdit;
    btOk: TButton;
    btCancel: TButton;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  foConectar: TfoConectar;

implementation

{$R *.dfm}

end.
