unit FmEvalDisplay;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TFormDisplay = class(TForm)
    edDisplay: TMemo;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

procedure TFormDisplay.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caHide;
end;

end.
