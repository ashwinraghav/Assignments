unit FmEvalCode;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TFormCode = class(TForm)
    edMatches: TMemo;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

procedure TFormCode.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caHide;
end;

end.
