unit FmMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs,

  Sil,
  UiLogan,
  UmLogan, ExtCtrls;

type
  TForm1 = class(TForm)
    Timer: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure TimerTimer(Sender: TObject);
  private
    FLogan: ILogan;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  FLogan := TLogan.Create;
  FLogan.Start;
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if Assigned(FLogan) then
  begin
    FLogan.Stop;
    FLogan := nil;
  end;
end;

procedure TForm1.TimerTimer(Sender: TObject);
begin
  Caption := Int.ToStr(AllocMemSize);
end;

end.
