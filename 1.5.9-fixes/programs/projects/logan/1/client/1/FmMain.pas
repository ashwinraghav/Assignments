unit FmMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls,

  Sil,
  SilLog;

type
  TForm2 = class(TForm)
    Button1: TButton;
    Timer1: TTimer;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

uses SilOjSocket;

{$R *.dfm}

procedure TForm2.FormCreate(Sender: TObject);
begin
  Sil.Trace.LogginManager(SilLog.Logger.SocketClient(24557));
  timer1.Enabled := true;
end;

procedure TForm2.Button1Click(Sender: TObject);
var
  i: Integer;
begin
  i := Str.ToInt(Caption, 0) + 1;
  Caption := Int.ToStr(i);
  Sil.Trace.Enter('Button1Click', [i]);
  Sil.Trace.Log('texto de log');
  Sil.Trace.Leave;
end;

procedure TForm2.Timer1Timer(Sender: TObject);
begin
  Button1Click(nil);
end;

end.
