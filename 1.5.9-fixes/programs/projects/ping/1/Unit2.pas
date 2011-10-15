unit Unit2;

interface

uses
  Windows, WinSock, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls,

  Sil, UiIcmp, UiIcmpEcho, UmIcmpEcho;

type
  TForm2 = class(TForm)
    Button1: TButton;
    Edit1: TEdit;
    Display: TMemo;
    Timer: TTimer;
    Button2: TButton;
    CheckBox1: TCheckBox;
    Button3: TButton;
    procedure Button1Click(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
  private
    FCounter: IPerformanceCounter;
    FPing: IIcmpEcho;
  end;

var
  Form2: TForm2;

implementation

uses
  UmIcmp;

var
  Seq: Integer = 0;

{$R *.dfm}

{ TForm2 }

procedure TForm2.FormCreate(Sender: TObject);
begin
  FCounter := Sil.Os.Performance.Create(); 
  FPing := TSilIcmpEcho.Create;
end;

procedure TForm2.FormDestroy(Sender: TObject);
begin
  FPing := nil;
  FCounter := nil;
end;

procedure TForm2.Button1Click(Sender: TObject);
begin
  TimerTimer(nil);
end;

procedure TForm2.Button2Click(Sender: TObject);
begin
  Timer.Enabled := not Timer.Enabled;
  button2.caption := Str.IIf(Timer.Enabled, 'Enabled', 'Disabled');
end;

procedure TForm2.TimerTimer(Sender: TObject);
var
  Socket: ISocketClient;
  Dest: ISocketAddress;
  Query, Reply: REcho;
  Line, Addr: String;
  Elapsed: Single;
begin
  Socket := Sil.Os.Socket.CreateClient(stRaw, spICMP);
  Socket.Parameters.ReadTimeout := 2000;

  Dest := Sil.Os.Socket.IP.Create(Edit1.Text, 0);

  Query.Id := GetCurrentProcessId();
  Query.Sequence := Sil.Os.Locked.Increment(Seq);
  Query.Data := Sil.Str.Replicate('PING!', 4);


  FCounter.Reset();    
  Reply := FPing.Ping(Socket, Dest, Query);
  Elapsed := FCounter.ToMilliseconds(); 

  if Assigned(Dest) then
  begin
    if CheckBox1.Checked then
      Addr := Dest.Host else
      Addr := Dest.Format;

    Line := Format('%d bytes from %s: sequence = %d, time %6.4f ms', [Length(Reply.Data), Addr, Reply.Sequence, Elapsed])
  end else
    Line := 'timeout';

  Display.Lines.Add(Line);
end;

procedure TForm2.Button3Click(Sender: TObject);
var
  Lap: Integer;
begin
  Lap := FPing.Ping(Edit1.Text);
  Display.Lines.Add(Str.Format('Host %s Lap %d -> %s', [Edit1.Text, Lap, Str.IIf(Lap >= 0, 'OK', 'TIMEOUT')]));
end;

end.
