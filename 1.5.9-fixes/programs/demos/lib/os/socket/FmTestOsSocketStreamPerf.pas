unit FmTestOsSocketStreamPerf;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls,

  Sil;

type
  TForm1 = class(TForm, IRunnable)
    send: TButton;
    msecs: TLabel;
    Edit1: TEdit;
    Label2: TLabel;
    Label3: TLabel;
    Edit2: TEdit;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Label4: TLabel;
    Edit3: TEdit;
    edTime: TEdit;
    Memo1: TMemo;
    procedure Button1Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure sendClick(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FServer: ISocketServer;
    FClient: ISocketClient;
    FServerPeer: ISocketClient;
    FThread: IThread;
    procedure DoDestroy; reintroduce;
  protected
    procedure Run(const Sender: IThread);
  public
    { Public declarations }
  end;

var
  Form1: TForm1;
  f: ITextFile;

implementation

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
begin
  FThread := Sil.OS.Thread.Spawn(Self);
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  FClient := Sil.OS.Socket.CreateClient(stStream, spTCP);
  FClient.Parameters.ReadTimeout := 100;
  FClient.Connect(edit1.text, Str.ToInt(edit2.text));
end;

procedure TForm1.Run(const Sender: IThread);
var
  Buffer: String;
  Size: Integer;
begin
  FServer := Sil.OS.Socket.CreateServer(stStream, spTCP, 0, Str.ToInt(edit2.text));
  FServer.Listen;

  if FServer.Accept(FServerPeer) then
  begin
    SetLength(Buffer, Str.ToInt(Edit3.Text, 1024));

    while true do
    begin
      Size := FServerPeer.Stream.Read(Buffer[1], Length(Buffer));
      if Size < 1 then Break;

      FServerPeer.Stream.Write(Buffer[1], Size);
    end;
  end;
end;

procedure TForm1.sendClick(Sender: TObject);
var
  Buffer: String;
  Perf: IPerformanceCounter;
begin
  if Assigned(FClient) then
  begin
    SetLength(Buffer, Str.ToInt(Edit3.Text, 1024));
    Perf := Sil.OS.Performance.Create;

    if FClient.Stream.Write(Buffer[1], Length(Buffer)) > 0 then
    begin
      FClient.Stream.Read(Buffer[1], Length(Buffer));
      edTime.Text := Float.ToStr(Perf.ToMilliseconds);
    end else
      Memo1.lines.add('falso write! ' + Float.ToStr(Perf.ToMilliseconds));
  end;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  DoDestroy;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  DoDestroy;
end;

procedure TForm1.DoDestroy;
begin
  if Assigned(FServer) then FServer.Cancel;
  if Assigned(FServerPeer) then FServerPeer.Disconnect;
  if Assigned(FClient) then FClient.Disconnect;
  if Assigned(FThread) then FThread.Termination.WaitFor(INFINITE);

  FServer := nil;
  FServerPeer := nil;
  FClient := nil;
  FThread := nil;
end;

end.
