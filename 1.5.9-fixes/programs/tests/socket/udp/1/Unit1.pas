unit Unit1;

interface

{$include Defines.inc}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls,
  ExtCtrls,

  Sil;

type
  TForm1 = class(TForm, IUnknown, IRunnable)
    Memo1: TMemo;
    Panel1: TPanel;
    Panel2: TPanel;
    send: TButton;
    Edit1: TEdit;
    Edit2: TEdit;
    close: TButton;
    Edit3: TEdit;
    Button1: TButton;
    cblast: TCheckBox;
    procedure sendClick(Sender: TObject);
    procedure closeClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  protected // IRunnable
    procedure Run(const Thread: IThread);
  private
    procedure DoAddTextLine(const Sender: IUnknown; Ref: Pointer);
  public
    cli: ISocketClient;
    addr: ISocketAddressDef;
    fthread: IThread;
  end;

var
  Form1: TForm1;

implementation

uses SilLiWideStringList, SilLmWideStringList, SilOjSocket, SilOiSocket;

{$R *.DFM}

function buscacontrol(sender: TWinControl; const name: string; out control: TControl): boolean;
var
  i: integer;
begin
  result := false;

  for i := 0 to sender.ControlCount - 1 do
    if sender.Controls[i].Name = name then
    begin
      control := sender.Controls[i];
      result := true;
      break;
    end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  cli := Sil.OS.Socket.CreateClient(stDatagram, spUdp);
  cli.Bind(stDatagram, spUdp, 0, 34502);
  fthread := Sil.OS.Thread.Spawn(Self);
end;

procedure TForm1.DoAddTextLine(const Sender: IUnknown; Ref: Pointer);
begin
  memo1.lines.add(PChar(Ref));
end;

procedure TForm1.Run(const Thread: IThread);
var
  Buf: String;
  Address: ISocketAddress;
  i: Integer;
begin
  while true do
  begin
    SetString(Buf, nil, 1024);
    i := cli.Stream.ReadFrom(Buf[1], Length(Buf), Address);
    SetLength(Buf, i);

    if i > 0 then
    begin
      //if addr <> nil then
        //addr.Address := Address.Address
      //else
        addr := address as ISocketAddressDef;

      Buf := Str.Format('%s:%d > %s', [Address.Host, Address.Port, Buf]);
      Sil.OS.Thread.SyncCall(DoAddTextLine, PChar(Buf));
    end else
      Break;
  end;
  // caption := 'salio!'; // esto cuelga la salida
end;

procedure TForm1.sendClick(Sender: TObject);
var
  Buf: String;
  t: tdatetime;
  s: integer;
  sendaddr: ISocketAddressDef;
begin
  if cblast.checked and (addr <> nil) then
    sendaddr := addr
  else
    sendaddr := Sil.OS.Socket.IP.Create(Edit2.Text, str.toint(Edit3.text));

  Buf := Trim(edit1.text);

  if Length(Buf) > 0 then
  begin
    t := now;
    memo1.lines.add(Buf);
    s := cli.Stream.WriteTo(Buf[1], Length(Buf), sendaddr);
    //edit1.Clear;
    caption := sil.datetime.tostr(now - t, 'hh:nnn:ss.zzz ') + int.tostr(s);
  end;
end;

procedure TForm1.closeClick(Sender: TObject);
begin
  if cli <> nil then cli.Disconnect;
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  closeClick(nil);
  fthread.Termination.WaitFor;
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  s: ISocketServer;
  c: ISocketClient;
begin
  s := Sil.OS.Socket.CreateServer(stStream, spTCP, 0, 12345);
  s.Listen;
  if s.Accept(c) then
  begin
    ShowMessage('entro');
    c.Disconnect;
  end;
end;

end.
