unit FmMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls,

  Sil,
  SilHttp,
  SilStNtlm;

type
  TForm2 = class(TForm)
    Memo1: TMemo;
    Button1: TButton;
    edPassword: TEdit;
    procedure Button1Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FFiler: IHttpFiler;
    //FRequest: IHttpRequest;
    FResponse: IHttpResponse;
    procedure Test(const Host, Domain, Req: String);
    function DoRead(const Socket: ISocketClient): IHttpResponse;
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

uses SilBtVart, SilLtStream, SilLiStream, SilSiHttpData;

{$R *.dfm}

procedure TForm2.FormCreate(Sender: TObject);
begin
  FFiler := SilHttp.Tk.Filer;
end;

procedure TForm2.FormDestroy(Sender: TObject);
begin
  FFiler := nil;
end;

procedure TForm2.Button1Click(Sender: TObject);
var
  msg: String;
begin
  msg :=
    'GET http://www.yahoo.com/ HTTP/1.0' + #13#10 +
    'Accept: image/gif, image/x-xbitmap, image/jpeg, image/pjpeg, application/vnd.ms-powerpoint, application/vnd.ms-excel, application/msword, application/x-comet, */*' + #13#10 +
    'Accept-Language: en-us' + #13#10 +
    'Host: www.yahoo.com' + #13#10 +
    'Proxy-Connection: Keep-Alive' + #13#10#13#10;

  Test(Sil.OS.Computer.Local.Name, 'SIDERCA', msg);
end;

function TForm2.DoRead(const Socket: ISocketClient): IHttpResponse;
var
  Text: string;
  Size: LongWord;
  Buffer: IMemoryStream;
begin
  Buffer := Sil.MemoryStream.Create();
  repeat
    Socket.WaitFor([ssRead]);
    SetLength(Text, Socket.Stream.Size);
    Size := Socket.Stream.Read(Text[1], Length(Text));
    if Size > 0 then
      Buffer.Write(Text[1], Size);
  until Size = 0;
  Buffer.Position := 0;
  FFiler.Read(Buffer, Result);
end;

procedure TForm2.Test(const Host, Domain, Req: String);
var
  i, Pos: Integer;
  Msg, Msg1, Msg2, Msg3: String;
  Proxy: ISocketClient;
  SndBuf, RcvBuf: IStringList;

  procedure DoSend(const Buf: String);
  begin
    Proxy.Stream.Write(Buf[1], Length(Buf));
  end;

  function DoReceive: String;
  var
    Buf: String;
    Remain, Size, ContentLength: LongWord;
    Stream: IMemoryStream;
  begin
    Result := '';
    Stream := Sil.MemoryStream.Create;

    Proxy.WaitFor([ssRead]);
    FFiler.Read(Proxy.Stream, FResponse);
    ContentLength := Vart.ToInt(FResponse.Entity.Tags.List['Content-Length'].Value, 0);

    if ContentLength = 0 then
      ContentLength := Proxy.Parameters.ReceiveBufferSize;

    SetLength(Buf, ContentLength);
    FFiler.Write(Stream, FResponse);

    with FResponse.Entity.Stream do
      if Size > 0 then
        Stream.Write(Memory^, Size);

    if Stream.Size > 0 then
      Result := Stream.Memory;
  end;

begin
  Proxy := Sil.OS.Socket.CreateClient;
  Proxy.Connect('proxy.siderca.ot', 80);
  Proxy.Parameters.ReadTimeout := 30000;

  SndBuf := Sil.List.StringList;
  RcvBuf := Sil.List.StringList;

  Msg1 := Ntlm.Message1(Host, Domain);

  SndBuf.Text := Req;
  i := SndBuf.Count;
  SndBuf.Insert(i - 1, 'Proxy-Authorization: NTLM ' + Msg1);

  Msg := SndBuf.Text;
  memo1.lines.add('---- envio msg1');
  memo1.lines.add(Msg);
  DoSend(Msg);

  RcvBuf.Text := DoReceive;
  memo1.lines.add('---- respuesta msg1');
  memo1.lines.add(RcvBuf.Text);

  if RcvBuf.Count > 0 then
  begin
    i := RcvBuf.IndexOfMask('*NTLM *');
    Pos := Str.Pos('NTLM ', RcvBuf[i]);
    Msg2 := Ntlm.Message2(Str.Copy(RcvBuf[i], Pos + 5));
    Msg3 := Ntlm.Message3(Domain, Host, 't58738', edPassword.Text, Msg2);

    i := SndBuf.Count;
    SndBuf[i - 2] := 'Proxy-Authorization: NTLM ' + Msg3;

    Msg := SndBuf.Text;
    memo1.lines.add('---- envio msg3');
    memo1.lines.add(Msg);
    DoSend(Msg);

    memo1.lines.add('---- respuesta msg3');

    //while true do
    //begin
      RcvBuf.Text := DoReceive;
      //if RcvBuf.Count = 0 then break;
      memo1.lines.add(RcvBuf.Text);
    //end;
  end;
end;

end.
