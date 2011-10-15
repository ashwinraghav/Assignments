unit FmMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs,

  Sil,
  SilStNtlm, StdCtrls;

type
  TForm1 = class(TForm)
    Memo1: TMemo;
    Button1: TButton;
    edPassword: TEdit;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    procedure Test(const Host, Domain, Req: String);
  end;

var
  Form1: TForm1;

implementation

uses SilOjSocket, SilLiList, SilLiStringList;

{$R *.dfm}

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
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

{
GET http://www.yahoo.com/ HTTP/1.0
Accept: image/gif, image/x-xbitmap, image/jpeg, image/pjpeg, application/vnd.ms-excel, application/msword, application/vnd.ms-powerpoint, */*
Accept-Language: en-us
User-Agent: Mozilla/4.0 (compatible; MSIE 6.0; Windows NT 5.0)
Host: www.yahoo.com
Proxy-Connection: Keep-Alive
Proxy-Authorization: NTLM TlRMTVNTUAABAAAABoIAAAAAAAAAAAAAAAAAAAAAAAAAAAAAMAAAAAAAAAAwAAAA
}

procedure TForm1.Test(const Host, Domain, Req: String);
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
  begin
    Result := '';
    
    repeat
      SetLength(Buf, Proxy.Parameters.ReceiveBufferSize);

      if Proxy.WaitFor([ssRead], 3000) then
      begin
        SetLength(Buf, Proxy.Stream.Read(Buf[1], Length(Buf)));
        Result := Result + Buf;
      end else
        Break;

      //if Str.Pos('Connection: close', Result) = 0 then break;
    until not Proxy.IsConnected;

    //HasMore := Str.Pos('Content-Length:', Result) > 0;
  end;

begin
  Proxy := Sil.OS.Socket.CreateClient;
  Proxy.Connect('proxy.siderca.ot', 80);

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

    while true do
    begin
      RcvBuf.Text := DoReceive;
      if RcvBuf.Count = 0 then break;
      memo1.lines.add(RcvBuf.Text);
    end;
  end;
end;

end.

