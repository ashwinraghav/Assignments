unit SilStNtlm;

interface

{$include Defines.inc}

uses
  Sil;

type
  Ntlm = class (Tool)
    class function Message1(const Host, Domain: String): String;
    class function Message2(const ServerReply: String): String;
    class function Message3(const Domain, Host, User, Password: String; const Nonce: String): String;
  end;

implementation

uses
  SilCoder,
  SilSeNtlm,
  SilSfNtlm;

{ Ntlm }

class function Ntlm.Message1(const Host, Domain: String): String;
var
  Msg: TNtlmMessage1;
  Buffer: String;
begin
  FillChar(Msg, SizeOf(Msg), #0);

  with Msg do
  begin
    Move('NTLMSSP'#0, Protocol, 8);
    MsgType := #1;
    Flags := 45571;
    LenHost1 := Length(Host);
    LenHost2 := LenHost1;
    OffSetHost := $20;
    LenDomain1 := Length(Domain);
    LenDomain2 := LenDomain1;
    OffSetDomain := OffSetHost + LenHost1;
  end;

  SetLength(Buffer, SizeOf(Msg));
  Move(Msg, Buffer[1], SizeOf(Msg));
  Result := SilCoder.Mime.Base64Coder.EncodeLine(Buffer + Host + Domain);
end;

class function Ntlm.Message2(const ServerReply: String): String;
var
  NTLMReply: String;
begin
  NTLMReply := SilCoder.Mime.Base64Coder.DecodeLine(ServerReply);
  SetLength(Result, 8);
  Move(PNtlmMessage2(@NTLMReply[1]).Nonce, Result[1], Length(Result));
end;

class function Ntlm.Message3(const Domain, Host, User, Password: String; const Nonce: String): String;
var
  Msg: TNtlmMessage3;
  FmtDomain, FmtHost, FmtUser, Buffer: String;
  LM_Resp: String[30];
  NT_Resp: String[30];
  ANonce: TByteArray;
begin
  FillChar(Msg, SizeOf(Msg), #0);

  FmtDomain := UnicodeString(Domain, true);
  FmtHost := UnicodeString(Host, true);
  FmtUser := UnicodeString(User, false);

  with Msg do
  begin
    Move('NTLMSSP'#0, Protocol, 8);
    MsgType := #3;
    LenDomain1 := Length(FmtDomain);
    LenDomain2 := LenDomain1;
    OffSetDomain := $40;
    LenUser1 := Length(FmtUser);
    LenUser2 := LenUser1;
    OffSetUser := OffSetDomain + LenDomain1;
    LenHost1 := Length(FmtHost);
    LenHost2 := LenHost1;
    OffSetHost := OffsetUser + LenUser1;
    Len_LM_Resp1 := $18;
    Len_LM_Resp2 := Len_LM_Resp1;
    OffSet_LM_Resp := OffsetHost + LenHost1;
    Len_NT_Resp1 := $18;
    Len_NT_Resp2 := Len_NT_Resp1;
    OffSet_NT_Resp := OffSet_LM_Resp + Len_LM_Resp1;
    LenMessage := Offset_NT_Resp + Len_NT_Resp1;
    Flags := 33281;
  end;

  SetLength(ANonce, Length(Nonce));
  Move(Nonce[1], ANonce[0], Length(Nonce));

  LM_Resp := GetLMHash(Password, ANonce);
  NT_Resp := GetNTHash(Password, ANonce);

  SetLength(Buffer, SizeOf(Msg));
  Move(Msg, Buffer[1], SizeOf(Msg));
  Result := SilCoder.Mime.Base64Coder.EncodeLine(Buffer + FmtDomain + FmtUser + FmtHost + LM_Resp + NT_Resp);
end;

end.
