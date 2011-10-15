{********************************************************************************
 *                  Standard Interface Library (SIL)                            *
 *                                                                              *
 *       General purpose library whose design is based in STRONG                *
 *   use of interfaces.                                                         *
 *                                                                              *
 *                                                                              *
 *     Copyright (C) 2000 Mariano Podestá    antiriad@gmail.com                 *
 *     Copyright (C) 2000 Leandro Conde      lconde@str.com.ar                  *
 *     Copyright (C) 2000 Lisandro Podestá   lisandrop@movi.com.ar              *
 *                                                                              *
 *     See License.txt for details.                                             *
 *                                                                              *
 *   This library is free software; you can redistribute it and/or              *
 *   modify it under the terms of the GNU Lesser General Public                 *
 *   License as published by the Free Software Foundation; either               *
 *   version 2.1 of the License, or (at your option) any later version.         *
 *                                                                              *
 *   This library is distributed in the hope that it will be useful,            *
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of             *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU          *
 *   Lesser General Public License for more details.                            *
 *                                                                              *
 *   You should have received a copy of the GNU Lesser General Public           *
 *   License along with this library; if not, write to the Free Software        *
 *   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA  *
 *                                                                              *
 ********************************************************************************}

unit SilSmSmtpCommand;


interface

uses
  SilBeTypes,
  SilLkInterfaced,
  SilSiAsciiProtocol,
  SilSiSmtpCommand;

type
  TSmtpClientCommand = class (
    // extends
    TSilInterfacedObject,
    // implements
    ISmtpClientCommand)
  private
    FLast: Word;
    FCommand: IAsciiProtocol;
  protected
    function DoCommand(const CmdStr: String; const Resp: array of Word; out Response: String): Boolean;
  protected // IConnectable
    procedure AddListener(const Listener: IUnknown; KeepRef: Boolean); override;
    procedure RemoveListener(const Listener: IUnknown); override;
  protected // IAsciiProtocol
    function WriteLn(const Command: String): Boolean;
    function ReadLn(out Response: String): Boolean;
  protected // ISmtpClientCommand
    function WaitGreetings(out Response: String): Boolean;
    function Hello(const Domain: String; out Response: String): Boolean;
    function Verify(const UserName: String; out Response: String): Boolean;
    function Quit(out Response: String): Boolean;
    function MailFrom(const Account: String; out Response: String): Boolean;
    function SendFrom(const Account: String; out Response: String): Boolean;
    function SendOrMailFrom(const Account: String; out Response: String): Boolean;
    function SendAndMailFrom(const Account: String; out Response: String): Boolean;
    function Recipient(const Account: String; out Response: String): Boolean;
    function Data(out Response: String): Boolean;
    function WaitDelivery(out Response: String): Boolean;
    function Reset(out Response: String): Boolean;
    function Turn(out Response: String): Boolean;
    function Expand(const Account: String; out Response: String): Boolean;
    function Help(out Response: String): Boolean; overload;
    function Help(const Command: String; out Response: String): Boolean; overload;
    function Noop(out Response: String): Boolean;
  public
    constructor Create(const Command: IAsciiProtocol);
    destructor Destroy; override;
  end;

implementation

uses
  SilBtStr,
  SilLtConnection;

{ TSmtpClientCommand }

constructor TSmtpClientCommand.Create(const Command: IAsciiProtocol);
begin
  inherited Create;
  FCommand := Command;
end;

destructor TSmtpClientCommand.Destroy;
begin
  FCommand := nil;
  inherited;
end;

procedure TSmtpClientCommand.AddListener(const Listener: IInterface; KeepRef: Boolean);
begin
  Sink.Connect(FCommand, Listener, KeepRef);
end;

procedure TSmtpClientCommand.RemoveListener(const Listener: IInterface);
begin
  Sink.Disconnect(FCommand, Listener);
end;

function TSmtpClientCommand.DoCommand(const CmdStr: String; const Resp: array of Word; out Response: String): Boolean;
var
  i: Integer;
begin
  if (Str.IsEmpty(CmdStr) or WriteLn(CmdStr)) and ReadLn(Response) then
  begin
    FLast := Str.ToInt(Str.Copy(Response, 1, 3));

    for i := 0 to Length(Resp) - 1 do
    begin
      Result := FLast = Resp[i];
      if Result then Exit;
    end;
  end;

  Result := false;
end;

function TSmtpClientCommand.ReadLn(out Response: String): Boolean;
begin
  Result := FCommand.ReadLn(Response);
end;

function TSmtpClientCommand.WriteLn(const Command: String): Boolean;
begin
  Result := FCommand.WriteLn(Command);
end;

function TSmtpClientCommand.Data(out Response: String): Boolean;
begin
  Result := DoCommand('DATA', [354], Response);
end;

function TSmtpClientCommand.Expand(const Account: String; out Response: String): Boolean;
begin
  Result := DoCommand('EXPN ' + Account, [250], Response);
end;

function TSmtpClientCommand.Hello(const Domain: String; out Response: String): Boolean;
begin
  Result := DoCommand('HELO' + Str.IIf(Str.IsEmpty(Domain), '', ' ' + Domain), [250], Response);
end;

function TSmtpClientCommand.Help(out Response: String): Boolean;
begin
  Result := DoCommand('HELP', [211], Response);
end;

function TSmtpClientCommand.Help(const Command: String; out Response: String): Boolean;
begin
  Result := DoCommand('HELP ' + Command, [211], Response);
end;

function TSmtpClientCommand.MailFrom(const Account: String; out Response: String): Boolean;
begin
  Result := DoCommand('MAIL FROM:' + Account, [250], Response);
end;

function TSmtpClientCommand.Noop(out Response: String): Boolean;
begin
  Result := DoCommand('NOOP', [250], Response);
end;

function TSmtpClientCommand.WaitDelivery(out Response: String): Boolean;
begin
  Result := DoCommand('', [250], Response);
end;

function TSmtpClientCommand.Quit(out Response: String): Boolean;
begin
  Result := DoCommand('QUIT', [221], Response);
end;

function TSmtpClientCommand.Recipient(const Account: String; out Response: String): Boolean;
begin
  Result := DoCommand('RCPT TO:' + Account, [250, 251], Response);
end;

function TSmtpClientCommand.Reset(out Response: String): Boolean;
begin
  Result := DoCommand('RSET', [250], Response);
end;

function TSmtpClientCommand.SendAndMailFrom(const Account: String; out Response: String): Boolean;
begin
  Result := DoCommand('SAML FROM:' + Account, [250], Response);
end;

function TSmtpClientCommand.SendFrom(const Account: String; out Response: String): Boolean;
begin
  Result := DoCommand('SEND FROM:' + Account, [250], Response);
end;

function TSmtpClientCommand.SendOrMailFrom(const Account: String; out Response: String): Boolean;
begin
  Result := DoCommand('SOML FROM:' + Account, [250], Response);
end;

function TSmtpClientCommand.Turn(out Response: String): Boolean;
begin
  Result := DoCommand('TURN', [250], Response);
end;

function TSmtpClientCommand.Verify(const UserName: String; out Response: String): Boolean;
begin
  Result := DoCommand('VRFY ' + UserName, [250, 251], Response);
end;

function TSmtpClientCommand.WaitGreetings(out Response: String): Boolean;
begin
  Result := DoCommand('', [220, 250], Response);
end;

end.
 