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

unit SilSmPop3Command;

interface

uses
  SilSiAsciiProtocol,
  SilSiPop3Command,
  SilSmAsciiProtocol;

type
  TPop3ClientCommand = class (
    // extends
    TAsciiProtocol,
    // implements
    IPop3ClientCommand)
  protected
    function DoCommand(const CmdStr: String; out Response: String): Boolean;
  protected // IPop3ClientCommand
    function WaitGreetings(out Response: String): Boolean;
    function User(const Name: String; out Response: String): Boolean;
    function Password(const Value: String; out Response: String): Boolean;
    function Quit(out Response: String): Boolean;
    function Status(out Response: String): Boolean;
    function MessageList(out Response: String): Boolean;
    function MessageInfo(const MessageNumber: LongWord; out Response: String): Boolean;
    function Retrieve(const MessageNumber: LongWord; out Response: String): Boolean;
    function Delete(const MessageNumber: LongWord; out Response: String): Boolean;
    function Noop(out Response: String): Boolean;
    function Reset(out Response: String): Boolean;
    function Header(const MessageNumber, Lines: LongWord; out Response: String): Boolean;
    function Uidl(const MessageNumber: LongWord; out Response: String): Boolean;
    function Uidls(out Response: String): Boolean;
    function APop(const Mailbox, Digest: String; out Response: String): Boolean;
  public
    constructor Create;
    destructor Destroy; override;
  end;

implementation

uses
  SilBtText,
  SilBcChr,
  SilBtStr;

const
  AResult: array[Boolean] of String = ('-ERR', '+OK');

{ TPop3ClientCommand }

constructor TPop3ClientCommand.Create;
begin
  inherited Create;
end;

destructor TPop3ClientCommand.Destroy;
begin
  inherited;
end;

function TPop3ClientCommand.DoCommand(const CmdStr: String; out Response: String): Boolean;
var
  sBuf: String;
begin
  if (Str.IsEmpty(CmdStr) or WriteLn(CmdStr)) and ReadLn(sBuf) then
  begin
    Result := Text.Compare(sBuf, AResult[true], Length(AResult[true])) = 0;
    Response := Str.Copy(sBuf, Length(AResult[Result]) + 2);
  end else
    Result := false;
end;

function TPop3ClientCommand.WaitGreetings(out Response: String): Boolean;
begin
  Result := DoCommand('', Response);
end;

function TPop3ClientCommand.APop(const Mailbox, Digest: String; out Response: String): Boolean;
begin
  Result := DoCommand(Str.Format('APOP %s %s', [Mailbox, Digest]), Response);
end;

function TPop3ClientCommand.Delete(const MessageNumber: LongWord; out Response: String): Boolean;
begin
  Result := DoCommand(Str.Format('DELE %d', [MessageNumber]), Response);
end;

// <...>
// .
function TPop3ClientCommand.Header(const MessageNumber, Lines: LongWord; out Response: String): Boolean;
begin
  Result := DoCommand(Str.Format('TOP %d %d', [MessageNumber, Lines]), Response);
end;

// S: +OK 2 200
function TPop3ClientCommand.MessageInfo(const MessageNumber: LongWord; out Response: String): Boolean;
begin
  Result := DoCommand(Str.Format('LIST %d', [MessageNumber]), Response);
end;

// 1 120
// .
function TPop3ClientCommand.MessageList(out Response: String): Boolean;
begin
  Result := DoCommand('LIST', Response);
end;

function TPop3ClientCommand.Noop(out Response: String): Boolean;
begin
  Result := DoCommand('NOOP', Response);
end;

function TPop3ClientCommand.Password(const Value: String; out Response: String): Boolean;
begin
  Result := DoCommand(Str.Format('PASS %s', [Value]), Response);
end;

function TPop3ClientCommand.Quit(out Response: String): Boolean;
begin
  Result := DoCommand('QUIT', Response);
end;

function TPop3ClientCommand.Reset(out Response: String): Boolean;
begin
  Result := DoCommand('RSET', Response);
end;

// <...>
// .
function TPop3ClientCommand.Retrieve(const MessageNumber: LongWord; out Response: String): Boolean;
begin
  Result := DoCommand(Str.Format('RETR %d', [MessageNumber]), Response);
end;

// +OK 2 320
function TPop3ClientCommand.Status(out Response: String): Boolean;
begin
  Result := DoCommand('STAT', Response);
end;

// +OK 2 QhdPYR:00WBw1Ph7x7
function TPop3ClientCommand.Uidl(const MessageNumber: LongWord; out Response: String): Boolean;
begin
  Result := DoCommand(Str.Format('UIDL %d', [MessageNumber]), Response);
end;

// 1 whqtswO00WBw418f9t5JxYwZ
// .
function TPop3ClientCommand.Uidls(out Response: String): Boolean;
begin
  Result := DoCommand('UIDL', Response);
end;

function TPop3ClientCommand.User(const Name: String; out Response: String): Boolean;
begin
  Result := DoCommand(Str.Format('USER %s', [Name]), Response);
end;

end.
 