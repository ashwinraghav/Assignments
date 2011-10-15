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

unit SilSmFtpCommand;

interface

uses
  SilSiAsciiProtocol,
  SilSiFtpCommand,
  SilSmAsciiProtocol;

type
  TFtpClientCommand = class (
    // extends
    TAsciiProtocol,
    // implements
    IFtpClientCommand)
  protected
    function DoCommand(const CmdStr: String; out Response: String): Word;
  protected // IFtpClientCommand
    function IFtpClientCommand.ReadLn = DoReadLn;
    function DoReadLn(out Response: String): Word;
    function Passive(out Response: String): Word;
    function AccountInfo(const Text: String; out Response: String): Word;
    function AbortTransfer(out Response: String): Word;
    function Allocate(Size, Pages: LongWord; out Response: String): Word;
    function Restart(Marker: LongWord; out Response: String): Word;
    function Append(const Path: String; out Response: String): Word;
    function ChangeDir(const Path: String; out Response: String): Word;
    function ChangeDirToParent(out Response: String): Word;
    function CurrentDir(out Response: String): Word;
    function DeleteFile(const Path: String; out Response: String): Word;
    function Help(const Command: String; out Response: String): Word;
    function List(const Path: String; out Response: String): Word;
		function User(const UserName: String; out Response: String): Word;
		function Password(const Password: String; out Response: String): Word;
    function MakeDir(const Path: String; out Response: String): Word;
    function NameList(const Path: String; out Response: String): Word;
    function Noop(out Response: String): Word;
    function Quit(out Response: String): Word;
    function Reinitialize(out Response: String): Word;
    function RemoveDir(const Path: String; out Response: String): Word;
    function RenameFileFrom(const OldPath: String; out Response: String): Word;
    function RenameFileTo(const NewPath: String; out Response: String): Word;
    function TypeCode(Code: TFtpTypeCode; out Response: String): Word;
    function Retrieve(const Path: String; out Response: String): Word;
    function SiteParameters(out Response: String): Word;
    function Status(const Path: String; out Response: String): Word;
    function Store(const Path: String; out Response: String): Word;
    function Structure(Code: TFtpStructCode; out Response: String): Word;
    function System(out Response: String): Word;
    function TransferMode(Mode: TFtpTransferMode; out Response: String): Word;
    function Port(const Address: String; Port: Word; out Response: String): Word;
    function FileSize(const Path: String; out Response: String): Word;
    function Mount(const Path: String; out Response: String): Word;
  public
    constructor Create;
    destructor Destroy; override;
  end;

implementation

uses
  SilBcChr,
  SilBtInt,
  SilBtStr;

const
  ACode: array [TFtpTypeCode] of Char = ('A', 'E', 'I', 'L');
  AStructCode: array [TFtpStructCode] of Char = ('F', 'R', 'P');
  ATransferMode: array [TFtpTransferMode] of Char = ('S', 'B', 'C');

{ TFtpClientCommand }

constructor TFtpClientCommand.Create;
begin
  inherited Create;
end;

destructor TFtpClientCommand.Destroy;
begin
  inherited;
end;

function TFtpClientCommand.DoCommand(const CmdStr: String; out Response: String): Word;
var
  sBuf, sMulti: String;
begin
  if (Str.IsEmpty(CmdStr) or WriteLn(CmdStr)) and ReadLn(sBuf) then
  begin
    sMulti := Str.Left(sBuf, 3);
    Result := Str.ToInt(sMulti, 0);
    Response := Str.Copy(sBuf, 5);

    while (Str.Compare(sBuf, sMulti + '-', 4) = 0) and ReadLn(sBuf) do
      Response := Response + ccCRLF + Str.Copy(sBuf, 5);
  end else
    Result := 0;
end;

function TFtpClientCommand.DoReadLn(out Response: String): Word;
begin
  Result := DoCommand('', Response);
end;

function TFtpClientCommand.AccountInfo(const Text: String; out Response: String): Word;
begin
  Result := DoCommand('ACCT ' + Text, Response);
end;

function TFtpClientCommand.AbortTransfer(out Response: String): Word;
begin
  Result := DoCommand('ABOR', Response);
end;

function TFtpClientCommand.Allocate(Size, Pages: LongWord; out Response: String): Word;
begin
  Result := DoCommand('ALLO ' + Int.ToStr(Size) + Str.IIf(Pages > 0, ' R ' + Int.ToStr(Pages)), Response);
end;

function TFtpClientCommand.Append(const Path: String; out Response: String): Word;
begin
  Result := DoCommand('APPE ' + Path, Response);
end;

function TFtpClientCommand.ChangeDir(const Path: String; out Response: String): Word;
begin
  Result := DoCommand('CWD ' + Path, Response);
end;

function TFtpClientCommand.ChangeDirToParent(out Response: String): Word;
begin
  Result := DoCommand('CDUP', Response);
end;

function TFtpClientCommand.CurrentDir(out Response: String): Word;
begin
  Result := DoCommand('PWD', Response);
end;

function TFtpClientCommand.DeleteFile(const Path: String; out Response: String): Word;
begin
  Result := DoCommand('DELE ' + Path, Response);
end;

function TFtpClientCommand.FileSize(const Path: String; out Response: String): Word;
begin
  Result := DoCommand('SIZE ' + Path, Response);
end;

function TFtpClientCommand.Help(const Command: String; out Response: String): Word;
begin
  Result := DoCommand('HELP ' + Command, Response);
end;

function TFtpClientCommand.List(const Path: String; out Response: String): Word;
begin
  Result := DoCommand('LIST ' + Path, Response);
end;

function TFtpClientCommand.MakeDir(const Path: String; out Response: String): Word;
begin
  Result := DoCommand('MKD ' + Path, Response);
end;

function TFtpClientCommand.NameList(const Path: String; out Response: String): Word;
begin
  Result := DoCommand('NLST ' + Path, Response);
end;

function TFtpClientCommand.Noop(out Response: String): Word;
begin
  Result := DoCommand('NOOP', Response);
end;

function TFtpClientCommand.Passive(out Response: String): Word;
begin
  Result := DoCommand('PASV', Response);
end;

function TFtpClientCommand.Password(const Password: String; out Response: String): Word;
begin
  Result := DoCommand('PASS ' + Password, Response);
end;

function TFtpClientCommand.Port(const Address: String; Port: Word; out Response: String): Word;
var
  sPort: String;
begin
  sPort := Str.Format('%s,%d,%d', [Str.Translate(Address, '.', ','), Hi(Port), Lo(Port)]);
  Result := DoCommand('PORT ' + sPort, Response);
end;

function TFtpClientCommand.Quit(out Response: String): Word;
begin
  Result := DoCommand('QUIT', Response);
end;

function TFtpClientCommand.Reinitialize(out Response: String): Word;
begin
  Result := DoCommand('REIN', Response);
end;

function TFtpClientCommand.RemoveDir(const Path: String; out Response: String): Word;
begin
  Result := DoCommand('RMD ' + Path, Response);
end;

function TFtpClientCommand.RenameFileFrom(const OldPath: String; out Response: String): Word;
begin
  Result := DoCommand('RNFR ' + OldPath, Response);
end;

function TFtpClientCommand.RenameFileTo(const NewPath: String; out Response: String): Word;
begin
  Result := DoCommand('RNTO ' + NewPath, Response);
end;

function TFtpClientCommand.Restart(Marker: LongWord; out Response: String): Word;
begin
  Result := DoCommand('REST ' + Int.ToStr(Marker), Response);
end;

function TFtpClientCommand.Retrieve(const Path: String; out Response: String): Word;
begin
  Result := DoCommand('RETR ' + Path, Response);
end;

function TFtpClientCommand.SiteParameters(out Response: String): Word;
begin
  Result := DoCommand('SITE', Response);
end;

function TFtpClientCommand.Status(const Path: String; out Response: String): Word;
begin
  Result := DoCommand('STAT ' + Path, Response);
end;

function TFtpClientCommand.Store(const Path: String; out Response: String): Word;
begin
  Result := DoCommand('STOR ' + Path, Response);
end;

function TFtpClientCommand.Structure(Code: TFtpStructCode; out Response: String): Word;
begin
  Result := DoCommand('STRU ' + AStructCode[Code], Response);
end;

function TFtpClientCommand.System(out Response: String): Word;
begin
  Result := DoCommand('SYST', Response);
end;

function TFtpClientCommand.TransferMode(Mode: TFtpTransferMode; out Response: String): Word;
begin
  Result := DoCommand('MODE ' + ATransferMode[Mode], Response);
end;

function TFtpClientCommand.TypeCode(Code: TFtpTypeCode; out Response: String): Word;
begin
  Result := DoCommand('TYPE ' + ACode[Code], Response);
end;

function TFtpClientCommand.User(const UserName: String; out Response: String): Word;
begin
  Result := DoCommand('USER ' + UserName, Response);
end;

function TFtpClientCommand.Mount(const Path: String; out Response: String): Word;
begin
  Result := DoCommand('SMNT ' + Path, Response);
end;

end.
