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

unit SilSiFtpCommand;

interface

uses
  SilSiAsciiProtocol;

type
  TFtpTypeCode = (tcAscii, tcEbcdic, tcImage, tcLocal);
  TFtpStructCode = (scFile, scRecord, scPage);
  TFtpTransferMode = (tmStream, tmBlock, tmCompressed);

  IFtpClientCommand = interface (IAsciiProtocol)
    ['{185189A3-3569-4A8E-A1E7-21E9DA3BDDC9}']
    function ReadLn(out Response: String): Word;
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
  end;

implementation

end.
 