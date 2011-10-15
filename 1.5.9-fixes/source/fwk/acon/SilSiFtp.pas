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

unit SilSiFtp;

interface

uses
  SilLiStream,
  SilSiFtpCommand;

type
  IFtpClient = interface
    ['{201DAEA8-BC68-4F32-8939-3EBA4E4A1CA4}']
    function Login(const UserName, Password: String; const AccountInfo: String = ''): Boolean;
    function ChangeDir(const Path: String): Boolean;
    function ChangeDirToParent: Boolean;
    function Quit: Boolean;
    function Reinitialize: Boolean;
    function Representation(Code: TFtpTypeCode): Boolean;
    function Structure(Code: TFtpStructCode): Boolean;
    function TransferMode(Mode: TFtpTransferMode): Boolean;
    function Retrieve(const Path: String; const Stream: IRandomStream = nil; Marker: LongWord = 0): Boolean;
    function Store(const Path: String; const Stream: IRandomStream = nil; Marker: LongWord = 0): Boolean;
    function Append(const Path: String; const Stream: IRandomStream = nil): Boolean;
    function Allocate(Size: LongWord; Pages: LongWord = 0): Boolean;
    function RenameFile(const OldPath, NewPath: String): Boolean;
    function AbortTransfer: Boolean;
    function DeleteFile(const Path: String): Boolean;
    function RemoveDir(const Path: String): Boolean;
    function MakeDir(const Path: String): Boolean;
    function CurrentDir: String;
    function FileSize(const Path: String): LongWord;
    function List(const Path: String = ''): Boolean;
    function NameList(const Path: String = ''): Boolean;
    function SiteParameters: String;
    function System: String;
    function Status(const Path: String = ''): String;
    function Help(const Command: String = ''): String;
    function Noop: Boolean;
    procedure PassiveMode(Enabled: Boolean);
  end;

  RFtpClientListEvent = record
    Sender: IFtpClient;
    Path: String;
    Line: String;
  end;

  RFtpClientFileEvent = record
    Sender: IFtpClient;
    Path: String;
    Stream: IRandomStream;
  end;

  IFtpClientEvents = interface
    ['{7042FE8C-E9F8-4379-B32F-535B105C7B0A}']
    procedure OnList(var Event: RFtpClientListEvent);
    procedure OnFileRetrieve(var Event: RFtpClientFileEvent);
    procedure OnFileSend(var Event: RFtpClientFileEvent);
  end;

  RFtpClientPortListen = record
    Sender: IFtpClient;
    Address: String;
    Port: Word;
    Listener: IUnknown;
  end;

  RFtpClientPortAccept = record
    Sender: IFtpClient;
    Listener: IUnknown;
    DataStream: IStream;
  end;

  RFtpClientPassiveConnect = record
    Sender: IFtpClient;
    Address: String;
    Port: Word;
    DataStream: IStream;
  end;

  IFtpClientConnectionEvents = interface
    ['{469A1A81-909F-11D4-989E-00104B0FA1EF}']
    procedure OnPortListen(var Event: RFtpClientPortListen);
    procedure OnPortAccept(var Event: RFtpClientPortAccept);
    procedure OnPassiveConnect(var Event: RFtpClientPassiveConnect);
  end;

implementation

end.
 