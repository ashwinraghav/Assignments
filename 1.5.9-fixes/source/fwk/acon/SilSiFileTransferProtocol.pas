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

unit SilSiFileTransferProtocol;

interface

{$INCLUDE Defines.inc}

uses
  Sil,
  SilSiProtocolBase,
  SilSiAbstractConnection;

type
  TFtpTypeCode = (tcAscii, tcEbcdic, tcImage, tcLocal);
  TFtpStructCode = (scFile, scRecord, scPage);
  TFtpTransferMode = (tmStream, tmBlock, tmCompressed);

  TFtpHostAddress = record
    Address: String;
    case Byte of
      0: (Port: Word);
      1: (PortLo: Byte; PortHi: Byte);
  end;

  IFtpServer = interface (IProtocol)
    ['{68389B31-7E94-11D4-9897-00104B0FA1EF}']
    // set def params
  end;

  IFtpClient = interface (IProtocol)
    ['{A0E48084-0ACD-423D-8924-9E3D02571995}']
    function GetDataConnection: IAbstractConnection;
    procedure SetDataConnection(const Value: IAbstractConnection);
    function Login(const UserName, Password: String): Boolean;           
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
    function DeleteFile(const PathName: String): Boolean;                
    function RemoveDir(const PathName: String): Boolean;                 
    function MakeDir(const PathName: String): Boolean;                   
    function CurrentDir: String;                                         
    function FileSize(const PathName: String): LongWord;              
    function List(const PathName: String = ''): Boolean;                 
    function NameList(const PathName: String = ''): Boolean;             
    function SiteParameters: String;                                     
    function System: String;                                             
    function Status(const PathName: String = ''): String;                
    function Help(const Command: String = ''): String;                   
    function Noop: Boolean;
    procedure WaitTransferComplete;
    property DataConnection: IAbstractConnection read GetDataConnection write SetDataConnection;
  end;

type
  TFtpListenEvent = record
    Sender: IFtpClient;
    Address: String;
    Port: Word;
  end;

  TFtpConnectEvent = record
    Sender: IFtpClient;
    Address: String;
    Port: Word;
    Connection: IAbstractConnection;
  end;

  TFtpListEvent = record
    Sender: IFtpClient;
    Line: String;
  end;

  TFtpFileTransferEvent = record
    Sender: IFtpClient;
    FileName: String;
    Marker: LongWord;
    Stream: IRandomStream;
  end;

  IFtpClientSocketEvents = interface
    ['{469A1A81-909F-11D4-989E-00104B0FA1EF}']
    procedure OnListenToServer(var Event: TFtpListenEvent);
    procedure OnConnectToServer(var Event: TFtpConnectEvent);
  end;

  IFtpClientEvents = interface
    ['{3B918F01-8271-11D4-9898-00104B0FA1EF}']
    procedure OnList(var Event: TFtpListEvent);
    procedure OnListNames(var Event: TFtpListEvent);
    procedure OnFileRetrieved(var Event: TFtpFileTransferEvent);
    procedure OnFileSent(var Event: TFtpFileTransferEvent);
  end;

  TFtpResultMessage = record
    Code: Word;
    Text: String;
  end;

  TFtpServerEvent = record
    Sender: IFtpServer;
    Result: TFtpResultMessage;
  end;

  TFtpLoginEvent = record
    Sender: IFtpServer;
    UserName: String;
    Password: String;
    Result: TFtpResultMessage;
  end;

  TFtpChangeDirEvent = record
    Sender: IFtpServer;
    Path: String;
    Result: TFtpResultMessage;
  end;

  TFtpAddressEvent = record
    Sender: IFtpServer;
    Address: String;
    Port: Word;
    Result: TFtpResultMessage;
  end;

  TFtpRepresentationEvent = record
    Sender: IFtpServer;
    Code: TFtpTypeCode;
    Result: TFtpResultMessage;
  end;

  TFtpStructureEvent = record
    Sender: IFtpServer;
    Code: TFtpStructCode;
    Result: TFtpResultMessage;
  end;

  TFtpTransferModeEvent = record
    Sender: IFtpServer;
    Mode: TFtpTransferMode;
    Result: TFtpResultMessage;
  end;

  TFtpRestartEvent = record
    Sender: IFtpServer;
    Marker: LongWord;
    Result: TFtpResultMessage;
  end;

  TFtpPathEvent = record
    Sender: IFtpServer;
    Path: String;
    Result: TFtpResultMessage;
  end;

  TFtpAllocateEvent = record
    Sender: IFtpServer;
    Size: LongWord;
    Pages: LongWord;
    Result: TFtpResultMessage;
  end;

  TFtpRenameFileEvent = record
    Sender: IFtpServer;
    FromPath: String;
    ToPath: String;
    Result: TFtpResultMessage;
  end;

  IFtpServerEvents = interface
    ['{28BAE81D-B940-4344-9703-D3F576F63404}']
    procedure OnLogin(var Event: TFtpLoginEvent);
    procedure OnChangeDir(var Event: TFtpChangeDirEvent);
    procedure OnQuit(var Event: TFtpServerEvent);
    procedure OnReinitialize(var Event: TFtpServerEvent);
    procedure OnPort(var Event: TFtpServerEvent);
    procedure OnPassive(var Event: TFtpAddressEvent);
    procedure OnRepresentation(var Event: TFtpRepresentationEvent);
    procedure OnStructure(var Event: TFtpStructureEvent);
    procedure OnTransferMode(var Event: TFtpTransferModeEvent);
    procedure OnRestart(var Event: TFtpRestartEvent);
    procedure OnRetrieve(var Event: TFtpPathEvent);
    procedure OnStore(var Event: TFtpPathEvent);
    procedure OnAppend(var Event: TFtpPathEvent);
    procedure OnAllocate(var Event: TFtpAllocateEvent);
    procedure OnRenameFile(var Event: TFtpRenameFileEvent);
    procedure OnAbortTransfer(var Event: TFtpServerEvent);
    procedure OnDeleteFile(var Event: TFtpPathEvent);
    procedure OnRemoveDir(var Event: TFtpPathEvent);
    procedure OnList(var Event: TFtpPathEvent);
    procedure OnNameList(var Event: TFtpPathEvent);
    procedure OnSiteParameters(var Event: TFtpServerEvent);
    procedure OnSystem(var Event: TFtpServerEvent);
    procedure OnStatus(var Event: TFtpPathEvent);
    procedure OnHelp(var Event: TFtpServerEvent);
  end;

implementation

end.
