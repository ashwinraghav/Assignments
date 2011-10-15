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

unit SilOiSocket;

{$I Defines.inc}

interface

uses
  SilOsTypes,
  SilLiEnumerator,
  SilLiLock,
  SilLiStream,
  SilOiHandle,
  SilLiStringList;

const
  CAddressShort = '%a';
  CAddressLong = '%a:%p';
  CAddressAndMask = '%a/%t';

  CAddress = '%a';
  CAddressPort = '%p';
  CAddressMask = '%m';
  CAddressBroadcast = '%b';
  CAddressNetwork = '%n';
  CAddressNetBits = '%t';

type
  ISocket = interface;
  ISocketPeer = interface;
  ISocketStream = interface;
  ISocketParameters = interface;
  ISocketInfo = interface;
  ISocketPeerInfo = interface;
  ISocketAddress = interface;
  ISocketAddresses = interface;
  ISocketAddressList = interface;
  ISocketAddressDef = interface;
  ISocketClient = interface;
  ISocketServer = interface;
  ISocketProtocol = interface;
  ISocketService = interface;
  ISockets = interface;
  ISocketList = interface;

  TSocketState = (ssRead, ssWrite, ssError);
  TSocketStates = set of TSocketState;
  TSocketShutdown = (shRead, shWrite, shBoth);

  TSocketType = (stUnknown, stStream, stDatagram, stRaw, stRdm, stSeqPacket);
  TSocketProtocol = (spUnknown, spIP, spICMP, spTCP, spUDP, spRaw);
  TSocketNetworkClass = (snUnknown, snInternal, snClassA, snClassB, snClassC);

  TSocketFlag = (
    sfBroadcast, sfDebugging, sfDontLinger, sfDontRouting, sfKeepAlive, sfOutOfBandReception,
    sfReuseAddress, sfSendTimeout, sfReceiveTimeout, sfNoDelay);

  TSocketFlags = set of TSocketFlag;

  TSocketAddressFlag = (
    afHost, afAddress, afPort, afTypeSpec, afProtocol,
    afSubnetMask, afNetworkClass, afBroadcast, afNetwork);

  TSocketAddressFlags = set of TSocketAddressFlag;

  ISocketAddress = interface
    ['{0FB22EFA-0675-4BFB-8B1D-F5451F31EE4A}']
    function GetHost: String;
    function GetAddress: LongWord;
    function GetPort: Word;
    function GetTypeSpec: TSocketType;
    function GetProtocol: TSocketProtocol;
    function GetSubnetMask: LongWord;
    function GetNetworkClass: TSocketNetworkClass;
    function GetBroadcast: LongWord;
    function GetNetwork: LongWord;
    function GetFlags: TSocketAddressFlags;
    // properties
    function Format(const FmtStr: String = CAddressShort): String;
    property Host: String read GetHost;
    property Address: LongWord read GetAddress;
    property Port: Word read GetPort;
    property TypeSpec: TSocketType read GetTypeSpec;
    property Protocol: TSocketProtocol read GetProtocol;
    property SubnetMask: LongWord read GetSubnetMask;
    property NetworkClass: TSocketNetworkClass read GetNetworkClass;
    property Broadcast: LongWord read GetBroadcast;
    property Network: LongWord read GetNetwork;
    property Flags: TSocketAddressFlags read GetFlags;
  end;

  ISocketAddressDef = interface (ISocketAddress)
    ['{15CBAB82-BDFD-48A4-8AD5-0B7D0085C54A}']
    procedure SetHost(const Value: String);
    procedure SetAddress(const Value: LongWord);
    procedure SetPort(const Value: Word);
    procedure SetTypeSpec(const Value: TSocketType);
    procedure SetProtocol(const Value: TSocketProtocol);
    procedure SetSubnetMask(const Value: LongWord);
    // properties
    property Host: String read GetHost write SetHost;
    property Address: LongWord read GetAddress write SetAddress;
    property Port: Word read GetPort write SetPort;
    property TypeSpec: TSocketType read GetTypeSpec write SetTypeSpec;
    property Protocol: TSocketProtocol read GetProtocol write SetProtocol;
    property SubnetMask: LongWord read GetSubnetMask write SetSubnetMask;
  end;

  ISocketAddresses = interface
    ['{689EC973-915E-431B-8A03-165845057CBE}']
    function GetCount: Integer;
    function Get(Index: Integer): ISocketAddress;
    // methods
    function Enumerate(var Enum: IEnumerator; out Item: ISocketAddress): Boolean;
    function ValidIndex(Index: Integer): Boolean;
    procedure CheckIndex(Index: Integer);
    // properties
    property Count: Integer read GetCount;
    property Items[Index: Integer]: ISocketAddress read Get; default;
  end;

  ISocketAddressList = interface (ISocketAddresses)
    ['{C7170D7F-EED4-4F90-B149-61C24F6B96B9}']
    // methods
    procedure Put(Index: Integer; const Value: ISocketAddress);
    function Add(const Value: ISocketAddress): Integer;
    procedure Delete(Index: Integer);
    procedure Insert(Index: Integer; const Value: ISocketAddress);
    function Remove(const Item: IUnknown): Integer;
    function IndexOf(const Item: IUnknown): Integer;
    procedure Clear;
    // properties
    procedure Exchange(Index1, Index2: Integer);
    property Items[Index: Integer]: ISocketAddress read Get write Put; default;
  end;

  ISocketParameters = interface
    ['{71D636E0-F82D-46E6-8CAA-D68A905096B4}']
    //function GetAddress: ISocketAddress;
    function GetWriteTimeout: LongWord;
    procedure SetWriteTimeout(const Value: LongWord);
    function GetReadTimeout: LongWord;
    procedure SetReadTimeout(const Value: LongWord);
    function GetFlag(Index: TSocketFlag): Boolean;
    procedure SetFlag(Index: TSocketFlag; const Value: Boolean);
    function GetReceiveBufferSize: LongWord;
    procedure SetReceiveBufferSize(const Value: LongWord);
    function GetSendBufferSize: LongWord;
    procedure SetSendBufferSize(const Value: LongWord);
    function GetAutoDisconnect: Boolean;
    procedure SetAutoDisconnect(Value: Boolean);
    // properties
    property WriteTimeout: LongWord read GetWriteTimeout write SetWriteTimeout;
    property ReadTimeout: LongWord read GetReadTimeout write SetReadTimeout;
    property Flags[Index: TSocketFlag]: Boolean read GetFlag write SetFlag;
    property ReceiveBufferSize: LongWord read GetReceiveBufferSize write SetReceiveBufferSize;
    property SendBufferSize: LongWord read GetSendBufferSize write SetSendBufferSize;
    property AutoDisconnect: Boolean read GetAutoDisconnect write SetAutoDisconnect;
  end;

  ISocketInfo = interface
    ['{6AAD74E7-DD82-45EC-9158-241A7728E244}']
    function GetLocal: ISocketAddress;
    function GetErrorStatus: Integer;
    // properties
    property Local: ISocketAddress read GetLocal;
    property ErrorStatus: Integer read GetErrorStatus;
  end;

  ISocketPeerInfo = interface (ISocketInfo)
    ['{6AAD74E7-DD82-45EC-9158-241A7728E244}']
    function GetRemote: ISocketAddress;
    // properties
    property Remote: ISocketAddress read GetRemote;
  end;

  TSocketStreamFlag = (rfOutOfBand, rfPeek, wfOutOfBand);
  TSocketStreamFlags = set of TSocketStreamFlag;

  ISocketStream = interface (IStream)
    ['{0E6244FA-4CA4-4B0C-9AFA-91DDEA807AAA}']
    function Read(var Buffer; Count: LongWord; Flags: TSocketStreamFlags = []): LongWord; overload;
    function Write(const Buffer; Count: LongWord; Flags: TSocketStreamFlags = []): LongWord; overload;
    function ReadFrom(var Buffer; Count: LongWord; out Address: ISocketAddress; Flags: TSocketStreamFlags = []): LongWord;
    function WriteTo(const Buffer; Count: LongWord; const Address: ISocketAddress; Flags: TSocketStreamFlags = []): LongWord;
  end;

  ISocket = interface (IHandledObject)
    ['{58EEA25D-ED83-4162-9B07-11BD7A46A6E2}']
    function GetParameters: ISocketParameters;
    // methods
    procedure Bind(const Address: ISocketAddress = nil); overload;
    procedure Bind(const Host: String; Port: Word = 0); overload;
    procedure Bind(const Address: Cardinal; Port: Word = 0); overload;
    procedure Bind(TypeSpec: TSocketType; Protocol: TSocketProtocol; const Host: String; Port: Word = 0); overload;
    procedure Bind(TypeSpec: TSocketType; Protocol: TSocketProtocol; Address: Cardinal; Port: Word = 0); overload;
    function WaitFor(const States: TSocketStates; Timeout: LongWord = INFINITE): Boolean;
    procedure Shutdown(const Mode: TSocketShutdown);
    procedure Disconnect;
    // properties
    property Parameters: ISocketParameters read GetParameters;
  end;

  ISockets = interface
    ['{BCB1AD4A-E066-400C-85E0-332A649C97D8}']
    function GetCount: Integer;
    function Get(Index: Integer): ISocket;
    // methods
    function Enumerate(var Enum: IEnumerator; out Item: ISocket): Boolean;
    function ValidIndex(Index: Integer): Boolean;
    procedure CheckIndex(Index: Integer);
    // properties
    property Count: Integer read GetCount;
    property Items[Index: Integer]: ISocket read Get; default;
  end;

  ISocketList = interface (ISockets)
    ['{B9AA2E19-E6D8-4589-9DD1-88D5E11E8276}']
    // methods
    procedure Put(Index: Integer; const Value: ISocket);
    function Add(const Value: ISocket): Integer;
    procedure Delete(Index: Integer);
    procedure Insert(Index: Integer; const Value: ISocket);
    function Remove(const Item: IUnknown): Integer;
    function IndexOf(const Item: IUnknown): Integer;
    procedure Clear;
    // properties
    procedure Exchange(Index1, Index2: Integer);
    property Items[Index: Integer]: ISocket read Get write Put; default;
  end;

  ISocketWaitList = interface
    ['{ECF7403C-5872-4373-B567-BBF03E4AB4D6}']
    // ...
  end;

  ISocketPeer = interface (ISocket)
    ['{CCD6D7A0-49C2-42B0-9A45-A3816CE4E085}']
    function GetStream: ISocketStream;
    function GetInfo: ISocketPeerInfo;
    // properties
    property Stream: ISocketStream read GetStream;
    property Info: ISocketPeerInfo read GetInfo;
  end;

  ISocketClient = interface (ISocketPeer)
    ['{2BE8598D-93E8-404F-943F-11C755DF963B}']
    function GetIsLocal: Boolean;
    function GetIsConnected: Boolean;
    procedure Connect(const Address: ISocketAddress = nil); overload;
    procedure Connect(const Host: String; Port: Word); overload;
    procedure Connect(const Address: Cardinal; Port: Word); overload;
    // properties
    property IsLocal: Boolean read GetIsLocal;
    property IsConnected: Boolean read GetIsConnected;
  end;

  ISocketServer = interface (ISocket)
    ['{A88D09B4-21D2-11D4-987F-00104B0FA1EF}']
    function GetStream: ISocketStream;
    function GetInfo: ISocketInfo;
    function GetIsListening: Boolean;
    // methods
    procedure Listen;
    procedure Cancel;
    function Accept(out Client: ISocketClient): Boolean;
    // properties
    property Stream: ISocketStream read GetStream;
    property Info: ISocketInfo read GetInfo;
    property IsListening: Boolean read GetIsListening;
  end;

  ISocketEtcInfo = interface 
    ['{D47F5E13-CE3C-45D8-A231-FC28AAA81025}']
    function GetIsValid: Boolean;
    function GetName: String;
    function GetAliases: IStringList;
    function GetComment: String;
    // properties
    property IsValid: Boolean read GetIsValid;
    property Name: String read GetName;
    property Aliases: IStringList read GetAliases;
    property Comment: String read GetComment;
  end;

  ISocketProtocol = interface (ISocketEtcInfo)
    ['{5E593988-770E-4E97-9A2C-598D26625BA0}']
    function GetId: Smallint;
    // properties
    property Id: Smallint read GetId;
  end;

  ISocketService = interface (ISocketEtcInfo)
    ['{19A462D9-8FD2-47B3-AFEE-1AD1C41ACCD5}']
    function GetPort: Word;
    function GetProtocol: ISocketProtocol;
    // properties
    property Port: Word read GetPort;
    property Protocol: ISocketProtocol read GetProtocol;
  end;

implementation

end.


