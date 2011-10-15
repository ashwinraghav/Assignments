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

unit SilSiLayer;

interface

{$include Defines.inc}

uses
  Sil;

type
  ILayer = interface;
  ILayerStatus = interface;
  ILayerControl = interface;
  ILayerLinkControl = interface;
  ILayerLink = interface;
  ILayerOperation = interface;
  ILayerDuplicate = interface;
  ILayerSelfDuplicate = interface;
  ILayerLinkList = interface;
  ILayerChain = interface;
  ILayerSlot = interface;
  ILayerActivationEvents = interface;
  ILayerCommand = interface;
  ILayerCommands = interface;
  ILayerTerminal = interface;
  ILayerProtocol = interface;
  ILayerProtocolControl = interface;
  ILayerProtocolStatus = interface;
  //ILayerProtocolMessage = interface;
  ILayerWaitPacket = interface;
  ILayerBindings = interface;
  ILayerStream = interface;
  ILayerThreadHook = interface;

  ILayer = interface
    ['{E591F5EC-B463-456E-ADC6-B2F306B32D92}']
    function GetId: Variant;
    procedure SetId(const Value: Variant);
    function GetParameters: IParameterList;
    property Id: Variant read GetId write SetId;
    property Parameters: IParameterList read GetParameters;
  end;

  ILayerStatus = interface
    ['{C3D1A4CC-8C7C-4F38-AC48-22E6CDFBD06E}']
    function GetIsActive: Boolean;
    property IsActive: Boolean read GetIsActive;
  end;

  ILayerControl = interface (ILayerStatus)
    ['{B2CA85E6-F67C-4D81-9B21-091F625B3924}']
    procedure Activate(const Link: ILayerLink = nil; const Context: IUnknown = nil);
    procedure Deactivate(const Link: ILayerLink = nil; const Context: IUnknown = nil; IsBroken: Boolean = false);
    procedure Reactivate(const Link: ILayerLink = nil; const Context: IUnknown = nil);
  end;

  ILayerLinkControl = interface (ILayerStatus)
    ['{70E61D92-32FE-4A33-ABED-CF555CA1D31C}']
    procedure Activate(const Context: IUnknown = nil);
    procedure Deactivate(const Context: IUnknown = nil; IsBroken: Boolean = false);
    procedure Reactivate(const Context: IUnknown = nil);
  end;

  ILayerOperation = interface (ILayer)
    ['{C3E55730-6A91-491F-AA24-71494743C183}']
    function GetControl: ILayerControl;
    procedure Write(const Command: ILayerCommand);
    procedure Read(const Command: ILayerCommand);
    procedure Receive(const Command: ILayerCommand);
    property Control: ILayerControl read GetControl;
  end;

  ILayerDuplicate = interface
    ['{60B83B72-26A6-464D-9371-F9E03E0A6817}']
    function Duplicate(out Obj: IUnknown; const Context: IUnknown = nil): Boolean;
  end;

  ILayerSelfDuplicate = interface
    ['{03ACDFCA-E693-4C65-A1A8-7D077B6DF184}']
  end;

  ILayerLink = interface
    ['{62F84D71-5F86-49F9-B4DA-BF1CE4EAC8C2}']
    function GetChain: ILayerChain;
    procedure SetChain(const Value: ILayerChain);
    function GetLower: ILayerLink;
    procedure SetLower(const Value: ILayerLink);
    function GetUpper: ILayerLink;
    procedure SetUpper(const Value: ILayerLink);
    function GetOperation: ILayerOperation;
    function GetControl: ILayerLinkControl;
    property Chain: ILayerChain read GetChain write SetChain;
    property Lower: ILayerLink read GetLower write SetLower;
    property Upper: ILayerLink read GetUpper write SetUpper;
    property Operation: ILayerOperation read GetOperation;
    property Control: ILayerLinkControl read GetControl;
  end;

  ILayerTerminal = interface
    ['{0E9B18A9-C212-46A0-BC6E-E8290585B09F}']
    function GetLink: ILayerLink;
    procedure StartLayer(const Context: IUnknown = nil);
    procedure StopLayer(const Context: IUnknown = nil);
    property Link: ILayerLink read GetLink;
  end;

  ILayerCommand = interface 
    ['{FE34086E-5D78-43DE-B240-70185DBDB889}']
    function GetCaller: ILayerLink;
    function GetLink: ILayerLink;
    function GetParams: IParameterList;
    function GetContext: IInterface;
    procedure SetContext(const Value: IUnknown);
    function GetPacket: IPacket;
    function GetTarget: Variant;
    property Caller: ILayerLink read GetCaller;
    property Link: ILayerLink read GetLink;
    property Params: IParameterList read GetParams;
    property Context: IInterface read GetContext write SetContext;
    property Packet: IPacket read GetPacket;
    property Target: Variant read GetTarget;
  end;

  ILayerCommands = interface
    ['{7A759A3E-86C1-4367-814D-58BFA1D0B52E}']
    function GetCount: Integer;
    function GetFirst: ILayerCommand;
    function GetLast: ILayerCommand;
    function GetParams: IParameterList;
    function GetItem(Index: Integer): ILayerCommand;
    function Enumerate(var Enum: IEnumerator; out Item: ILayerCommand): Boolean;
    function Find(const Layer: IUnknown; out Item: ILayerCommand): Boolean;
    property Count: Integer read GetCount;
    property First: ILayerCommand read GetFirst;
    property Last: ILayerCommand read GetLast;
    property Params: IParameterList read GetParams;
    property Item[Index: Integer]: ILayerCommand read GetItem; default;
  end;

  RLayerActivated = record
    Link: ILayerLink;
    Context: IUnknown;
  end;

  RLayerDeactivated = record
    Link: ILayerLink;
    Context: IUnknown;
    IsBroken: Boolean;
    Reactivate: Boolean;
  end;

  ILayerActivationEvents = interface
    ['{780DF4CC-2525-43A1-B5DD-CD265D516848}']
    procedure OnLayerActivated(const Event: RLayerActivated);
    procedure OnLayerDeactivated(const Event: RLayerDeactivated);
  end;

  ILayerLinkList = interface (ILayerLink) 
    ['{B844DE94-889F-4C02-85A8-B4D940C72AD8}']
    function GetId: Variant;
    procedure SetId(const Value: Variant);
    function GetParameters: IParameterList;
    procedure Add(const Value: IUnknown); overload;
    procedure Add(const Value: IUnknown; out Obj; const IID: TGuid); overload;
    procedure Remove(const Value: IUnknown);
    procedure AddList(const List: ILayerLinkList);
    procedure Insert(const Before, Value: IUnknown);
    procedure Clear(Recursive: Boolean = false);
    function Enumerate(var Enum: IEnumerator; out Item: IUnknown): Boolean;
    function GetFirst(const IID: TGuid; out Obj; FromIndex: Integer = -1): Boolean;
    function GetLast(const IID: TGuid; out Obj; FromIndex: Integer = -1): Boolean;
    function GetBefore(const Value: IUnknown; out Obj: IUnknown): Boolean;
    function GetAfter(const Value: IUnknown; out Obj: IUnknown): Boolean;
    property Id: Variant read GetId write SetId;
    property Parameters: IParameterList read GetParameters;
  end;

  ILayerChain = interface (ILayerLinkList)
    ['{2E6F04CE-5DDB-45DD-A0AE-521E505955A3}']
  end;

  ILayerSlot = interface (ILayerLinkList)
    ['{E7CAA1B9-3036-4B85-A8E0-BDA52BD9FAEE}']
  end;

  ILayerChainSource = interface
    ['{52436B38-675D-4816-BA49-1FEAA54CA357}']
    procedure CreateChain(const Chain: ILayerChain; const Context: IUnknown = nil);
    procedure DestroyChain(const Chain: ILayerChain; const Context: IUnknown = nil);
  end;

  PProtocolMessage = ^RProtocolMessage;
  RProtocolMessage = packed record
    Id: Word;
    Packet: IPacket;
  end;

  ILayerProtocol = interface
    ['{D9F5F8AD-3A74-4F5F-8DDC-4A3B96B4A143}']
    function CreatePacket(Data: Integer = -1; Flags: LongWord = 0): IPacket;
    procedure SendPacket(const Packet: IPacket; const Context: IUnknown = nil; const Params: IParameterList = nil); overload;
    procedure SendPacket(const Packet: IPacket; out Reply: IPacket; Data: Integer = -1; Timeout: LongWord = INFINITE; const Context: IUnknown = nil; const Params: IParameterList = nil); overload;
    procedure WaitPacket(const Request: IPacket; out Reply: IPacket; Response: Integer = -1; Timeout: LongWord = INFINITE);
    function GetHook(out Hook: IUnknown): Boolean;
  end;

  ILayerProtocolControl = interface (IDispatchable)
    ['{00ECB345-5B38-459A-B724-DED5C329E33A}']
    function GetParameters: IParameters;
    function GetStatus: ILayerStatus;
    procedure Activate(const Protocol: ILayerProtocol; const Context: IUnknown = nil);
    procedure Deactivate(const Protocol: ILayerProtocol; const Context: IUnknown = nil);
    property Parameters: IParameters read GetParameters;
    property Status: ILayerStatus read GetStatus;
  end;

  ILayerProtocolStatus = interface
    ['{6DF40F77-BA98-4D8E-9AB1-8B2A30A6F0B7}']
    procedure OnProtocolActivation(const Sender: IUnknown);
    procedure OnProtocolDeactivation(const Sender: IUnknown);
  end;

  ILayerWaitPacket = interface
    ['{9A123980-633E-4AD7-9D68-D67F3CD039AD}']
    function GetEvent: IEvent;
    function GetPacket: IPacket;
    procedure SetPacket(const Value: IPacket);
    function GetSource: IPacket;
    procedure SetSource(const Value: IPacket);
    function GetRequest: IPacket;
    function GetData: Integer;
    procedure Signal;
    property Event: IEvent read GetEvent;
    property Packet: IPacket read GetPacket write SetPacket;
    property Source: IPacket read GetSource write SetSource;
    property Request: IPacket read GetRequest;
    property Data: Integer read GetData;
  end;

  ILayerStream = interface
    ['{2FF83A75-CC92-4912-BBF2-767A79698816}']
    function GetDevice: IUnknown;
    procedure SetDevice(const Value: IUnknown);
    property Device: IUnknown read GetDevice write SetDevice;
  end;

  ILayerBindings = interface (IParameterList)
    ['{34122C58-5217-4379-A333-C1CC67F4CF03}']
    procedure Add(const Name: String; const IID: TGUID; const Getter: IUnknown; const Setter: PUnknown = nil); overload;
    procedure Add(const Name: String; const IID: TGUID; const Setter: PUnknown); overload;
  end;

  ILayerThreadHook = interface
    ['{84652E4F-FEF0-498B-9BE4-C3E25DC275CA}']
    procedure InitializeThread(const Layer: ILayer);
    procedure FinalizeThread(const Layer: ILayer);
    procedure UnhandledException(const Layer: ILayer; Error: Exception);
  end;

implementation

end.
