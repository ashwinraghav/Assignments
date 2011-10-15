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

unit SilLiGlobalServicesV2;

{$I Defines.inc}

interface

uses
  SilBeTypes,
  SilLiLock,
  SilLiEnumerator;

type
  PGlobalService = ^RGlobalService;
  
  IGlobalServicesV2 = interface;
  IGlobalServiceListV2 = interface;
  IGlobalListV2 = interface;
  IGlobalServiceV2 = interface;
  IGlobalReferencesV2 = interface;
  IGlobalLinksV2 = interface;

  PGlobalRef = ^RGlobalRef;
  RGlobalRef = record
    Ref: PUnknown;
    Module: LongWord;
  end;

  TGlobalServiceKind = (skGlobal, skLocal);

  RGlobalService = record
    ID: PGUID;
    Name: PShortString;
    Module: LongWord;
    Initialize: procedure(Self: PGlobalService; const Services: IGlobalServiceListV2);
    Finalize: procedure(Self: PGlobalService; const Services: IGlobalServiceListV2);
    Create: function(Self: PGlobalService; const Services: IGlobalServiceListV2): IUnknown;
    Destroy: procedure(Self: PGlobalService; const Services: IGlobalServiceListV2);
    RefAdded: procedure(Self: PGlobalService; const Services: IGlobalServiceListV2; const Ref: PUnknown);
    RefRemoved: procedure(Self: PGlobalService; const Services: IGlobalServiceListV2; const Ref: PUnknown);
    LinkAdded: procedure(Self: PGlobalService; const Services: IGlobalServiceListV2; const Link: PGlobalService);
    LinkRemoved: procedure(Self: PGlobalService; const Services: IGlobalServiceListV2; const Link: PGlobalService);
  end;

  IGlobalServicesV2 = interface
    ['{0724FCC5-84BC-4655-82C7-0918C8A82A8E}']
    function Locked: ILock;
    function Find(const ID: TGUID): Boolean; overload;
    function Find(const ID: TGUID; const IID: TGuid; Obj: PUnknown; Module: LongWord): Boolean; overload;
    procedure Link(const ID: TGUID; const Parent: TGUID);
    procedure Unlink(const ID: TGUID; const Parent: TGUID);
    procedure Get(const ID: TGUID; const IID: TGuid; Obj: PUnknown; Module: LongWord); overload;
    procedure Release(const ID: TGUID; Obj: PUnknown; Module: LongWord); overload;
    function Enumerate(var Enum: IEnumerator; const IID: TGuid; out Obj): Boolean; overload;
  end;

  IGlobalServiceListV2 = interface (IGlobalServicesV2)
    ['{DCE210B7-F9A4-463D-B838-EB945268B2C9}']
    procedure Register(Service: PGlobalService; Module: LongWord); overload;
    procedure Register(Service: PGlobalService; const Instance: IUnknown; Module: LongWord); overload;
    procedure Register(Service: PGlobalService; const Instance: IUnknown; const IID: TGuid; Obj: PUnknown; Module: LongWord); overload;
    procedure Unregister(Service: PGlobalService; Module: LongWord); overload;
    procedure Unregister(Module: LongWord); overload;
    procedure Unregister(const ID: TGUID; Module: LongWord); overload;
    procedure Get(Service: PGlobalService; const IID: TGuid; Obj: PUnknown; Module: LongWord); overload;
    procedure Release(Service: PGlobalService; Obj: PUnknown; Module: LongWord); overload;
    procedure Remove(const ID: TGUID; Module: LongWord); overload;
    procedure Remove(Service: PGlobalService; Module: LongWord); overload;
    function Enumerate(var Enum: IEnumerator; out Service: IGlobalServiceV2): Boolean; overload;
    function Find(const ID: TGUID; out Service: IGlobalServiceV2): Boolean; overload;
    function Get(const ID: TGUID): IGlobalServiceV2; overload;
  end;

  IGlobalListV2 = interface
    ['{EB900CC4-356C-40F9-80E7-659BA71225FC}']
    function GetCount: Integer;
    function GetHook: Pointer;
    function GetOwner: IGlobalServiceListV2;
    procedure Clear;
    function Enumerate(var Enum: IEnumerator; out Item: IGlobalServiceV2): Boolean;
    function Get(const ID: TGUID): IGlobalServiceV2; overload;
    function Get(Index: Integer): IGlobalServiceV2; overload;
    function Get(Service: PGlobalService; Module: LongWord): IGlobalServiceV2; overload;
    function Find(const ID: TGUID; out Item: IGlobalServiceV2): Boolean; overload;
    function Find(Service: PGlobalService; out Item: IGlobalServiceV2): Boolean; overload;
    function Add(Service: PGlobalService; Module: LongWord): IGlobalServiceV2;
    function Remove(const ID: TGUID; Module: LongWord): Integer; overload;
    function Remove(Service: PGlobalService; Module: LongWord): Integer; overload;
    function Remove(const Item: IGlobalServiceV2; Module: LongWord): Integer; overload;
    function Remove(Module: LongWord): Integer; overload; 
    property Owner: IGlobalServiceListV2 read GetOwner;
    property Hook: Pointer read GetHook;
    property Count: Integer read GetCount;
  end;

  IGlobalServiceV2 = interface
    ['{84AC1573-BEA2-4001-BB77-733FDE2782A7}']
    function GetList: IGlobalListV2;
    function GetService: PGlobalService;
    function GetKind: TGlobalServiceKind;
    function GetIsCreated: Boolean;
    function GetInstance: IUnknown;
    procedure SetInstance(const Value: IUnknown);
    function GetReferences: IGlobalReferencesV2;
    function GetLinks: IGlobalLinksV2;
    function AddRef(const IID: TGuid; Obj: PUnknown; Module: LongWord): Boolean; 
    function DropRef(Obj: PUnknown; Module: LongWord): Boolean;
    property List: IGlobalListV2 read GetList;
    property Service: PGlobalService read GetService;
    property Kind: TGlobalServiceKind read GetKind;
    property IsCreated: Boolean read GetIsCreated;
    property Instance: IUnknown read GetInstance write SetInstance;
    property References: IGlobalReferencesV2 read GetReferences;
    property Links: IGlobalLinksV2 read GetLinks;
  end;

  IGlobalReferencesV2 = interface
    ['{1BEB0849-DB02-41BB-8B33-462CDC2F2FCD}']
    function GetCount: Integer;
    procedure Clear;
    function Enumerate(var Enum: IEnumerator; out Item: RGlobalRef): Boolean;
    procedure Add(const Ref: PUnknown; Module: LongWord);
    function Remove(const Ref: PUnknown; Module: LongWord): Boolean; overload;
    function Remove(Module: LongWord): Boolean; overload;
    property Count: Integer read GetCount;
  end;

  IGlobalLinksV2 = interface
    ['{7FBC38C1-2106-4654-9E2D-F57B51F4891C}']
    function GetCount: Integer;
    procedure Clear;
    function Enumerate(var Enum: IEnumerator; out Item: IGlobalServiceV2): Boolean;
    procedure Add(const Item: IGlobalServiceV2);
    procedure Remove(const Item: IGlobalServiceV2);
    property Count: Integer read GetCount;
  end;

  IGlobalServicesHookV2 = interface
    ['{78A286A6-8960-4687-988A-984F0C837647}']
    procedure ServiceInitialize(const Service: IGlobalServiceV2);
    procedure ServiceFinalize(const Service: IGlobalServiceV2);
    procedure ServiceCreate(const Service: IGlobalServiceV2);
    procedure ServiceDestroy(const Service: IGlobalServiceV2);
    procedure ServiceAddRef(const Service: IGlobalServiceV2; Ref: PUnknown);
    procedure ServiceDropRef(const Service: IGlobalServiceV2; Ref: PUnknown);
    procedure ServiceLink(const Service: IGlobalServiceV2; const Link: IGlobalServiceV2);
    procedure ServiceUnlink(const Service: IGlobalServiceV2; const Link: IGlobalServiceV2);
  end;

implementation
end.
