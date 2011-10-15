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

unit SilLiGlobalServicesV1;

{$I Defines.inc}

interface

uses
  SilBeTypes,
  SilBkTool,
  SilLiLock,
  SilLiEnumerator;

type
  PServiceRefV1 = ^RServiceRefV1;
  GlobalServiceType = class of GlobalService;

  RServiceRefV1 = record
    Service: GlobalServiceType;
    Ptr: PUnknown;
    ID: Pointer;
  end;

  GlobalService = class(Tool)
  protected
    class function Create: IUnknown; virtual; abstract;
  public
    class function ID: TGuid; virtual; abstract;
  end;

  IGlobalServicesV1 = interface
    ['{CDAA3E5F-B341-48E1-8A24-0397EB9933F2}']
    function Locked: ILock;
    procedure Register(Service: GlobalServiceType);
    procedure Unregister(Service: GlobalServiceType);
    function Find(Service: GlobalServiceType): Boolean; overload;
    function Find(Service: GlobalServiceType; const IID: TGuid; Obj: PUnknown; ID: Pointer = nil): Boolean; overload;
    procedure Add(Service: GlobalServiceType; const Instance: IUnknown; const IID: TGuid; Obj: PUnknown; ID: Pointer = nil);
    procedure Remove(Service: GlobalServiceType);
    procedure Get(Service: GlobalServiceType; const IID: TGuid; Obj: PUnknown; ID: Pointer = nil);
    procedure Release(Service: GlobalServiceType; Obj: PUnknown);
    function Enumerate(var Enum: IEnumerator; const IID: TGuid; out Obj): Boolean; overload;
    function Enumerate(var Enum: IEnumerator; out Obj: IUnknown): Boolean; overload;
    function Enumerate(var Enum: IEnumerator; out Obj: GlobalServiceType): Boolean; overload;
    function Enumerate(var Enum: IEnumerator; out Ref: RServiceRefV1): Boolean; overload;
  end;

implementation
end.
