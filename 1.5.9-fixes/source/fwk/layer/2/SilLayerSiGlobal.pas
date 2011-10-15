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

unit SilLayerSiGlobal;

interface

{$include Sil.inc}

uses
  Sil,
  SilLjFiler;

type
  TLayerLogLevel = (slAlways, slStatus, slAction);
  TLayerLogLevelSet = set of TLayerLogLevel;

  TLayerKind = (lkUnspecified, lkDevice, lkPacker, lkProtocol, lkStack);

  ILayer = interface;
  ILayerStack = interface;
  ILayerStatus = interface;
  ILayerAction = interface;
  ILayerHook = interface;
  ILayerToken = interface;
  ILayerWait = interface;
  ILayerClientHolder = interface;

  ILayer = interface
    ['{8551CBDB-3470-44F7-8BF5-ED1211418EBD}']
    function GetAction: ILayerAction;
    function GetStack: ILayerStack;
    function GetStatus: ILayerStatus;
    function GetKind: TLayerKind;
    function GetLower: ILayer;
    function GetUid: TGuid;
    function GetUpper: ILayer;
    procedure SetStack(const Value: ILayerStack);
    procedure SetLower(const Value: ILayer);
    procedure SetUpper(const Value: ILayer);
    property Stack: ILayerStack read GetStack write SetStack;
    property Lower: ILayer read GetLower write SetLower;
    property Upper: ILayer read GetUpper write SetUpper;
    property Action: ILayerAction read GetAction;
    property Status: ILayerStatus read GetStatus;
    property Kind: TLayerKind read GetKind;
    property Uid: TGuid read GetUid;
  end;

  ILayerStack = interface
    ['{F0379FC0-7731-4294-8C3E-F22E97C2E237}']
    function GetAction: ILayerAction;
    function GetStatus: ILayerStatus;
    procedure Add(const Value: ILayer);
    procedure Remove(const Value: ILayer);
    procedure Insert(Index: Integer; const Value: ILayer);
    procedure Clear;
    function GetToken: ILayerToken;
    //function Enumerate(var Enum: IEnumerator; out Item: ILayer): Boolean;
    function GetLowerLink(out Item: ILayer; const Ref: ILayer = nil): Boolean;
    function GetUpperLink(out Item: ILayer; const Ref: ILayer = nil): Boolean;
    property Action: ILayerAction read GetAction;
    property Status: ILayerStatus read GetStatus;
  end;

  ILayerStatus = interface
    ['{66DB45BF-61EA-4914-9A6C-4BF85079B87C}']
    function GetIsActive: Boolean;
    function GetParams: IParameterList;
    function GetLogMask: TLayerLogLevelSet;
    procedure SetLogMask(value: TLayerLogLevelSet);
    procedure Activate(const Context: IUnknown = nil);
    procedure Deactivate(const Context: IUnknown = nil; Manual: Boolean = true);
    procedure Finalize;
    property IsActive: Boolean read GetIsActive;
    property Params: IParameterList read GetParams;
    property LogMask: TLayerLogLevelSet read GetLogMask write SetLogMask;
  end;

  ILayerAction = interface
    ['{72E33943-7287-42B0-B55E-2117345B1E5F}']
    procedure Write(const Packet: ILayerToken);
    procedure Read(const Packet: ILayerToken);
  end;

  ILayerHook = interface
    ['{B0C05B0A-F862-43F0-BD5C-05F046B4AC2D}']
    procedure Status(const Link: ILayer; const Context: IUnknown);
    procedure Error(const Link: ILayer; const Context: IUnknown);
  end;

  ILayerToken = interface
    ['{9FA4D646-9781-4FEA-B55C-36B6C0E3040A}']
    function GetParams: IParameterList;
    function GetBuffer: IMemoryStream;
    function Reader: IReader;
    function Writer: IWriter;
    procedure Configure(Filer: FilerFactoryType);
    procedure Clear;
    property Params: IParameterList read GetParams;
    property Buffer: IMemoryStream read GetBuffer;
  end;

  ILayerWait = interface
    ['{B0C3EEAD-9B4D-4D88-818D-705FC8842803}']
    procedure settoken(const token: ILayerToken);
    function gettoken: ILayerToken;
    function event: IEvent;
    function buffer: IMemoryStream;
    function timeout: longword;
  end;

  ILayerClientHolder = interface
    ['{4e563766-fd84-42e2-86cc-050ed176743a}']
    procedure AddClient(const stack: ILayerStack);
    procedure ReleaseClient(const stack: ILayerStack);
  end;

implementation

end.
