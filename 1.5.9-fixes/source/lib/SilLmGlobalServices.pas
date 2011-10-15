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

unit SilLmGlobalServices;

{$I Defines.inc}

interface

uses
  SilBeTypes,
  SilLkObject,

  SilLiLock,
  SilLiEnumerator,
  SilLiConnection,
  SilLiStringList,
  SilLiGlobalServices,
  SilLkInterfaced;

type
  TSilServiceList = class (
    TSilObject,
    IGlobalServicesV1,
    IGlobalServicesV2,
    IGlobalServiceListV2,
    IConnectable )
  private
    FInstanceV1: IUnknown;
    FInstanceV2: IUnknown;
  private
    FModule: LongWord;
  private
    function DoGetGlobalServicesV1: IGlobalServicesV1;
    function DoGetGlobalServicesV2: IGlobalServiceListV2;
  protected // IGlobalServicesV1
    property GlobalServicesV1: IGlobalServicesV1 read DoGetGlobalServicesV1 implements IGlobalServicesV1;
  protected // IGlobalServicesV2, IGlobalServiceListV2
    property GlobalServicesV2: IGlobalServiceListV2 read DoGetGlobalServicesV2 implements IGlobalServicesV2, IGlobalServiceListV2;
  protected // IConnectable
    procedure AddListener(const Listener: IUnknown; KeepRef: Boolean);
    procedure RemoveListener(const Listener: IUnknown); 
  public
    constructor Create(Module: LongWord; Kind: TGlobalServiceKind);
    destructor Destroy; override;
  end;

implementation

uses
  SilLtConnection,
  SilLmGlobalServicesV1,
  SilLmGlobalServicesV2;

{ TSilServiceList }

constructor TSilServiceList.Create(Module: LongWord; Kind: TGlobalServiceKind);
begin
  inherited Create;
  FModule := Module;
  FInstanceV2 := TGlobalServiceListV2.Create(Self, Kind);
end;

destructor TSilServiceList.Destroy;
begin
  FInstanceV1 := nil;
  FInstanceV2 := nil;
  inherited;
end;

function TSilServiceList.DoGetGlobalServicesV1: IGlobalServicesV1;
begin
  if not Assigned(FInstanceV1) then
    FInstanceV1 := TGlobalServiceListV1.Create(Self);
  Result := FInstanceV1 as IGlobalServicesV1;
end;

function TSilServiceList.DoGetGlobalServicesV2: IGlobalServiceListV2;
begin
  Result := FInstanceV2 as IGlobalServiceListV2;
end;

procedure TSilServiceList.AddListener(const Listener: IInterface; KeepRef: Boolean);
begin
  if Assigned(FInstanceV1) then Sink.Connect(FInstanceV1, Listener, KeepRef);
  if Assigned(FInstanceV2) then Sink.Connect(FInstanceV2, Listener, KeepRef);
end;

procedure TSilServiceList.RemoveListener(const Listener: IInterface);
begin
  if Assigned(FInstanceV2) then Sink.Disconnect(FInstanceV2, Listener);
  if Assigned(FInstanceV1) then Sink.Disconnect(FInstanceV1, Listener);
end;

end.
