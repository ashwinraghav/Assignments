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

unit SilOmComputer;

{$I Defines.inc}

interface

uses
  SilBeTypes,
  SilLiInterfaceList,
  SilLkObject,
  SilOiVersion,
  SilOiComputer,
  (*)SilOsWntNetapi32,(*)
  SilOkComputer;

type
  TSilLinuxComputer = class;
  TSilLinuxDomain = class;
  
  TSilLinuxComputer = class(TSilComputer)
  private
    (*)FNetapiDll: INetapi32;
    FName: WideString;
    FDomain: IComputerDomain;
    FServer: PNetServerInfo101;
    FMacs: IInterfaceList;(*)
  private
    procedure DoGetServerInfo;
    procedure DoGetDomainInfo;
    //procedure DoGetDisplayInfo;
    procedure DoGetMacsInfo;
  protected
    function GetName: string; override;
    function GetDomain: IComputerDomain; override;
    function GetDescription: string; override;
    function GetKinds: TComputerKinds; override;
    function GetOS: IVersionInfo; override;
    function GetMACs: IInterfaceList; override;
  public
    constructor Create; override;
    constructor Create(const Name: string); override;
    destructor Destroy; override;
  end;

  TSilLinuxLocalComputer = class(
    TSilLinuxComputer,
    ILocalComputer )
  protected // ILocalComputer
    function UserName(Format: TNameFormat = nfUserPrincipal): string;
  public
  end;

  TSilLinuxRemoteComputer = class(TSilLinuxComputer)
  end;

  TSilLinuxDomain = class(
    TSilObject,
    IComputerDomain )
  private
    FName: WideString;
    FPrimary: WideString;
    (*)FJoin: TNetJoinStatus;(*)
  protected
    function GetName: string;
    function GetKind: TDomainKind;
    function GetDescription: string;
    function GetMainServer: string;
  public
    constructor Create((*)const DLL: INetapi32; (*)const Computer: string);
  end;

  TSilLinuxNetworkCard = class(
    TSilObject,
    IComputerNetworkCard )
  private
    FLana: Integer;
    (*)FInfo: TNetAdapterStatus;(*)
  protected // IComputerNetworkCard
    function GetMacAddress: LargeInt;
  public
    constructor Create(Lana: Integer(*); const Info: TNetAdapterStatus(*));
  end;

implementation

uses
  SysUtils,

  SilLtList,
  SilOfComputer;

{ TSilLinuxComputer }

constructor TSilLinuxComputer.Create;
begin
  raise Exception.CreateFmt('%s: not implemented', ['TSilLinuxComputer.Create']);
(*)
  Create(SilOfComputer.DoGetComputerName());
(*)end;

constructor TSilLinuxComputer.Create(const Name: string);
begin
  raise Exception.CreateFmt('%s: not implemented', ['TSilLinuxComputer.Create']);
(*)
  inherited Create;
  FNetapiDll := SilOsWntNetapi32.Netapi.Create();
  FName := Name;
(*)end;

destructor TSilLinuxComputer.Destroy;
begin(*)
  if Assigned(FServer) then Netapi.Free(FNetapiDLL, FServer);
  FNetapiDll := nil;
  inherited;
(*)end;

function TSilLinuxComputer.GetName: string;
begin(*)
  Result := FName;
(*)end;

function TSilLinuxComputer.GetDescription: string;
begin(*)
  DoGetServerInfo;
  Result := FServer.sv101_comment;
(*)end;

function TSilLinuxComputer.GetDomain: IComputerDomain;
begin(*)
  DoGetDomainInfo;
  Result := FDomain;
(*)end;

function TSilLinuxComputer.GetKinds: TComputerKinds;
begin(*)
  DoGetServerInfo;
  Result := DoServerTypeToComputerKind(FServer.sv101_type);
(*)end;

function TSilLinuxComputer.GetOS: IVersionInfo;
begin

end;

function TSilLinuxComputer.GetMACs: IInterfaceList;
begin(*)
  DoGetMacsInfo;
  Result := FMacs;
(*)end;

procedure TSilLinuxComputer.DoGetServerInfo;
begin(*)
  if not Assigned(FServer) then
    FServer := Netapi.ServerGetInfo(FNetapiDll, FName, 101);
(*)end;

(*)procedure TSilLinuxComputer.DoGetDisplayInfo;
var
  Index, Count: Integer;
begin
  DoGetDomainInfo;
  if not Assigned(FDisplay) then
  begin
    Index := Netapi.QueryIndex(FNetapiDll, FDomain.MainServer, FName, NET_QUERY_SERVERS);
    FDisplay := Netapi.QueryInfo(FNetapiDll, FDomain.MainServer, Index, NET_QUERY_SERVERS, Count);
  end;
end;(*)

procedure TSilLinuxComputer.DoGetDomainInfo;
begin(*)
  if not Assigned(FDomain) then
    FDomain := TSilLinuxDomain.Create(FNetapiDll, FName);
(*)end;

procedure TSilLinuxComputer.DoGetMacsInfo;
(*)var
  Lana: Integer;
  Lanas: TNetAdapters;
  Adapter: TNetAdapterStatus;(*)
begin(*)
  if not Assigned(FMacs) then
  begin
    FMacs := ListTool.InterfaceList();
    Netapi.NbLanaEnum(FNetapiDll, Lanas);
    for Lana := Low(Lanas) to High(Lanas) do
    begin
      Netapi.NbReset(FNetapiDll, Lana);
      if Netapi.NbAdapterStatus(FNetapiDll, Lanas[Lana], Adapter) = NRC_GOODRET then
        FMacs.Add(TSilLinuxNetworkCard.Create(Lana, Adapter) as IComputerNetworkCard);
    end;
  end;
(*)end;

{ TSilLinuxLocalComputer }

function TSilLinuxLocalComputer.UserName(Format: TNameFormat = nfUserPrincipal): string;
begin(*)
  Result := SilOfComputer.DoGetUserName();
(*)end;

{ TSilLinuxDomain }

constructor TSilLinuxDomain.Create((*)const DLL: INetapi32; (*)const Computer: string);
//var
//  Info: PNetD
begin
  raise Exception.CreateFmt('%s: not implemented', ['TSilLinuxDomain.Create']);
  (*)
  inherited Create;
  FJoin := Netapi.GetJoinInformation(DLL, Computer, FName);
//  FPrimary := Netapi.GetDomainController(DLL, Computer, FName);
(*)end;

function TSilLinuxDomain.GetName: string;
begin(*)
  Result := FName;
(*)end;

function TSilLinuxDomain.GetMainServer: string;
begin(*)
  Result := FPrimary;
(*)end;

function TSilLinuxDomain.GetDescription: string;
begin

end;

function TSilLinuxDomain.GetKind: TDomainKind;
begin(*)
  case FJoin of
    NET_JOIN_UNJOINED:      Result := dkStandalone;
    NET_JOIN_WORKGROUPNAME: Result := dkWorkgroup;
    NET_JOIN_DOMAINNAME:    Result := dkDomain;
    else                    Result := dkUnknown;
  end;
(*)end;

{ TSilLinuxNetworkCard }

type
  TMacAddress = array[0 .. 7] of Byte;
  PMacAddress = ^TMacAddress;

constructor TSilLinuxNetworkCard.Create(Lana: Integer(*); const Info: TNetAdapterStatus(*));
begin
  raise Exception.CreateFmt('%s: not implemented', ['TSilLinuxNetworkCard.Create']);
(*)
  inherited Create;
  FLana := Lana;
  FInfo := Info;
(*)end;

function TSilLinuxNetworkCard.GetMacAddress: LargeInt;
var
  P: PMacAddress;
begin(*)
  P := @Result;
  P^[0] := FInfo.adapter_address[0];
  P^[1] := FInfo.adapter_address[1];
  P^[2] := FInfo.adapter_address[2];
  P^[3] := FInfo.adapter_address[3];
  P^[4] := FInfo.adapter_address[4];
  P^[5] := FInfo.adapter_address[5];
  P^[6] := 0;
  P^[7] := 0;
(*)end;

end.
