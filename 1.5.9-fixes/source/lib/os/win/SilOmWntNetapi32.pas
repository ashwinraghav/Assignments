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

unit SilOmWntNetapi32;

{$I Defines.inc}

interface

uses
  SilLkObject,
  SilOiSharedLibrary,
  SilOhWntNetapi32,
  SilOiWntNetapi32;

type
  TNetapi32 = class (
    // extends
    TSilObject,
    // implements
    INetapi32 )
  private
    FLib: ISharedLibrary;
  protected // IKernel32
    function DoNetUserGetInfo: TNetUserGetInfo;
    function DoNetUserEnum: TNetUserEnum;
    function DoNetServerEnum: TNetServerEnum;
    function DoNetGetDCName: TNetGetDCName;
    function DoNetDsGetDCName: TNetDsGetDCName;
    function DoNetApiBufferFree: TNetApiBufferFree;
    function DoNetUserChangePassword: TNetUserChangePassword;
    function DoNetGetDisplayInformationIndex: TNetGetDisplayInformationIndex;
    function DoNetQueryDisplayInformation: TNetQueryDisplayInformation;
    function DoNetGetJoinInformation: TNetGetJoinInformation;
    function DoNetServerGetInfo: TNetServerGetInfo;
    function DoNetUseAdd: TNetUseAdd;
    function DoNetUseDel: TNetUseDel;
    function DoNetUseEnum: TNetUseEnum;
    function DoNetbios: TNetbiosCall;
    function DoNetMessageBufferSend: TNetMessageBufferSend;
    function DoNetUserModalsGet: TNetUserModalsGet;
  public
    constructor Create(const FileName: String);
  end;

implementation

uses
  SilOtTool;

type
  TNetapiEntryPoint = (
      ixNetUserGetInfo,
      ixNetUserEnum,
      ixNetServerEnum,
      ixNetGetDCName,
      ixNetDsGetDCName,
      ixNetApiBufferFree,
      ixNetUserChangePassword,
      ixNetGetDisplayInformationIndex,
      ixNetQueryDisplayInformation,
      ixNetGetJoinInformation,
      ixNetServerGetInfo,
      ixNetUseAdd,
      ixNetUseDel,
      ixNetUseEnum,
      ixNetbios,
      ixNetMessageBufferSend,
      ixNetUserModalsGet
    );

{ TNetapi32 }

constructor TNetapi32.Create(const FileName: String);
begin
  inherited Create;
  FLib := OS.SharedLibrary.Load(FileName);
end;

function TNetapi32.DoNetApiBufferFree: TNetApiBufferFree;
begin
  FLib.Bind('NetApiBufferFree', Ord(ixNetApiBufferFree), Result, true);
end;

function TNetapi32.DoNetGetDCName: TNetGetDCName;
begin
  FLib.Bind('NetGetDCName', Ord(ixNetGetDCName), Result, true);
end;

function TNetapi32.DoNetDsGetDCName: TNetDsGetDCName;
begin
  FLib.Bind('DsGetDcNameW', Ord(ixNetDsGetDCName), Result, true);
end;

function TNetapi32.DoNetServerEnum: TNetServerEnum;
begin
  FLib.Bind('NetServerEnum', Ord(ixNetServerEnum), Result, true);
end;

function TNetapi32.DoNetUserChangePassword: TNetUserChangePassword;
begin
  FLib.Bind('NetUserChangePassword', Ord(ixNetUserChangePassword), Result, true);
end;

function TNetapi32.DoNetUserEnum: TNetUserEnum;
begin
  FLib.Bind('NetUserEnum', Ord(ixNetUserEnum), Result, true);
end;

function TNetapi32.DoNetUserGetInfo: TNetUserGetInfo;
begin
  FLib.Bind('NetUserGetInfo', Ord(ixNetUserGetInfo), Result, true);
end;

function TNetapi32.DoNetGetDisplayInformationIndex: TNetGetDisplayInformationIndex;
begin
  FLib.Bind('NetGetDisplayInformationIndex', Ord(ixNetGetDisplayInformationIndex), Result, true);
end;

function TNetapi32.DoNetQueryDisplayInformation: TNetQueryDisplayInformation;
begin
  FLib.Bind('NetQueryDisplayInformation', Ord(ixNetQueryDisplayInformation), Result, true);
end;

function TNetapi32.DoNetGetJoinInformation: TNetGetJoinInformation;
begin
  FLib.Bind('NetGetJoinInformation', Ord(ixNetGetJoinInformation), Result, true);
end;

function TNetapi32.DoNetServerGetInfo: TNetServerGetInfo;
begin
  FLib.Bind('NetServerGetInfo', Ord(ixNetServerGetInfo), Result, true);
end;

function TNetapi32.DoNetUseAdd: TNetUseAdd;
begin
  FLib.Bind('NetUseAdd', Ord(ixNetUseAdd), Result, true);
end;

function TNetapi32.DoNetUseDel: TNetUseDel;
begin
  FLib.Bind('NetUseDel', Ord(ixNetUseDel), Result, true);
end;

function TNetapi32.DoNetUseEnum: TNetUseEnum;
begin
  FLib.Bind('NetUseEnum', Ord(ixNetUseEnum), Result, true);
end;

function TNetapi32.DoNetbios: TNetbiosCall;
begin
  FLib.Bind('Netbios', Ord(ixNetbios), Result, true);
end;

function TNetapi32.DoNetMessageBufferSend: TNetMessageBufferSend;
begin
  FLib.Bind('NetMessageBufferSend', Ord(ixNetbios), Result, true);
end;

function TNetapi32.DoNetUserModalsGet: TNetUserModalsGet;
begin
	FLib.Bind('NetUserModalsGet', Ord(ixNetUserModalsGet), Result, true);
end;

end.
