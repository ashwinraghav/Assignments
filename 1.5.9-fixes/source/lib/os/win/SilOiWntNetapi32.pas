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

unit SilOiWntNetapi32;

{$I Defines.inc}

interface

uses
  SilOhWntNetapi32;

type
  INetapi32 = interface
    ['{50B1D346-87DD-4DC4-B8E7-B5D8301EB031}']
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
    function DoNetUserModalsGet: TNetUserModalsGet;
    function DoNetMessageBufferSend: TNetMessageBufferSend;
    property NetUserGetInfo: TNetUserGetInfo read DoNetUserGetInfo;
    property NetUserEnum: TNetUserEnum read DoNetUserEnum;
    property NetServerEnum: TNetServerEnum read DoNetServerEnum;
    property NetGetDCName: TNetGetDCName read DoNetGetDCName;
    property DsGetDCName: TNetDsGetDCName read DoNetDsGetDCName;
    property NetApiBufferFree: TNetApiBufferFree read DoNetApiBufferFree;
    property NetUserChangePassword: TNetUserChangePassword read DoNetUserChangePassword;
    property NetGetDisplayInformationIndex: TNetGetDisplayInformationIndex read DoNetGetDisplayInformationIndex;
    property NetQueryDisplayInformation: TNetQueryDisplayInformation read DoNetQueryDisplayInformation;
    property NetGetJoinInformation: TNetGetJoinInformation read DoNetGetJoinInformation;
    property NetServerGetInfo: TNetServerGetInfo read DoNetServerGetInfo;
    property NetUseAdd: TNetUseAdd read DoNetUseAdd;
    property NetUseDel: TNetUseDel read DoNetUseDel;
    property NetUseEnum: TNetUseEnum read DoNetUseEnum;
    property Netbios: TNetbiosCall read DoNetbios;
    property NetMessageBufferSend: TNetMessageBufferSend read DoNetMessageBufferSend;
    property NetUserModalsGet: TNetUserModalsGet read DoNetUserModalsGet;
  end;

implementation
end.
