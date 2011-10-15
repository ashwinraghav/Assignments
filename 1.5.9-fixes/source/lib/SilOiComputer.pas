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

unit SilOiComputer;

{$I Defines.inc}

interface

uses
  SilBeTypes,
  SilLiInterfaceList,
  SilOiVersion;

type
  IComputer = interface;
  IComputerDomain = interface;
  IComputerNetworkCard = interface;

  TComputerKind = (
      ckPersonal,
      ckWorkstation,
      ckStandaloneServer,
      ckDomainServer,
      ckTimeServer,
      ckDatabaseServer,
      ckServerCluster,
      ckVirtualCluster,
      ckGraphicalTerminal
    );

  TComputerKinds = set of TComputerKind;

  TShutdownFlag = (
      sfLogOff,
      sfShutdown,
      sfReboot,
      sfForce,
      sfPowerOff
    );

  TShutdownFlags = set of TShutdownFlag;

  IComputer = interface
    ['{39F3E68C-B8C4-44E5-B65C-4BB542340B04}']
    function GetName: string;
    function GetDomain: IComputerDomain;
    function GetDescription: string;
    function GetKinds: TComputerKinds;
    function GetOS: IVersionInfo;
    function GetMACs: IInterfaceList;
    property Name: string read GetName;
    property Description: string read GetDescription;
    property Domain: IComputerDomain read GetDomain;
    property Kinds: TComputerKinds read GetKinds;
    property OS: IVersionInfo read GetOS;
    property MACs: IInterfaceList read GetMACs;
  end;

  TNameFormat = (
      nfUnknown,
      nfFullyQualifiedDN,
      nfSamCompatible,
      nfDisplay,
      nfUniqueId,
      nfCanonical,
      nfUserPrincipal,
      nfCanonicalEx,
      nfServicePrincipal
    );

  ILocalComputer = interface (IComputer)
    ['{AC371FF2-F36A-48F0-9441-2D9573F8B507}']
    function UserName(Format: TNameFormat = nfUserPrincipal): string;
  end;

  TDomainKind = (
      dkUnknown,
      dkStandalone,
      dkWorkgroup,
      dkDomain
    );

  IComputerDomain = interface
    ['{BF69FB05-1D2A-4E56-89E8-778CFDF1E55F}']
    function GetName: string;
    function GetKind: TDomainKind;
    function GetDescription: string;
    function GetMainServer: string;
    property Name: string read GetName;
    property Kind: TDomainKind read GetKind;
    property Description: string read GetDescription;
    property MainServer: string read GetMainServer;
  end;

  IComputerNetworkCard = interface
    ['{58414736-1E4D-4D22-BD83-2BBB8E228652}']
    function GetMacAddress: LargeInt;
    property MACAddress: LargeInt read GetMacAddress;    
  end;

implementation
end.
 