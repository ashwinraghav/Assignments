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

unit SilSiUrl;

interface

uses
  Sil;

type
  IUrl = interface;
  IUrlData = interface;
  IUrlProtocol = interface;
  IUrlAddress = interface;
  IUrlHost = interface;
  IUrlLogin = interface;
  IUrlPath = interface;
  IUrlParams = interface;
  IUrlQuery = interface;

  IUrlData = interface
    ['{B2D83E5E-9A11-4B74-8EEA-538707F9CE19}']
    function GetIsEmpty: Boolean;
    function GetIsAssigned: Boolean;
    function GetText: string;
    procedure SetText(const AUrl: string);
    procedure Clear;
    property IsEmpty: Boolean read GetIsEmpty;
    property IsAssigned: Boolean read GetIsAssigned;
    property Text: string read GetText write SetText;
  end;

  // Información contenida en una URL
  IUrl = interface (IUrlData)
    ['{506A1CD2-BA2C-11D4-B9DB-006008AE4EDF}']
    function GetProtocol: IUrlProtocol;
    function GetAddress: IUrlAddress;
    function GetPath: IUrlPath;
    function GetParams: IUrlParams;
    function GetQuery: IUrlQuery;
    property Protocol: IUrlProtocol read GetProtocol;
    property Address: IUrlAddress read GetAddress;
    property Path: IUrlPath read GetPath;
    property Params: IUrlParams read GetParams;
    property Query: IUrlQuery read GetQuery;
  end;

  IUrlProtocol = interface (IUrlData)
    ['{C5C94EE1-7D16-494D-81E9-2240FD627E3F}']
  end;

  IUrlAddress = interface (IUrlData)
    ['{801535BB-70BB-49B1-8BBF-96674E6E7563}']
    function GetHost: IUrlHost;
    function GetLogin: IUrlLogin;
    property Host: IUrlHost read GetHost;
    property Login: IUrlLogin read GetLogin;
  end;

  IUrlHost = interface (IUrlData)
    ['{2CEB586E-82CE-49BE-8F02-F51EB9773651}']
    function GetServer: string;
    procedure SetServer(const Value: string);
    function GetPort: string;
    procedure SetPort(Value: string);
    property Server: string read GetServer write SetServer;
    property Port: string read GetPort write SetPort;
  end;

  IUrlLogin = interface (IUrlData)
    ['{2F4D470E-E778-481F-B264-D76B21A5F985}']
    function GetUser: string;
    procedure SetUser(const Value: string);
    function GetPassword: string;
    procedure SetPassword(const Value: string);
    property User: string read GetUser write SetUser;
    property Password: string read GetPassword write SetPassword;
  end;

  IUrlPath = interface (IUrlData)
    ['{094033C5-B833-4031-B23D-0F404A790F99}']
    function GetCount: Integer;
    function GetIsRelative: Boolean;
    function GetIsAbsolute: Boolean;
    function GetFirst: string;
    function GetLast: string;
    function GetItem(Index: Integer): string;
    procedure SetItem(Index: Integer; const Value: string);
    function Enumerate(var Enum: IEnumerator; out Item: string): Boolean;
    function Add(const Item: string; Index: Integer = -1): Integer;
    function Remove(const Item: string): Integer;
    procedure Delete(Index: Integer);
    property Count: Integer read GetCount;
    property IsRelative: Boolean read GetIsRelative;
    property IsAbsolute: Boolean read GetIsAbsolute;
    property First: string read GetFirst;
    property Last: string read GetLast;
    property Item[Index: Integer]: string read GetItem write SetItem; default;
  end;

  IUrlItems = interface (IUrlData)
    ['{70CEF93F-C15E-4897-8223-8E4383C08299}']
    function GetList: IParameterList;
    procedure SetList(const Value: IParameterList);
    property List: IParameterList read GetList write SetList;
  end;    

  IUrlParams = interface (IUrlItems)
    ['{B7B9E2B9-4DA3-4EF8-A0E9-97FE9D12CC71}']
  end;

  IUrlQuery = interface (IUrlItems)
    ['{34A480C4-FC5B-434C-B0CA-FF08BEDCF47F}']
  end;

  // Url de una base de datos o tabla
  IUrlDB = interface( IUrl )
    ['{506A1CD3-BA2C-11D4-B9DB-006008AE4EDF}']
    function GetDatabase: string;
    function GetTable: string;
    procedure SetDatabase(const Value: string);
    procedure SetTable(const Value: string);
    property Database: string read GetDatabase write SetDatabase;
    property Table: string read GetTable write SetTable;
  end;

implementation
end.

