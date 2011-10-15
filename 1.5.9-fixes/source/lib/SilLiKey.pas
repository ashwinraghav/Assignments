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

unit SilLiKey;

{$I Defines.inc}

interface

uses
  SilBeTypes,
  SilLiEnumerator,
  SilLiStringList,
  SilLiLock;

type
  TNamedKeyNotificationFilter = (knChangeName, knChangeAttributes, knChangeValues, knChangeSecurity);
  TNamedKeyNotificationFilters = set of TNamedKeyNotificationFilter;
  TNamedKeyPermision = (kpRead, kpWrite, kpReadWrite);
  TNamedKeyDataType = (kdUnknown, kdString, kdInteger, kdStrings, kdBinary, kdLargeInt);

const
  knChangeAny = [Low(TNamedKeyNotificationFilter) .. High(TNamedKeyNotificationFilter)];
  
type
  INamedItems = interface;
  INamedItem = interface;

  INamedKeys = interface;
  INamedKey = interface;

  INamedValues = interface;

  INamedItems = interface
    ['{F1BAB989-4C37-11D4-9889-00104B0FA1EF}']
    function Locked: ILock;
    function Remove(const Name: String): Boolean;
    function Enumerate(var Enum: IEnumerator; out Name: string): Boolean;
  end;

  INamedKeys = interface (INamedItems)
    ['{F1BAB983-4C37-11D4-9889-00104B0FA1EF}']
    function Add(const Name: String; Permision: TNamedKeyPermision = kpReadWrite): INamedKey; 
    function Exists(const Name: String): Boolean;
    function Find(const Name: String; out Key: INamedKey): Boolean;
    function Get(const Name: String; CanCreate: Boolean = false): INamedKey; overload;
    function Get(const Name: String; Permision: TNamedKeyPermision; CanCreate: Boolean = false): INamedKey; overload;
  end;

  INamedValues = interface (INamedItems)
    ['{F1BAB984-4C37-11D4-9889-00104B0FA1EF}']
    function Exists(const Name: String): Boolean;
    function GetDataType(const Name: String): TNamedKeyDataType;
    function GetDataSize(const Name: String): LongWord;
    function ReadString(const Name: String; const Default: String = ''; CanCreate: Boolean = false): String;
    function ReadInteger(const Name: String; const Default: Integer = 0; CanCreate: Boolean = false): Integer;
    function ReadLargeInt(const Name: String; const Default: LargeInt = 0; CanCreate: Boolean = false): LargeInt;
    function ReadBoolean(const Name: String; const Default: Boolean = false; CanCreate: Boolean = false): Boolean;
    function ReadFloat(const Name: String; const Default: Double = 0; CanCreate: Boolean = false): Double;
    function ReadStrings(const Name: String; const Default: IStringList = nil; CanCreate: Boolean = false): IStringList;
    function WriteString(const Name: String; const Value: String): Boolean;
    function WriteInteger(const Name: String; Value: Integer): Boolean;
    function WriteLargeInt(const Name: String; Value: LargeInt): Boolean;
    function WriteBoolean(const Name: String; Value: Boolean): Boolean;
    function WriteFloat(const Name: String; Value: Double): Boolean;
    function WriteStrings(const Name: String; const Value: IStringList): Boolean;
  end;

  INamedItem = interface
    ['{F1BAB986-4C37-11D4-9889-00104B0FA1EF}']
    function GetName: String;
    procedure SetName(const Value: String);
    property Name: String read GetName write SetName;
  end;

  IEvNamedKeyChanged = interface
    ['{949BC19F-8463-4433-B5C3-DBBE2DA6B57E}']
    procedure OnChanged(const Key: INamedKey);
  end;

  INamedKey = interface(INamedItem)
    ['{F1BAB981-4C37-11D4-9889-00104B0FA1EF}']
    function GetKeys: INamedKeys;
    function GetValues: INamedValues;
    function GetIsNew: Boolean;
    function Notify(const Sink: IUnknown; const Filters: TNamedKeyNotificationFilters = knChangeAny; const Subtree: Boolean = True; KeepRef: Boolean = True): INamedKey;
    procedure Release;
    function GetPath: string;
    function GetParent: INamedKey;
    property Parent: INamedKey read GetParent;
    property Path: string read GetPath;
    property Keys: INamedKeys read GetKeys;
    property Values: INamedValues read GetValues;
    property IsNew: Boolean read GetIsNew;
  end;

  IValueKeys = interface (INamedItems)
    ['{F1BAB983-4C37-11D4-9889-00104B0FA1EF}']
    function Add(const Name: String): INamedValues;
    function Exists(const Name: String): Boolean;
    function Find(const Name: String; out Values: INamedValues): Boolean;
    function Get(const Name: String; CanCreate: Boolean = false): INamedValues;
  end;

  IValueKey = interface(INamedItem)
    ['{6FAD9621-7470-11D4-9894-00104B0FA1EF}']
    function GetValues: INamedValues;
    property Values: INamedValues read GetValues;
  end;

implementation

end.
