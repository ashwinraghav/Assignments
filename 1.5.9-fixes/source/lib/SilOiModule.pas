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

unit SilOiModule;

{$I Defines.inc}

interface

uses
  SilLiEnumerator,
  SilOiHandle,
  SilOiFile,
  SilOiVersion;

type
  (*)IModule = interface
    ['{D733B261-9EDC-11D4-ADA6-00902794F778}']
    function GetName: string;
    function GetPath: string;
    function GetVersion: IVersionInfo;
    function GetHandle: IHandle;
    function GetFullName: string;
    function GetPathName: string;
    property Handle: IHandle read GetHandle;
    property Name: string read GetName;
    property Path: string read GetPath;
    property Version: IVersionInfo read GetVersion;
    property FullName: string read GetFullName;
    property PathName: string read GetPathName;
  end;(*)

  IModule = interface
    ['{D3C341E7-918E-4123-BEFF-07FA3A16B479}']
    function GetHandle: IHandle;
    function GetInfo: IFileInfo;
    function GetFullName: string;
    property Handle: IHandle read GetHandle;
    property FullName: string read GetFullName;
    property Info: IFileInfo read GetInfo;
  end;

  IModule2 = IModule;

  IModules = interface
    ['{2E8C23A4-B8FB-4ECB-BC13-42BAE07ECB78}']
    function GetCount: Integer;
    function Enumerate(var Enum: IEnumerator; out Item: IModule2): Boolean;
    property Count: Integer read GetCount;
  end;                                       

  IModuleList = interface (IModules)
    ['{A358FAD7-3243-49A9-86ED-EBDC0473EAF3}']
    procedure Add(const Item: IModule2); 
  end;
  
implementation
end.
