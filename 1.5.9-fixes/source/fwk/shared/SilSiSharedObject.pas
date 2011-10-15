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

unit SilSiSharedObject;

{$INCLUDE Defines.inc}

interface

uses
  SilBkTool,
  SilLiEnumerator,
  SilLiLock,
  SilOiModule;

type
  ISharedObject = interface
    ['{EF24914B-D36E-4CAE-8D40-4B713E0B07C9}']
    function GetClassID: TGUID; 
    function GetModule: IModule2;
    property ClassID: TGUID read GetClassID;
    property Module: IModule2 read GetModule;
  end;

  TSharedID = (idName, idClass, idModule);

  ISharedClassID = interface;
  ISharedProgID = interface;
  ISharedModuleID = interface;

  ISharedID = interface
    ['{40218D05-1223-486E-BB18-AADE1B128590}']
    function GetKind: TSharedID;
    function GetValue: string;
    function GetAsName: ISharedProgID;
    function GetAsClass: ISharedClassID;
    function GetAsModule: ISharedModuleID;
    property Kind: TSharedID read GetKind;
    property Value: string read GetValue;
    property AsName: ISharedProgID read GetAsName;
    property AsClass: ISharedClassID read GetAsClass;
    property AsModule: ISharedModuleID read GetAsModule;
  end;

  ISharedClassID = interface
    ['{1E244666-17A3-4044-B6F8-74DAF05C018C}']
    function GetClassID: TGUID;
    property ClassID: TGUID read GetClassID;
  end;

  ISharedProgID = interface
    ['{71A13405-24DF-45BB-A351-0275AD3AF367}']
    function GetProgID: string;
    property ProgID: string read GetProgID;
  end;
  
  ISharedModuleID = interface
    ['{6CE3E5A2-F50D-4A2C-BF7E-9A9999B9BD46}']
    function GetModuleID: string;
    property ModuleID: string read GetModuleID;
  end;
  
  ISharedFactory = interface (ISharedObject)
    ['{FB01508C-F924-4F8C-ACAF-46307AF6EB46}']
    function GetProgID: string;
    function CreateObject(const Owner: IUnknown; const IID: TGuid; out Instance; const Controller: IUnknown = nil; Param: Pointer = nil): Boolean;
    property ProgID: string read GetProgID;
  end;

  ISharedClassFactory = interface (ISharedFactory)
    ['{84B70268-8D2E-4D49-A67A-F9D11BA830C6}']
    function GetClassType: TClass;
    property ClassType: TClass read GetClassType;
  end;

  ISharedFactoryList = interface
    ['{F47F422D-3262-48BC-AD68-52D431896DFA}']
    function GetCount: Integer;
    function Get(const ClassID: TGUID): ISharedFactory; overload;
    function Get(const ProgID: string): ISharedFactory; overload;
    function Find(const ClassID: TGUID; out Instance: ISharedFactory): Boolean; overload;
    function Find(const ProgID: string; out Instance: ISharedFactory): Boolean; overload;
    function Enumerate(var Enum: IEnumerator; out Item: ISharedFactory): Boolean; overload;
    function Enumerate(var Enum: IEnumerator; const IID: TGUID; out Item): Boolean; overload;
    property Count: Integer read GetCount;
  end;

  ISharedRegistry = interface (ISharedFactoryList)
    ['{E67FC2BC-7672-440B-995D-C65D42E7FAF5}']
    function Locked: ILock;
    function Add(const Factory: ISharedFactory): Integer;
    function Remove(const Factory: ISharedFactory): Integer;
  end;

  ClassFactoryType = class of ClassFactory;
  
  ClassFactory = class(Tool)
    class function Create(const Factory: ISharedFactory; ClassType: TClass; const Owner: IUnknown = nil; const Controller: IUnknown = nil; Param: Pointer = nil): IUnknown; virtual; abstract;     
  end;

implementation
end.
