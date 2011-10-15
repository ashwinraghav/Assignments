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

unit SilShSharedManager;

{$INCLUDE Defines.inc}

interface

uses
  Sil,
  SilSiSharedObject,
  SilShSharedObject;

type
  ISharedObjectManager = interface;
  ISharedObjectConfig = interface;
  ISharedObjectList = interface;
  ISharedObjectLoader = interface;
  ISharedObjectCache = interface;
  ISharedObjectFactory = interface;
  ISharedObjectContainer = interface;
  ISharedObjectProvider = interface;
  ISharedObjectWatchdog = interface;

  ISharedObjectManager = interface
    ['{7D967CAE-1BCF-44D8-9D6C-8B49365BC9DF}']
    function Locked: ILock;
    function GetConfig: ISharedObjectConfig;
    function GetList: ISharedObjectList;
    function GetLoader: ISharedObjectLoader;
    function GetCache: ISharedObjectCache;
    function GetFactory: ISharedObjectFactory;
    function GetWatchdog: ISharedObjectWatchdog;
    procedure Initialize(const Key: INamedKey);
    procedure Finalize;
    property Config: ISharedObjectConfig read GetConfig;
    property List: ISharedObjectList read GetList;
    property Loader: ISharedObjectLoader read GetLoader;
    property Cache: ISharedObjectCache read GetCache;
    property Factory: ISharedObjectFactory read GetFactory;
    property Watchdog: ISharedObjectWatchdog read GetWatchdog;
  end;

  ISharedObjectConfig = interface 
    ['{E68FE75D-AC4C-4BFE-BC04-93A36C03B427}']
    function GetPath: IStrings;
    function GetMask: IStrings;
    procedure AddPath(const Value: string);
    procedure AddMask(const Value: string);
    procedure SetPath(const Value: string);
    procedure SetMask(const Value: string);
    property Path: IStrings read GetPath;
    property Mask: IStrings read GetMask;
  end;

  ISharedObjectCache = interface
    ['{52D90013-7C53-461A-B7CA-D56CC3534D7D}']
    function Lookup(const ID: ISharedID; out Provider: ISharedObjectProvider): Boolean;
    function Add(const Provider: ISharedObjectProvider; const ID: ISharedID = nil): Boolean;
    function Remove(const Provider: ISharedObjectProvider): Boolean; overload;
    function Remove(const ID: ISharedID): Boolean; overload;
  end;

  ISharedObjectList = interface
    ['{9E1A3D5E-B973-4087-ADC5-E0DE5A6784F2}']
    function GetCount: Integer;
    function Enumerate(var Enum: IEnumerator; out Item: ISharedObjectContainer): Boolean;
    function Find(const ID: ISharedID; out Container: ISharedObjectContainer): Boolean; overload; 
    function Find(const Provider: ISharedObjectProvider; out Container: ISharedObjectContainer): Boolean; overload;
    function Add(const Provider: ISharedObjectProvider): ISharedObjectContainer;
    function Remove(const Provider: ISharedObjectProvider): Integer; overload;  
    function Remove(const Container: ISharedObjectContainer): Integer; overload; 
    property Count: Integer read GetCount;
  end;

  ISharedObjectLoader = interface
    ['{645F49C7-7BE9-4C4F-8118-C4196636221E}']
    function Get(const FileName: string): ISharedObjectProvider; overload; 
    function Get(const ID: ISharedID): ISharedObjectProvider; overload;
    function Search(const ID: ISharedID; out Provider: ISharedObjectProvider): Boolean;
  end;

  ISharedObjectFactory = interface
    ['{91E29A16-053A-4357-9DBD-8111F76E5F16}']
    function CreateInstance(const ClassID: TGuid; Param: Pointer = nil; const Owner: IUnknown = nil; const Controller: IUnknown = nil): IUnknown; overload;
    function CreateInstance(const FileName: String; const ClassID: TGuid; Param: Pointer = nil; const Owner: IUnknown = nil; const Controller: IUnknown = nil): IUnknown; overload;
    function CreateInstance(const FileName: String; const Name: string; Param: Pointer = nil; const Owner: IUnknown = nil; const Controller: IUnknown = nil): IUnknown; overload;
    function CreateInstance(const Name: string; Param: Pointer = nil; const Owner: IUnknown = nil; const Controller: IUnknown = nil): IUnknown; overload;
    function CreateTool(const FileName: String; const ClassID: TGuid): TClass; overload;
    function CreateTool(const ClassID: TGuid): TClass; overload;
    function CreateTool(const Name: String): TClass; overload;
    procedure ReleaseInstance(var Obj);
    procedure ReleaseTool(var Obj);
  end;
  
  ISharedObjectProxy = interface
    ['{0393F44C-84BA-42AF-84FB-33133FD00E2A}']
    function GetContainer: ISharedObjectContainer;
    function GetInstance: IUnknown;
    procedure SetInstance(const Value: IUnknown);
    property Container: ISharedObjectContainer read GetContainer;
    property Instance: IUnknown read GetInstance write SetInstance;
  end;

  ISharedObjectContainer = interface
    ['{B3553F73-5F37-4EEC-8075-9ED30C35EF6B}']
    function GetProvider: ISharedObjectProvider;
    function GetReferences: ISharedObjectReferences;
    property Provider: ISharedObjectProvider read GetProvider;
    property References: ISharedObjectReferences read GetReferences;
  end;

  //ISharedObject

  ISharedObjectProvider = interface
    ['{4963250E-4480-4263-A500-B64322A2FB39}']
    function GetModule: IModule2;
    function Provides(const ID: ISharedID): Boolean;
    function Get(const ID: ISharedID): ISharedFactory;
    function Find(const ID: ISharedID; out Factory: ISharedFactory): Boolean;
    function Enumerate(var Enum: IEnumerator; out Factory: ISharedFactory): Boolean;
    property Module: IModule2 read GetModule;
  end;

  ISharedObjectWatchdog = interface
    ['{4E753E97-5E55-4B96-88AC-872835BCD5A5}']
    function GetThread: IThread;
    procedure Initialize(const Key: INamedKey);
    procedure Finalize;
    procedure Post(const Action: IAction); 
    property Thread: IThread read GetThread;
  end;

implementation
end.
