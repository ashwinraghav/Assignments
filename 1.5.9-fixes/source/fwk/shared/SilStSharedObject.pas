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

unit SilStSharedObject;

{$INCLUDE Defines.inc}

interface

uses
  SilBeTypes,
  SilBkTool,
  SilLiEnumerator,
  SilLiKey,
  SilLiPointerList,
  SilLkObject,
  SilOiSharedLibrary,
  SilSiSharedObject,
  SilSkSharedObject;

type
  SvcSharedObjectClass = class of SharedObject;

  SharedObject = class (Tool)
    class procedure Initialize(const Key: INamedKey); overload; 
    class procedure Initialize(const Key: string); overload;
    class procedure Finalize;
    class procedure SetPath(const Path: String);
    class procedure AddPath(const Path: String);
    class function GetPath: String;
    class procedure SetMask(const Mask: String);
    class function GetMask: String;
    class procedure AddMask(const Mask: String);
    class function CreateObject(const ClassID: TGuid; Param: Pointer = nil; const Owner: IUnknown = nil; const Controller: IUnknown = nil): IUnknown; overload;
    class procedure CreateObject(const ClassID: TGuid; const IID: TGuid; out Obj; Param: Pointer = nil; const Owner: IUnknown = nil; const Controller: IUnknown = nil); overload;
    class function CreateObject(const ProgID: string; Param: Pointer = nil; const Owner: IUnknown = nil; const Controller: IUnknown = nil): IUnknown; overload;
    class procedure CreateObject(const ProgID: string; const IID: TGuid; out Obj; Param: Pointer = nil; const Owner: IUnknown = nil; const Controller: IUnknown = nil); overload;
    class function CreateObjectOn(const FileName: String; const ClassID: TGuid; Param: Pointer = nil; const Owner: IUnknown = nil; const Controller: IUnknown = nil): IUnknown; overload;
    class function CreateObjectOn(const FileName: String; const ProgID: string; Param: Pointer = nil; const Owner: IUnknown = nil; const Controller: IUnknown = nil): IUnknown; overload;
    class function CreateTool(const FileName: String): TClass; overload;
    class function CreateTool(const ClassID: TGuid): TClass; overload;
    class function FindSourceLibrary(const Obj: IUnknown; out Lib: ISharedLibrary): Boolean;
    class procedure Release(var Obj: IUnknown); {$IFDEF USE_DEPRECATED} deprecated; {$ENDIF}
    class procedure ReleaseObject(var Instance);
    class procedure ReleaseTool(var Tool);
    class procedure Register(const Factory: ISharedFactory); overload;
    class procedure Register(const ClassID: TGUID; ClassType: TSilSharedObjectClass; const ProgID: string = ''); overload;
    class procedure Register(const ClassID: TGUID; ClassType: TSilObjectClass; const ProgID: string = ''); overload;
    class procedure Unregister(const Factory: ISharedFactory); overload;
    class procedure Unregister(const ClassID: TGUID); overload;
    class procedure Unregister(const ProgID: string); overload;
    class procedure Unregister(ClassType: TClass); overload;
    class function ID: TGUID; 
  private
    class procedure DoInitialize;
    class procedure DoCheck;
    class procedure DoFinalize;
  end;
                                                 
implementation

uses
  Sil,
  SilShSharedObject,
  SilSfSharedManager,
  SilSfSharedFactoryList,
  SilShSharedManager,
  SilStSharedObjectFactory,
  SilStObjectFactory,
  SilSkClassFactory,
  SilSmSharedProviderV1,
  SilSmSharedProviderV2;

var
  MManager: ISharedObjectManager = nil;

{ SharedObject }

class procedure SharedObject.Initialize(const Key: string);
begin
  Initialize(Os.Registry.Open(Key, True));
end;

class procedure SharedObject.Initialize(const Key: INamedKey);
begin
  DoCheck;
  Os.Environment.Load(Key);
  MManager.Initialize(Key.Keys.Get('Objects', True));
end;

class procedure SharedObject.Finalize;
begin
  if Assigned(MManager) then
  begin
    MManager.Finalize;
    DoFinalize;
  end;
end;

class procedure SharedObject.SetPath(const Path: String);
begin
  DoCheck;
  MManager.Config.SetPath(Path);
end;

class function SharedObject.GetPath: String;
begin
  DoCheck;
  Result := MManager.Config.Path.Text;
end;

class procedure SharedObject.AddPath(const Path: String);
begin
  DoCheck;
  MManager.Config.AddPath(Path);
end;

class function SharedObject.GetMask: String;
begin
  DoCheck;
  Result := MManager.Config.Mask.Text;
end;

class procedure SharedObject.SetMask(const Mask: String);
begin
  DoCheck;
  MManager.Config.SetMask(Mask);
end;

class procedure SharedObject.AddMask(const Mask: String);
begin
  DoCheck;
  MManager.Config.AddMask(Mask);
end;

class procedure SharedObject.ReleaseObject(var Instance);
begin
  DoCheck;
  MManager.Factory.ReleaseInstance(Instance);
end;

class procedure SharedObject.ReleaseTool(var Tool);
begin
  DoCheck;
  MManager.Factory.ReleaseTool(Tool);
end;

class procedure SharedObject.Register(const Factory: ISharedFactory);
begin
  FactoryList.Add(Factory);
end;

class procedure SharedObject.Unregister(const Factory: ISharedFactory);
begin
  FactoryList.Remove(Factory);
end;

class procedure SharedObject.Register(const ClassID: TGUID; ClassType: TSilSharedObjectClass; const ProgID: string);
begin
  Register(TSilClassFactory.Create(SharedObjectFactory, ClassType, ClassID, ProgID));
end;

class procedure SharedObject.Unregister(const ProgID: string);
var
  Item: ISharedFactory;
begin
  FactoryList.Locked;
  if FactoryList.Find(ProgID, Item) then
    FactoryList.Remove(Item);
end;

class procedure SharedObject.Unregister(const ClassID: TGUID);
var
  Item: ISharedFactory;
begin
  FactoryList.Locked;
  if FactoryList.Find(ClassID, Item) then
    FactoryList.Remove(Item);
end;

class procedure SharedObject.Register(const ClassID: TGUID; ClassType: TSilObjectClass; const ProgID: string);
begin
  Register(TSilClassFactory.Create(ObjectFactory, ClassType, ClassID, ProgID));
end;

class procedure SharedObject.Unregister(ClassType: TClass);
var
  Enum: IEnumerator;
  Item: ISharedClassFactory;
begin
  FactoryList.Locked;
  
  with FactoryList do
    while Enumerate(Enum, ISharedClassFactory, Item) do
      if Item.ClassType = ClassType then
      begin
        Remove(Item);
        Break;
      end;
end;

class function SharedObject.CreateObject(const ClassID: TGuid; Param: Pointer; const Owner: IUnknown; const Controller: IUnknown): IUnknown;
begin
  DoCheck;
  Result := MManager.Factory.CreateInstance(ClassID, Param, Owner, Controller);
end;

class function SharedObject.CreateObjectOn(const FileName: String; const ClassID: TGuid; Param: Pointer; const Owner, Controller: IUnknown): IUnknown;
begin
  DoCheck;
  Result := MManager.Factory.CreateInstance(FileName, ClassID, Param, Owner, Controller);
end;

class function SharedObject.CreateObjectOn(const FileName, ProgID: string; Param: Pointer; const Owner, Controller: IUnknown): IUnknown;
begin
  DoCheck;
  Result := MManager.Factory.CreateInstance(FileName, ProgID, Param, Owner, Controller);
end;

class function SharedObject.CreateObject(const ProgID: string; Param: Pointer; const Owner, Controller: IUnknown): IUnknown;
begin
  DoCheck;
  with MManager.Factory do
    Result := CreateInstance(ProgID, Param, Owner, Controller);
end;

class procedure SharedObject.CreateObject(const ClassID: TGuid; const IID: TGuid; out Obj; Param: Pointer; const Owner, Controller: IUnknown);
var
  Instance: IUnknown;
begin
  Instance := CreateObject(ClassID, Param, Owner, Controller);
  if Assigned(Instance) then
    if Instance.QueryInterface(IID, Obj) <> 0 then
      raise Sil.Error.Create('SharedObject.CreateObject: el objeto instanciado no soporta la interface solicitada (%s)' + Sil.Guid.ToStr(IID));
end;

class procedure SharedObject.CreateObject(const ProgID: string; const IID: TGuid; out Obj; Param: Pointer; const Owner, Controller: IUnknown);
var
  Instance: IUnknown;
begin
  Instance := CreateObject(ProgID, Param, Owner, Controller);
  if Assigned(Instance) then
    if Instance.QueryInterface(IID, Obj) <> 0 then
      raise Error.Create('SharedObject.CreateObject: el objeto instanciado no soporta la interface solicitada (%s)' + Sil.Guid.ToStr(IID));
end;

class function SharedObject.CreateTool(const FileName: String): TClass;
begin
  DoCheck;
  Result := MManager.Factory.CreateTool(FileName);
end;

class function SharedObject.CreateTool(const ClassID: TGuid): TClass;
begin
  DoCheck;
  Result := MManager.Factory.CreateTool(ClassID);
end;

class function SharedObject.ID: TGUID;
begin
  Result := GsSharedManager;
end;

class function SharedObject.FindSourceLibrary(const Obj: IUnknown; out Lib: ISharedLibrary): Boolean;
begin
//  Result := DoFindLibrary(Obj, Lib);
  Result := False;
end;

class procedure SharedObject.DoCheck;
begin
  if not Assigned(MManager) then
    DoInitialize;
end;

class procedure SharedObject.DoInitialize;
begin
  SilSfSharedManager.Get(@MManager, SysInit.HInstance);
end;

class procedure SharedObject.DoFinalize;
begin
  SilSfSharedManager.Release(@MManager, SysInit.HInstance);
end;

{$IFDEF USE_DEPRECATED}
{$WARN SYMBOL_DEPRECATED OFF}
{$ENDIF}

class procedure SharedObject.Release(var Obj: IUnknown);
begin
  ReleaseObject(Obj);
end;

{$IFDEF USE_DEPRECATED}
{$WARN SYMBOL_DEPRECATED ON}
{$ENDIF}

end.
