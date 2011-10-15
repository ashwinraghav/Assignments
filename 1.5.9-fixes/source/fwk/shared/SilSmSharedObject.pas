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

unit SilSmSharedObject;

{$INCLUDE Defines.inc}

interface

uses
  Sil,
  SilSiSharedObject,
  SilShSharedObject,
  SilShSharedManager;

type
  TSilObjectProvider = class(
    TSilObject,
    ISharedObjectProvider )
  private
    //FModule: IObjectModule;
    //FContainer: IObjectContainer;
  protected // ISharedObjectProvider
    function GetModule: ISharedObjectModule;
    function GetContainer: ISharedObjectReferences;
  public
    constructor Create;
    destructor Destroy; override; 
  end;

  TSilObjectReferences = class (
    TSilObject,
    ISharedObjectReferences)
  private
    FModule: ISharedObjectModule;
    FReferences: IPointerList;
    FList: Pointer;
    function GetList: IPointerList;
  protected 
    function GetCount: Integer;
    function GetModule: ISharedObjectModule;
    procedure AddRef(Ptr: Pointer);
    function DropRef(Ptr: Pointer): Boolean;
    function Contains(Ptr: Pointer): Boolean;
  public
    constructor Create(const List: IInterfaceList; const Module: ISharedObjectModule);
    destructor Destroy; override;
    property List: IPointerList read GetList;
  end;

  TSilObjectModule = class(
    TSilInterfacedObject,               
    ISharedObjectModule )
  private
    FDll: ISharedLibrary;
  protected
    function GetDll: ISharedLibrary;
    function DoSignature: TSilSignature;
    function DoCreateObject: TSilCreateObject;
    function DoGetTool: TSilGetTool;
    function DoGetClassQuery: TSilClassQuery;
    function DoGetClassObject: TSilGetClassObject;
    function DoRegister: TSilRegister;
    function DoUnregister: TSilUnregister;
    function DoLocalServices: TSilLocalServices;
  public
    constructor Create(const DLL: ISharedLibrary); overload;
    destructor Destroy; override;
  public
    class function Create(const FileName: string; out Obj: ISharedObjectModule): Boolean; overload; 
  end;

implementation

function Signature: PChar; stdcall; forward;
function SilCreateObject(const ClassID: TGuid; const Owner: IUnknown = nil; const Controller: IUnknown = nil): IUnknown; stdcall; forward;
function SilGetTool: TClass; stdcall; forward;
function SilClassQuery(const ClassID: TGuid): Boolean; stdcall; forward;
function SilGetClassObject(const ClassID: TGuid; const IID: TGUID; out Obj): Integer; stdcall; forward;
function SilRegister: Integer; stdcall; forward;
function SilUnregister: Integer; stdcall; forward;
function SilLocalServices: IGlobalServices; stdcall; forward; 
{ TSilObjectProvider }

constructor TSilObjectProvider.Create;
begin

end;

destructor TSilObjectProvider.Destroy;
begin

  inherited;
end;

function TSilObjectProvider.GetContainer: ISharedObjectReferences;
begin

end;

function TSilObjectProvider.GetModule: ISharedObjectModule;
begin

end;

{ TSilObjectReferences }

constructor TSilObjectReferences.Create(const List: IInterfaceList; const Module: ISharedObjectModule);
begin
  inherited Create;

  FList := Pointer(List);
  FReferences := Sil.List.PointerList;
  FModule := Module;

  List.Add(ISharedObjectReferences(Self));
end;

destructor TSilObjectReferences.Destroy;
begin
  FList := nil;
  FReferences := nil;
  FModule := nil;
  inherited;
end;

procedure TSilObjectReferences.AddRef(Ptr: Pointer);
begin
  FReferences.Add(Ptr);
end;

function TSilObjectReferences.DropRef(Ptr: Pointer): Boolean;
begin
  Result := FReferences.Remove(Ptr) >= 0;
  if Result and (FReferences.Count = 0) then List.Remove(Self);
end;

function TSilObjectReferences.GetCount: Integer;
begin
  Result := List.Count;
end;

function TSilObjectReferences.GetModule: ISharedObjectModule;
begin
  Result := FModule;
end;

function TSilObjectReferences.GetList: IPointerList;
begin
  Result := IPointerList(FList);
end;

function TSilObjectReferences.Contains(Ptr: Pointer): Boolean;
begin
  Result := FReferences.IndexOf(Ptr) >= 0;
end;

{ TSilObjectModule }

constructor TSilObjectModule.Create(const DLL: ISharedLibrary);
begin
  inherited Create;
  FDll := DLL;  
end;

class function TSilObjectModule.Create(const FileName: string; out Obj: ISharedObjectModule): Boolean;
var
  DLL: ISharedLibrary;
  Signature: TSilSignature;
begin
  DLL := OS.SharedLibrary.Load(FileName);
  Result := DLL.Bind(CSilLibrarySignatureName, 0, Signature);
  if Result then Obj := Create(DLL);
end;

destructor TSilObjectModule.Destroy;
begin
  FDll := nil;
  inherited;
end;

function TSilObjectModule.GetDll: ISharedLibrary;
begin
  Result := FDll;
end;

function TSilObjectModule.DoSignature: TSilSignature;
begin
  if not Assigned(FDll) or not FDll.Bind(CSilLibrarySignatureName, 0, Result) then
    Result := Signature;
end;

function TSilObjectModule.DoCreateObject: TSilCreateObject;
begin
  if not Assigned(FDll) or not FDll.Bind(CSilCreateObject, 1, Result) then
    Result := SilCreateObject;
end;

function TSilObjectModule.DoGetClassQuery: TSilClassQuery;
begin
  if not Assigned(FDll) or not FDll.Bind(CSilClassQuery, 2, Result) then
    Result := SilClassQuery;
end;

function TSilObjectModule.DoGetTool: TSilGetTool;
begin
  if not Assigned(FDll) or not FDll.Bind(CSilGetTool, 3, Result) then
    Result := SilGetTool;
end;

function TSilObjectModule.DoGetClassObject: TSilGetClassObject;
begin
  if not Assigned(FDll) or not FDll.Bind(CSilGetClassObject, 4, Result) then
    Result := SilGetClassObject;
end;

function TSilObjectModule.DoRegister: TSilRegister;
begin
  if not Assigned(FDll) or not FDll.Bind(CSilRegister, 5, Result) then
    Result := SilRegister;
end;

function TSilObjectModule.DoUnregister: TSilUnregister;
begin
  if not Assigned(FDll) or not FDll.Bind(CSilUnregister, 6, Result) then
    Result := SilUnregister;
end;

function TSilObjectModule.DoLocalServices: TSilLocalServices;
begin
  if not Assigned(FDll) or not FDll.Bind(CSilLocalServices, 7, Result) then
    Result := SilLocalServices;
end;

function SilClassQuery(const ClassID: TGuid): Boolean;
begin
  Result := False;
end;

function SilCreateObject(const ClassID: TGuid; const Owner, Controller: IInterface): IUnknown;
begin
  Result := nil;
end;

function SilGetTool: TClass;
begin
  Result := nil;
end;

function Signature: PChar;
begin
  Result := '';
end;

function SilGetClassObject(const ClassID: TGuid; const IID: TGUID; out Obj): Integer; stdcall;
begin
  Result := E_NOTIMPL;
end;

function SilRegister: Integer; stdcall;
begin
  Result := E_NOTIMPL;
end;

function SilUnregister: Integer; stdcall;
begin
  Result := E_NOTIMPL;
end;

function SilLocalServices: IGlobalServices; stdcall;
begin
  Result := Sil.Local.Services;
end;                           

end.
 