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

unit SilSmSharedFunctions;

{$INCLUDE Defines.inc}

interface

uses
  Sil,
  SilSiSharedObject,
  SilShSharedObject;

type
  TSilName = (nmNONE, nmSIL, nmSO);
  
  TSilObjectFunctions = class(
    TSilInterfacedObject,
    ISharedObjectFunctions )
  private
    FDll: ISharedLibrary;
    FModule: IModule2;
    FLibrary: IVersionNumber;
    FRuntime: IVersionNumber;
    FSupport: TSharedObjectEntryPoints;
  private
    procedure DoParseSignature(const Value: string);
    procedure DoCheckEntries;
  protected // ISharedObjectFunctions
    function GetDll: ISharedLibrary;
    function GetModule: IModule2;
    function GetLibrary: IVersionNumber;
    function GetRuntime: IVersionNumber;
    function GetSupport: TSharedObjectEntryPoints;
  protected 
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
    class function Create(const FileName: string; out Instance: ISharedObjectFunctions): Boolean; overload;
  public 
    property Signature: TSilSignature read DoSignature;
    property CreateObject: TSilCreateObject read DoCreateObject;
    property GetTool: TSilGetTool read DoGetTool;
    property ClassQuery: TSilClassQuery read DoGetClassQuery;
    property GetClassObject: TSilGetClassObject read DoGetClassObject;
    property Register: TSilRegister read DoRegister;
    property Unregister: TSilUnregister read DoUnregister;
    property LocalServices: TSilLocalServices read DoLocalServices;
  end;

implementation

uses
  SilScSharedSignature, SilLjGlobal;

function SilSignature: PChar; stdcall; forward;
function SilCreateObject(const ClassID: TGuid; const Owner: IUnknown = nil; const Controller: IUnknown = nil): IUnknown; stdcall; forward;
function SilGetTool: TClass; stdcall; forward;
function SilClassQuery(const ClassID: TGuid): Boolean; stdcall; forward;
function SilGetClassObject(const ClassID: TGuid; const IID: TGUID; out Obj): Integer; stdcall; forward;
function SilRegister: Integer; stdcall; forward;
function SilUnregister: Integer; stdcall; forward;
function SilLocalServices: IUnknown; stdcall; forward;

{ TSilObjectFunctions }

class function TSilObjectFunctions.Create(const FileName: string; out Instance: ISharedObjectFunctions): Boolean;
var
  DLL: ISharedLibrary;
  Signature: TSilSignature;
begin
  try
    DLL := OS.SharedLibrary.Load(FileName, True);
    try
      Result := DLL.Bind(CSilLibrarySignatureName, 0, Signature);
      if Result then Instance := Create(DLL);
    finally
      DLL := nil;
    end;
  except
    Sil.Trace.Exception('tratando de cargar: ' + FileName, 'TSilObjectFunctions.Create', 'SharedObject');
    Dll := nil;
    raise;
  end;
end;

constructor TSilObjectFunctions.Create(const DLL: ISharedLibrary);
begin
  inherited Create;
  FDll := DLL;
  FModule := Sil.Os.Module.Get(Sil.Os.Handle.GetHandle(FDll).Value) as IModule2;
  DoParseSignature(Self.Signature());
  DoCheckEntries;
end;

destructor TSilObjectFunctions.Destroy;
begin
  FModule := nil;
  FDll := nil;
  inherited;
end;

function TSilObjectFunctions.GetDll: ISharedLibrary;
begin
  Result := FDll;
end;

function TSilObjectFunctions.GetModule: IModule2;
begin
  Result := FModule;
end;

function TSilObjectFunctions.GetLibrary: IVersionNumber;
begin
  Result := FLibrary;
end;

function TSilObjectFunctions.GetRuntime: IVersionNumber;
begin
  Result := FRuntime;
end;

function TSilObjectFunctions.GetSupport: TSharedObjectEntryPoints;
begin
  Result := FSupport;
end;

procedure TSilObjectFunctions.DoParseSignature(const Value: string);
var
  X: Integer;
  Buffer, Name, Version: string;
  Number: IVersionNumber;
  Kind: TSilName;
begin
  X := 0;

  while Sil.Str.Enumerate(Value, ',', Buffer, X) do
  begin
    if Sil.Str.Split(Buffer, '=', Name, Version) then
    begin
      Number := Sil.Os.Version.FromStr(Version);
      Kind := TSilName(Sil.Enum.Value(TypeInfo(TSilName), Name, 'nm'));
      case Kind of
        nmSIL: FLibrary := Number;
        nmSO : FRuntime := Number;
      end;
    end;
  end;
end;

procedure TSilObjectFunctions.DoCheckEntries;
var
  Dummy: Pointer;
begin
  FSupport := [];
  if FDll.Bind(CSilLibrarySignatureName, 0,  Dummy) then  Include(FSupport, epSignature);
  if FDll.Bind(CSilCreateObject,         1,  Dummy) then  Include(FSupport, epCreateObject);
  if FDll.Bind(CSilClassQuery,           2,  Dummy) then  Include(FSupport, epGetClassQuery);
  if FDll.Bind(CSilGetTool,              3,  Dummy) then  Include(FSupport, epGetTool);
  if FDll.Bind(CSilGetClassObject,       4,  Dummy) then  Include(FSupport, epGetClassObject);
  if FDll.Bind(CSilRegister,             5,  Dummy) then  Include(FSupport, epRegister);
  if FDll.Bind(CSilUnregister,           6,  Dummy) then  Include(FSupport, epUnregister);
  if FDll.Bind(CSilLocalServices,        7,  Dummy) then  Include(FSupport, epLocalServices);
end;

function TSilObjectFunctions.DoSignature: TSilSignature;
begin
  if not Assigned(FDll) or not FDll.Bind(CSilLibrarySignatureName, 0, Result) then
    Result := SilSignature;
end;

function TSilObjectFunctions.DoCreateObject: TSilCreateObject;
begin
  if not Assigned(FDll) or not FDll.Bind(CSilCreateObject, 1, Result) then
    Result := SilCreateObject;
end;

function TSilObjectFunctions.DoGetClassQuery: TSilClassQuery;
begin
  if not Assigned(FDll) or not FDll.Bind(CSilClassQuery, 2, Result) then
    Result := SilClassQuery;
end;

function TSilObjectFunctions.DoGetTool: TSilGetTool;
begin
  if not Assigned(FDll) or not FDll.Bind(CSilGetTool, 3, Result) then
    Result := SilGetTool;
end;

function TSilObjectFunctions.DoGetClassObject: TSilGetClassObject;
begin
  if not Assigned(FDll) or not FDll.Bind(CSilGetClassObject, 4, Result) then
    Result := SilGetClassObject;
end;

function TSilObjectFunctions.DoRegister: TSilRegister;
begin
  if not Assigned(FDll) or not FDll.Bind(CSilRegister, 5, Result) then
    Result := SilRegister;
end;

function TSilObjectFunctions.DoUnregister: TSilUnregister;
begin
  if not Assigned(FDll) or not FDll.Bind(CSilUnregister, 6, Result) then
    Result := SilUnregister;
end;

function TSilObjectFunctions.DoLocalServices: TSilLocalServices;
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

function SilSignature: PChar;
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

function SilLocalServices: IUnknown; stdcall;
begin
  Result := Sil.Local.ServicesV2();
end;

end.
