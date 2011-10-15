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

unit SilUtDll;

{$include Defines.inc}

interface

uses
  SilLiGlobalServices;

type
  RServices = record
    V1: IGlobalServicesV1;
    V2: IGlobalServiceListV2;
  end;

function GetProcAddress(Name: PChar; out Address; Handle: Integer = 0): Boolean;
function GetServices(out Services: RServices; Handle: LongWord = 0; Scope: TGlobalServiceKind = skGlobal; ForceHandle: Boolean = False): Boolean;
procedure InitServices;
procedure DoneServices;

implementation

uses
  SilLfMem,
  SilOhWntPsapi,
  SilLiEnumerator,
  SilSiMemoryManager,
  SilSjMemoryManager,
  Windows;

{$IFDEF USE_DEPRECATED} {$WARN SYMBOL_DEPRECATED OFF} {$ENDIF}

type
  TModuleList = array of LongWord;

function DoLoadServices(const EntryName: string; out Services: RServices; Handle: LongWord = 0; ForceHandle: Boolean = False): Boolean; forward;
function DoGetModules(out Modules: TModuleList): Boolean; forward;
function DoFindProc(const Modules: TModuleList; const Name: PChar; out Address): Boolean; forward;
function DoSearchProc(const Name: PChar; out Address): Boolean; forward;
procedure DoInitServicesV1(const Services: IGlobalServicesV1; This: THandle); forward;
procedure DoInitServicesV2(const Services: IGlobalServiceListV2; This: THandle); forward;
procedure DoDoneServicesV1(const Services: IGlobalServicesV1; This: THandle); forward;
procedure DoDoneServicesV2(const Services: IGlobalServiceListV2; This: THandle); forward;

var
  MServices: RServices;
  MSystemManager: TMemoryManager;
  MMemoryManager: ISharedMemoryManager = nil;

function DoHeapStatus: THeapStatus;
begin
  if Assigned(MMemoryManager) then
    Result := MMemoryManager.MemoryStatus else
    Result := System.GetHeapStatus;
end;

function GetServices(out Services: RServices; Handle: LongWord; Scope: TGlobalServiceKind; ForceHandle: Boolean): Boolean;
const
  SSil = 'Sil';
  SServices = 'Services';
  SServiceList = 'ServiceList';
  SScope: array[TGlobalServiceKind] of PChar = ('Global', 'Local');
begin
  Result := DoLoadServices(SSil + SScope[Scope] + SServiceList, Services, Handle, ForceHandle)
        or  DoLoadServices(SSil + SScope[Scope] + SServices, Services, Handle, ForceHandle);
end;

procedure InitServices;
var
  This: THandle;
begin
  This := FindHInstance(@MMemoryManager);

  if GetServices(MServices, This) then
  begin

    System.GetMemoryManager(MSystemManager);

    if Assigned(MServices.V2) then
      DoInitServicesV2(MServices.V2, This) else
      DoInitServicesV1(MServices.V1, This);

    if Assigned(MMemoryManager) and MMemoryManager.IsAssigned and (MMemoryManager.Instance <> This) then
      System.SetMemoryManager(MMemoryManager.Manager);

    SilLfMem.SetHeapStatus(DoHeapStatus);
    
  end;  
end;

procedure DoneServices;
var
  This: THandle;
begin
  This := FindHInstance(@MMemoryManager);
  try

    if Assigned(MServices.V2) then
      MServices.V2.Locked
    else if Assigned(MServices.V1) then
      MServices.V1.Locked;

    SilLfMem.SetHeapStatus(nil);
    if System.IsMemoryManagerSet then
      System.SetMemoryManager(MSystemManager);

    if Assigned(MServices.V2) then
      DoDoneServicesV2(MServices.V2, This)
    else if Assigned(MServices.V1) then
      DoDoneServicesV1(MServices.V1, This);

  finally
    System.Finalize(MServices);
  end;
end;

function GetProcAddress(Name: PChar; out Address; Handle: Integer = 0): Boolean;
begin
  Pointer(Address) := Windows.GetProcAddress(Handle, Name);
  Result := Pointer(Address) <> nil;
end;

function DoLoadServices(const EntryName: string; out Services: RServices; Handle: LongWord; ForceHandle: Boolean): Boolean;
var
  GetServices: function: IUnknown; stdcall;
  Instance: IUnknown;
  Name: PChar;
begin
  Name := PChar(EntryName);                        
  Result := (not ForceHandle and GetProcAddress(Name, GetServices))
        or  GetProcAddress(Name, GetServices, Handle)
        or  (not ForceHandle and DoSearchProc(Name, GetServices));
        
  if Result then
  begin
    Instance := GetServices();
    try
      Result := Instance.QueryInterface(IGlobalServiceListV2, Services.V2) = 0;
      if not Result then Result := Instance.QueryInterface(IGlobalServicesV1, Services.V1) = 0;
    finally
      Instance := nil;
    end;
  end;
end;

function DoGetModules(out Modules: TModuleList): Boolean;
var
  Handle, Size: LongWord;
  Enum: TEnumProcessModules;
begin
  Handle := Windows.LoadLibrary(CPsApi);
  Result := Handle <> 0; 
  if Result then
  try
    Result := GetProcAddress('EnumProcessModules', Enum, Handle);
    if Result then
    begin
      Enum(Windows.GetCurrentProcess(), nil, 0, Size);
      Result := Size <> 0; 
      if Result then
      begin
        SetLength(Modules, Size div SizeOf(LongWord));
        Result := Enum(Windows.GetCurrentProcess(), @Modules[0], Size, Size);
      end;
    end;
  finally
    Windows.FreeLibrary(Handle);
  end;
end;

function DoFindProc(const Modules: TModuleList; const Name: PChar; out Address): Boolean; 
var
  I: Integer;
begin
  Result := False;
  for I := Low(Modules) to High(Modules) do
    if GetProcAddress(Name, Address, Modules[I]) then
    begin
      Result := True;
      Break;
    end;
end;

function DoSearchProc(const Name: PChar; out Address): Boolean;
var
  Modules: TModuleList;
begin
  Result := DoGetModules(Modules)
        and DoFindProc(Modules, Name, Address)
        and Assigned(Pointer(Address));
end;

procedure DoInitServicesV1(const Services: IGlobalServicesV1; This: THandle);
begin
  Services.Find(SharedMemoryManager, ISharedMemoryManager, @MMemoryManager, Pointer(This));
end;

procedure DoInitServicesV2(const Services: IGlobalServiceListV2; This: THandle);
begin
  Services.Find(GsMemoryManager, ISharedMemoryManager, @MMemoryManager, This);
end;

procedure DoDoneServicesV1(const Services: IGlobalServicesV1; This: THandle);
var
  Enum: IEnumerator;
  Item: GlobalServiceType;
  Ref: RServiceRefV1;
begin
  with Services do
  begin
    while Enumerate(Enum, Ref) do
      if FindHInstance(Ref.Ptr) = This then
        Release(Ref.Service, Ref.Ptr);

    while Enumerate(Enum, Item) do
      if FindHInstance(Item) = This then
        Unregister(Item);
  end;
end;

procedure DoDoneServicesV2(const Services: IGlobalServiceListV2; This: THandle);
begin
  Services.Unregister(This);
end;

end.
