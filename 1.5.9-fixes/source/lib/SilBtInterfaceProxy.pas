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

unit SilBtInterfaceProxy;

{$INCLUDE Sil.inc}

interface

uses
  SilBkTool,
  SilBeInterfaceProxy;

type
  InterfaceProxy = class(Tool)
    class function Create(const IID: TGUID; const Controller: IUnknown; const Methods: array of RMethodInfo; Instance: Pointer): IUnknown;
    class function Method(Address: Pointer; Info: Pointer = nil; StackSize: LongWord = 4; Instance: Pointer = nil): RMethodInfo; overload;
    class function Method(Address: Pointer; StackSize: LongWord; Info: Pointer = nil; Instance: Pointer = nil): RMethodInfo; overload;
  end;    

implementation

uses
  SilBfInterfaceProxy,
  SilBhInterfaceProxy,
  SilAfInterfaceCode,
  SilAfProxyInvoke,
  SilOfLocked;

class function InterfaceProxy.Create(const IID: TGUID; const Controller: IUnknown; const Methods: array of RMethodInfo; Instance: Pointer): IUnknown;
var
  Proxy: PInterfaceProxy;
  Index: Integer;
  Entry: PMethodEntry;
begin
  ASSERT(Assigned(Controller));

  Proxy := System.New(PInterfaceProxy);
  Proxy.RefCount := 1;
  Proxy.Controller := Controller;
  Proxy.Instance := Instance;

  SetLength(Proxy.VmtProxy, 3 + Length(Methods));
  SetLength(Proxy.Methods.List, Length(Methods));

  Proxy.IID := IID;
  Proxy.VmtProxy[0] := @DoQueryInterface;
  Proxy.VmtProxy[1] := @DoAddRef;
  Proxy.VmtProxy[2] := @DoRelease;
  MakeHeader(Proxy.Methods.Header, @DoProxyStdcallInvoke);

  for Index := Low(Methods) to High(Methods) do
  begin
    Entry := @Proxy.Methods.List[Index];
    MakeCode(Entry.Code, @Proxy.Methods.Header.EntryPoint);
    Entry.Info := Methods[Index];
    if not Assigned(Entry.Info.Address.Data) then
      Entry.Info.Address.Data := Instance;
    Entry.Info.StackSize := Entry.Info.StackSize; 
    Proxy.VmtProxy[3 + Index] := @Proxy.Methods.List[Index].Code.EntryPoint;
  end;

  Result := IUnknown(Proxy);
  Dec(Proxy.RefCount);
end;

class function InterfaceProxy.Method(Address: Pointer; Info: Pointer; StackSize: LongWord; Instance: Pointer): RMethodInfo;
begin
  Result.Address.Code := Address;
  Result.Address.Data := Instance;
  Result.Info := Info;
  Result.StackSize := StackSize;
end;

class function InterfaceProxy.Method(Address: Pointer; StackSize: LongWord; Info: Pointer = nil; Instance: Pointer = nil): RMethodInfo;
begin
  Result.Address.Code := Address;
  Result.Address.Data := Instance;
  Result.Info := Info;
  Result.StackSize := StackSize;
end;

end.
