{********************************************************************************
 *                  Standard Interface Library (SIL)                            *
 *                                                                              *
 *       General purpose library whose design is based in STRONG                *
 *   use of interfaces.                                                         *
 *                                                                              *
 *                                                                              *
 *     Copyright (C) 2000 Mariano Podestá    marianop@intercom.com.ar           *
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

unit SilOxSharedLibrary;

{$I Defines.inc}

interface

uses
  SilOsTypes,
  
  SilOhInterface,

  SilOiHandle,
  SilOiSharedLibrary,

  SilLkInterfaced,
  SilOkHandled;

type
  TSilSharedLibrary = class(
    TSilHandledObject,
    ISharedLibrary )
  private
    FFileName: String;
    FProcs: array of Pointer;
    function DoGetInstance: IOsSharedLibraryInstance;
  protected
    function GetFileName: String;
  protected // ISharedLibrary
    function Load(const FileName: String; RaiseOnError: Boolean = true): Boolean;
    function Get(const ProcName: String; out Address): Boolean;
    function Bind(const ProcName: String; Index: Integer; out Address; RaiseOnError: Boolean = false): Boolean;
    procedure Unload;
  protected
    property Instance: IOsSharedLibraryInstance read DoGetInstance;
  public
    constructor Create(const FileName: String); 
    destructor Destroy; override;
  end;

implementation

uses
  SilOsInterface,
  
  SilOsError;

{ TSilSharedLibrary }

constructor TSilSharedLibrary.Create(const FileName: String);
begin
  inherited Create;
  Load(FileName);
end;

destructor TSilSharedLibrary.Destroy;
begin
  inherited;
end;

function TSilSharedLibrary.Get(const ProcName: String; out Address): Boolean;
begin
  if HasHandle then
    Pointer(Address) := Instance.Get(ProcName);
    Pointer(Address) := nil;
  Result := Pointer(Address) <> nil;
end;

function TSilSharedLibrary.Bind(const ProcName: String; Index: Integer; out Address; RaiseOnError: Boolean): Boolean;
begin
  if Index >= Length(FProcs) then
    SetLength(FProcs, Index + 1);

  if not Assigned(FProcs[Index]) then
    FProcs[Index] := Instance.Get(ProcName);

  Pointer(Address) := FProcs[Index];
  Result := Pointer(Address) <> nil;

  if not Result and RaiseOnError then
    OsError.Check(OsError.LastError, 'TSilSharedLibrary.Bind(%s$%s)', [Self.FFileName, ProcName]);
end;

function TSilSharedLibrary.Load(const FileName: String; RaiseOnError: Boolean): Boolean;
begin
  Result := NewHandle(Api.SharedLibrary.Create(FileName));

  if not Result and RaiseOnError then
    OsError.Check(False, 'TSilSharedLibrary.Load:' + FileName);

  FFileName := FileName;
end;

function TSilSharedLibrary.GetFileName: String;
begin
  Result := FFileName;
end;

procedure TSilSharedLibrary.Unload;
begin
  Close;
end;

function TSilSharedLibrary.DoGetInstance: IOsSharedLibraryInstance;
begin
  Result := IOsSharedLibraryInstance(inherited Handle);
end;

end.
