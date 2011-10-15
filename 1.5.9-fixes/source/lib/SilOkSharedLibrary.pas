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

unit SilOkSharedLibrary;

{$I Defines.inc}

interface

uses
  SilLkInterfaced,

  SilOiHandle,
  SilOiSharedLibrary,

  SilOkHandled;

type
  TSilSharedLibrary = class(
    TSilHandledObject,
    ISharedLibrary )
  private
    FFileName: String;
    FProcs: array of Pointer;
  protected
    function GetFileName: String;
    function DoGet(const Name: String): Pointer; virtual; abstract;
    function DoLoad(const Name: String): IHandle; virtual; abstract;
  protected // ISharedLibrary
    function Load(const FileName: String; RaiseOnError: Boolean = true): Boolean;
    function Get(const ProcName: String; out Address): Boolean;  
    function Bind(const ProcName: String; Index: Integer; out Address; RaiseOnError: Boolean = false): Boolean;   
    procedure Unload;
  public
    constructor Create(const FileName: String); overload;
  end;

implementation

uses
  SilOsError;

{ TSilSharedLibrary }

constructor TSilSharedLibrary.Create(const FileName: String);
begin
  inherited Create;
  Load(FileName);
end;

function TSilSharedLibrary.Get(const ProcName: String; out Address): Boolean;
begin
  Pointer(Address) := DoGet(ProcName);
  Result := Pointer(Address) <> nil;
end;

function TSilSharedLibrary.Bind(const ProcName: String; Index: Integer; out Address; RaiseOnError: Boolean): Boolean;
begin
  if Index >= Length(FProcs) then
    SetLength(FProcs, Index + 1);

  if not Assigned(FProcs[Index]) then
    FProcs[Index] := DoGet(ProcName);

  Pointer(Address) := FProcs[Index];
  Result := Pointer(Address) <> nil;

  if not Result and RaiseOnError then
    OsError.Check(OsError.LastError, 'TSilSharedLibrary.Bind(%s$%s)', [Self.FFileName, ProcName]);
end;

function TSilSharedLibrary.Load(const FileName: String; RaiseOnError: Boolean): Boolean;
begin
  Result := NewHandle(DoLoad(FileName));

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

(*)
{ TSilSharedLibraryHandle }

procedure TSilSharedLibraryHandle.HandleClose;
begin
  FreeLibrary(Handle);
  inherited;
end;

procedure TSilSharedLibraryHandle.HandleIsValid(var Result: Boolean);
begin
  inherited;
	Result := Result and (Handle > 0);
end;

(*)

end.
