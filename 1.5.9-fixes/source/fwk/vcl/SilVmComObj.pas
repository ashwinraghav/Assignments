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

unit SilVmComObj;

interface

{$INCLUDE Defines.inc}

uses
  Classes,
  Sil,
  SilClasses;

type
  TVCLObject = class(TSilObject, IVCLComObject)
  private
    FComponent: TComponent;
    FCounting: Boolean;
  protected //- IVCLComObject
    function GetTypeInfoCount(out Count: Integer): HResult; stdcall;
    function GetTypeInfo(Index, LocaleID: Integer; out TypeInfo): HResult; stdcall;
    function GetIDsOfNames(const IID: TGUID; Names: Pointer; NameCount, LocaleID: Integer; DispIDs: Pointer): HResult; stdcall;
    function Invoke(DispID: Integer; const IID: TGUID; LocaleID: Integer; Flags: Word; var Params; VarResult, ExcepInfo, ArgErr: Pointer): HResult; stdcall;
    procedure FreeOnRelease;
  public
    constructor Create(AComponent: TComponent; CountRefs: Boolean);
    destructor Destroy; override;
    function QueryInterface(const IID: TGUID; out Obj): HResult; override; stdcall;
  end;

implementation

uses
  Windows;

{ TVCLObject }

constructor TVCLObject.Create(AComponent: TComponent; CountRefs: Boolean);
begin
  inherited Create;
  FCounting := CountRefs;
  FComponent := AComponent;
  FComponent.VCLComObject := Pointer(IVCLComObject(Self));
  if not CountRefs then _AddRef;
end;

destructor TVCLObject.Destroy;
  var Temp: TComponent;
begin
  if (FComponent <> nil) and (FComponent.VCLComObject <> nil) then
  begin
    Temp := FComponent;
    FComponent := nil;
    Temp.VCLComObject := nil;
    Temp.Free;
  end;
  inherited Destroy;
end;

procedure TVCLObject.FreeOnRelease;
begin
end;

function TVCLObject.QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
begin
  if Assigned(FComponent) and FComponent.GetInterface(IID, Obj) then
    Result := S_OK else
    Result := inherited QueryInterface(IID, Obj);
end;

function TVCLObject.GetTypeInfoCount(out Count: Integer): HResult; stdcall;
begin
  Result := E_NOTIMPL;
end;

function TVCLObject.GetTypeInfo(Index, LocaleID: Integer; out TypeInfo): HResult; stdcall;
begin
  Result := E_NOTIMPL;
end;

function TVCLObject.GetIDsOfNames(const IID: TGUID; Names: Pointer; NameCount, LocaleID: Integer; DispIDs: Pointer): HResult; stdcall;
begin
  Result := E_NOTIMPL;
end;

function TVCLObject.Invoke(DispID: Integer; const IID: TGUID; LocaleID: Integer; Flags: Word; var Params; VarResult, ExcepInfo, ArgErr: Pointer): HResult;  
begin
  Result := E_NOTIMPL;
end;


end.
