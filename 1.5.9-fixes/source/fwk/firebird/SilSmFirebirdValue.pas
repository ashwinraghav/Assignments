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

unit SilSmFirebirdValue;

{$INCLUDE Defines.inc}

interface

uses
  Sil, SilLiValue, SilLmField,
  SilSeFirebirdClient,
  SilSiFirebird,
  SilShFirebird,
  SilSkFirebirdHandled;

type
  TSilFirebirdValue = class(
    TSilFirebirdHandled,
    IFbValueInternal )
  private
    FList: Pointer;
    FData: IFbDataInternal;
  private
    function DoGetList: IFbValuesInternal;
  protected // IFbValueInternal
    function DoGetCommand: IFbCommandInternal;
    function DoGetBinding: IFbBindingInternal;
    function DoGetHandle: PXSQLVAR_V1;
    procedure DoSetHandle(Value: PXSQLVAR_V1);
    procedure Detach;
  protected
    property List: IFbValuesInternal read DoGetList;
    property Handle: PXSQLVAR_V1 read DoGetHandle write DoSetHandle;
  public
    constructor Create(const List: IFbValuesInternal; const Data: IFbDataInternal);
    destructor Destroy; override;
  end;

implementation

uses
  SilSfFirebird,
  SilSmFirebirdData,
  SilSgFirebirdData;

{ TSilFirebirdValue }

constructor TSilFirebirdValue.Create(const List: IFbValuesInternal; const Data: IFbDataInternal);
begin
  inherited Create;
  FList := Pointer(List);
  List.Add(Self);
  FData := Data;
  
  Handle := @List.Handle.sqlvar[FData.Binding.Index];
  Handle.sqldata := FData.Memory;
  Handle.sqlind := FData.Status;
  
  Handle.sqltype := SqlType(FData.Binding.Domain);
  Handle.sqlsubtype := BlobType(FData.Binding.Domain);
  Handle.sqllen := FData.Binding.Domain.Length;
  Handle.sqlscale := FData.Binding.Domain.Scale;
end;

destructor TSilFirebirdValue.Destroy;
begin
  Detach;
  inherited;
end;

function TSilFirebirdValue.DoGetCommand: IFbCommandInternal;
begin
  Result := List.Command;
end;

function TSilFirebirdValue.DoGetBinding: IFbBindingInternal;
begin
  Result := FData.Binding;
end;

function TSilFirebirdValue.DoGetHandle: PXSQLVAR_V1;
begin
  Result := PXSQLVAR_V1(inherited Handle.Value^);
end;

procedure TSilFirebirdValue.DoSetHandle(Value: PXSQLVAR_V1);
begin
  PXSQLVAR_V1(inherited Handle.Value^) := Value;
end;

procedure TSilFirebirdValue.Detach;
var
  List: IFbValuesInternal;
begin
  if Assigned(FList) then
  begin
    if Assigned(Handle) then
    begin
      Handle.sqlind := nil;
      Handle.sqldata := nil;
      Handle := nil;
    end;
    FData := nil;
    List := Self.List; 
    FList := nil;
    List.Remove(Self);
  end;
end;

function TSilFirebirdValue.DoGetList: IFbValuesInternal;
begin
  Result := IFbValuesInternal(FList);
end;

end.
