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

unit SilOmHandle;

{$I Defines.inc}

interface

uses
  SilLkInterfaced,
  SilOcTypes,
  SilOeTypes,
  SilOiHandle;

type
  TSilHandle = class(
    TSilInterfacedObject,
    IHandle )
  private
    FHandle: THandle;
    FMustFree: Boolean;
  protected //IHandle
    function GetValue: THandle;
    function GetIsValid: Boolean;
    function GetIsOwned: Boolean;
    procedure Close;
  protected
    procedure HandleIsValid(var Result: Boolean); virtual;
    procedure HandleClose; virtual;
  protected
    property Handle: THandle read FHandle write FHandle;
  protected
    constructor CreateNew(AHandle: THandle; MustFree: Boolean); reintroduce; virtual; 
  public
    class function Create(AHandle: THandle = INVALID_HANDLE_VALUE; MustFree: Boolean = True): IHandle;
    destructor Destroy; override;
  end;

implementation

{ TSilHandle }

constructor TSilHandle.CreateNew(AHandle: THandle; MustFree: Boolean);
begin
  inherited Create;
  FHandle := AHandle;
  FMustFree := MustFree;
end;

class function TSilHandle.Create(AHandle: THandle; MustFree: Boolean): IHandle;
begin
  Result := CreateNew(AHandle, MustFree);
end;

destructor TSilHandle.Destroy;
begin
  if FMustFree then Close;
  inherited;
end;

procedure TSilHandle.Close;
begin
  if GetIsValid then HandleClose;
  FHandle := INVALID_HANDLE_VALUE;
end;

function TSilHandle.GetIsValid: Boolean;
begin
  Result := (FHandle <> INVALID_HANDLE_VALUE) and (FHandle <> NULL_HANDLE_VALUE);
  HandleIsValid(Result);
end;

function TSilHandle.GetValue: THandle;
begin
  Result := FHandle;
end;

procedure TSilHandle.HandleClose;
begin
end;

procedure TSilHandle.HandleIsValid(var Result: Boolean);
begin
end;

function TSilHandle.GetIsOwned: Boolean;
begin
  Result := FMustFree;
end;

end.
