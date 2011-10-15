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

unit SilSmFirebirdHandle;

{$INCLUDE Defines.inc}

interface

uses
  Sil,  
  SilSeFirebirdClient,
  SilSiFirebird;

type
  TSilFirebirdHandleClose = procedure(const Sender: IFbHandle) of object;

  TSilFirebirdHandle = class(
    TSilObject,
    IFbHandle )
  private
    FValue: Void;
    FOnClose: TSilFirebirdHandleClose;
    procedure DoClose(const Sender: IFbHandle);
  protected // IFbHandle
    function GetValue: Pointer;
    function GetIsAssigned: Boolean;
    procedure Close;
  public
    constructor Create(OnClose: TSilFirebirdHandleClose = nil);
    destructor Destroy; override;
  end;

implementation

{ TSilFirebirdHandle }

constructor TSilFirebirdHandle.Create(OnClose: TSilFirebirdHandleClose);
begin
  inherited Create;
  if Assigned(OnClose) then
    FOnClose := OnClose else
    FOnClose := DoClose;
end;

destructor TSilFirebirdHandle.Destroy;
begin
  Close;
  inherited;
end;

procedure TSilFirebirdHandle.DoClose(const Sender: IFbHandle);
begin
  PVoid(Sender.Value)^ := nil;
end;

function TSilFirebirdHandle.GetIsAssigned: Boolean;
begin
  Result := Assigned(FValue);
end;

procedure TSilFirebirdHandle.Close;
begin
  if Assigned(FValue) then
    FOnClose(Self);
end;

function TSilFirebirdHandle.GetValue: Pointer;
begin
  Result := @FValue;
end;

end.
 