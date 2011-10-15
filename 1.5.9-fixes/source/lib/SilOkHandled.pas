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

unit SilOkHandled;

{$I Defines.inc}

interface

uses
  SilLkInterfaced,

  SilLiObject,

  SilOsTypes,

  SilOiHandle;

type
  TSilHandledObject = class(
    TSilInterfacedObject,
    IHandledObject )
  protected 
    FHandle: IHandle;
  protected
    function DoCreateHandle(const Value: THandle; const MustFree: Boolean = True): IHandle; virtual; abstract;
    procedure DoOpen; virtual;
    procedure DoClose; virtual;
    function DoGetHasHandle: Boolean;
  protected // IHandledObject
		function GetHandle: IHandle; 
  protected
    function NewHandle(const Value: IHandle): Boolean;
    procedure SetHandle(const Value: IHandle);
    function ChangeHandle(const Value: IHandle): IHandle;
    procedure Close;
  public
    constructor Create(const Handle: IHandle; const Validate: Boolean = false; Locked: Boolean = False); overload; virtual;
    constructor Create(const Handle: THandle; const MustFree: Boolean = True; const Validate: Boolean = false; Locked: Boolean = False); overload; virtual;
    destructor Destroy; override;
    property HasHandle: Boolean read DoGetHasHandle;
    property Handle: IHandle read FHandle write SetHandle;
  end;

implementation

uses
  SilOsError,
  SilLtReference,
  SilOdHandle;

{ TSilHandledObject }

constructor TSilHandledObject.Create(const Handle: IHandle; const Validate: Boolean; Locked: Boolean);
begin
  inherited Create(Locked);
  if Validate then OsError.Check(Handle.IsValid, 'TSilHandledObject.Create: ' + SErrorHandleInvalidValue);
  FHandle := Handle;
end;

constructor TSilHandledObject.Create(const Handle: THandle; const MustFree: Boolean; const Validate: Boolean; Locked: Boolean);
begin
  Create(DoCreateHandle(Handle, MustFree), Validate, Locked);
end;

destructor TSilHandledObject.Destroy;
begin
  Close;
  inherited;
end;

procedure TSilHandledObject.Close;
begin
  SetHandle(nil);
end;

procedure TSilHandledObject.DoClose;
begin
end;

function TSilHandledObject.DoGetHasHandle: Boolean;
begin
  Result := Assigned(FHandle);
end;

function TSilHandledObject.GetHandle: IHandle;
begin
  Result := FHandle;
end;

procedure TSilHandledObject.DoOpen;
begin

end;

procedure TSilHandledObject.SetHandle(const Value: IHandle);
begin
  if not Ref.SameObject(Value, FHandle) then
  begin
    if Assigned(FHandle) then DoClose;
    FHandle := Value;
    if Assigned(FHandle) then DoOpen;
  end;
end;

function TSilHandledObject.ChangeHandle(const Value: IHandle): IHandle;
begin
  Result := FHandle;
  SetHandle(Value);
end;

function TSilHandledObject.NewHandle(const Value: IHandle): Boolean;
begin
  SetHandle(Value);
  Result := Assigned(Handle) and Handle.IsValid;
end;

end.
