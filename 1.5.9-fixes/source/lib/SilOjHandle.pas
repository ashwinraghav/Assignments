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

unit SilOjHandle;

{$INCLUDE Defines.inc}

interface

uses
  SilBeTypes,
  SilBkTool,
  SilOsTypes,
  SilOiHandle;

type
  SilHandleTool = class(Tool)
    class function Create(Handle: THandle = 0; MustFree: Boolean = True): IHandle; virtual; abstract; 
    class function CreateList: IHandleList; virtual;
    class function GetHandle(const Obj: IUnknown): IHandle; overload; virtual;
    class function GetHandle(const Obj: IUnknown; out Handle: IHandle): Boolean; overload; virtual;
    class function GetHandles(const Args: array of IUnknown): THandleArray; virtual;
  end;

implementation

uses
  SilLtReference,

  SilOmHandleList,
  SilOmHandle,

  SilBtError,

  SilOdHandle;

{ Handle }

class function SilHandleTool.CreateList: IHandleList;
begin
  Result := TSilHandleList.Create;
end;

class function SilHandleTool.GetHandle(const Obj: IUnknown): IHandle;
begin
  if not GetHandle(Obj, Result) then
    raise Error.Create(SErrorHandleInvalidObject);
end;

class function SilHandleTool.GetHandle(const Obj: IInterface; out Handle: IHandle): Boolean;
var
  Handled: IHandledObject;
begin
  if Ref.GetInterface(Obj, IHandledObject, Handled) then
    Handle := Handled.Handle else
    Ref.GetInterface(Obj, IHandle, Handle);
  Result := Handle <> nil;
end;

class function SilHandleTool.GetHandles(const Args: array of IInterface): THandleArray;
var
  I: Integer;
  H: IHandle;
begin
  SetLength(Result, Length(Args));
  for I := Low(Args) to High(Args) do
    if GetHandle(Args[I], H) then
      Result[i] := H.Value else
      raise Error.Create(SErrorHandleInvalidArray, [I]);
end;

end.
