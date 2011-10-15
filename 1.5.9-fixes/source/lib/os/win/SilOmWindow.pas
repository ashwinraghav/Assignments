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

unit SilOmWindow;

{$I Defines.inc}

interface

uses
  SilOeTypes,
  SilOmHandle;

type
  Tool = class
    class procedure DestroyWindow(Handle: THandle);
    class procedure PurgeMessages(Handle: THandle);
  end;

type
  TSilWindowHandle = class(TSilHandle)
  protected 
    procedure HandleIsValid(var Result: Boolean); override; 
    procedure HandleClose; override;
  end;

implementation

uses
  Windows,
  Messages;

{ Tool }

class procedure Tool.DestroyWindow(Handle: THandle);
begin
  Windows.DestroyWindow(Handle);
end;

class procedure Tool.PurgeMessages(Handle: THandle);
var
  Msg: TMsg;
begin
  while PeekMessage(Msg, Handle, 0, 0, PM_REMOVE) do
    DispatchMessage(Msg);
end;

{ TSilWindowHandle }
                                                         
procedure TSilWindowHandle.HandleClose;
var
  Handle: THandle;
begin
  Handle := GetValue;
  try
    Tool.DestroyWindow(Handle);
    //Tool.PurgeMessages(Handle);
  finally
    inherited;
  end;
end;

procedure TSilWindowHandle.HandleIsValid(var Result: Boolean);
begin
  Result := Windows.IsWindow(Self.GetValue);
end;

end.
