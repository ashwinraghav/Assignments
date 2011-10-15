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

unit SilOjWait;

{$I Defines.inc}

interface

uses
  SilBeTypes,
  SilBkTool,
  SilOsTypes,
  SilOiWait,
  SilOiHandle;

type
  SilWaitTool = class(Tool)
    class function Single(const Arg: IUnknown; TimeOut: OsWord; AvoidMsgLock: Boolean = false): Boolean; virtual;   
    class function Any(const Args: array of IUnknown; TimeOut: OsWord; var Signaled: Integer; AvoidMsgLock: Boolean): Boolean; overload; virtual; abstract;
    class function Any(const Args: array of IUnknown; TimeOut: OsWord; var Signaled: Integer): Boolean; overload; virtual; abstract;
    class function All(const Args: array of IUnknown; TimeOut: OsWord; AvoidMsgLock: Boolean): Boolean; overload; virtual; abstract;
    class function All(const Args: array of IUnknown; TimeOut: OsWord): Boolean; overload; virtual; abstract;
    class procedure Sleep(TimeOut: OsWord); virtual; abstract;
  end;

implementation

uses
  SilOeWait,
  SilLtReference;

{ SilWaitTool }

class function SilWaitTool.Single(const Arg: IInterface; TimeOut: OsWord; AvoidMsgLock: Boolean): Boolean;
var
  Waitable: IWaitable;
begin
  if Ref.GetInterface(Arg, IWaitable, Waitable) then
    Result := Waitable.WaitFor(TimeOut, AvoidMsgLock) = wrSignaled else
    Result := false;
end;


end.
