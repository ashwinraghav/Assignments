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

unit SilOtWait;

{$I Defines.inc}

interface

uses
  SilBkTool,

  SilOsTypes,

  SilOeWait,
  SilOiWait,
  SilOiHandle,

  SilOjWait;

type
  LinuxWaitTool = class(SilWaitTool)
    class function Any(const Args: array of IUnknown; TimeOut: OsWord; var Signaled: Integer; AvoidMsgLock: Boolean): Boolean; override;
    class function Any(const Args: array of IUnknown; TimeOut: OsWord; var Signaled: Integer): Boolean; override;
    class function All(const Args: array of IUnknown; TimeOut: OsWord; AvoidMsgLock: Boolean): Boolean; override;
    class function All(const Args: array of IUnknown; TimeOut: OsWord): Boolean; override;
    class procedure Sleep(TimeOut: OsWord); override;
  end;

implementation

uses
  SysUtils,
  SilLtReference,
  SilOtTool,
  SilOfWait;

{ LinuxWaitTool }

class function LinuxWaitTool.All(const Args: array of IUnknown; TimeOut: OsWord; AvoidMsgLock: Boolean): Boolean;
begin
  raise Exception.Create('not implemented: LinuxWaitTool.All');
  //Result := SilOfWait.Map(SilOfWait.Multiple(Os.Handle.GetHandles(Args), TimeOut, True, AvoidMsgLock), Length(Args)) = wrSignaled;
end;

class function LinuxWaitTool.Any(const Args: array of IUnknown; TimeOut: OsWord; var Signaled: Integer; AvoidMsgLock: Boolean): Boolean;
begin
  raise Exception.Create('not implemented: LinuxWaitTool.Any');
  //Signaled := SilOfWait.Multiple(Os.Handle.GetHandles(Args), TimeOut, False, AvoidMsgLock);
  //Result := SilOfWait.Map(Signaled, Length(Args)) = wrSignaled;
end;

class procedure LinuxWaitTool.Sleep(TimeOut: OsWord);
begin
  SilOfWait.Sleep(TimeOut);
end;

class function LinuxWaitTool.All(const Args: array of IUnknown; TimeOut: OsWord): Boolean;
begin
  Result := Self.All(Args, Timeout, false);
end;

class function LinuxWaitTool.Any(const Args: array of IUnknown; TimeOut: OsWord; var Signaled: Integer): Boolean;
begin
  Result := Self.Any(Args, Timeout, Signaled, false);
end;

end.
