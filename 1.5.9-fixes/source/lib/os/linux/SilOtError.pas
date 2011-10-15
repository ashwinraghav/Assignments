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

unit SilOtError;

{$I Defines.inc}

interface

uses
  SilOeError,
  SilOjError;

type
  LinuxErrorTool = class(SilOjError.SilErrorTool)
    class function Code(AState: OsState): OsCode; overload; override;
    class procedure Check(AState: OsState; const API: string = ''; Exception: OsExceptionClass = nil); override;
    class procedure Check(ACode: OsCode; const API: string = ''; Exception: OsExceptionClass = nil); override;
    class function Create(ACode: OsCode; const API: string = ''; Exception: OsExceptionClass = nil): OsException; override;
    class function ErrorMessage(ACode: OsCode): string; override;
    class function LastError: OsCode; override;
  end;

implementation

uses
  Libc,
  SysUtils,
  SilOdError,
  SilOsTypes;

{ LinuxErrorTool }

class function LinuxErrorTool.Code(AState: OsState): OsCode;
begin
  if AState then
    Result := 0 else
    Result := Libc.errno;
end;

class procedure LinuxErrorTool.Check(AState: OsState; const API: string; Exception: OsExceptionClass);
begin
  if not AState then Check(Libc.errno, API);
end;

class procedure LinuxErrorTool.Check(ACode: OsCode; const API: string; Exception: OsExceptionClass);
begin
  if ACode <> ERROR_SUCCESS then
    Throw(ACode, API, Exception);
end;

class function LinuxErrorTool.Create(ACode: OsCode; const API: string; Exception: OsExceptionClass): OsException;
begin
  Result := Create(ACode, SErrorOSException, [ACode, ErrorMessage(ACode), API], API, Exception);
end;

class function LinuxErrorTool.ErrorMessage(ACode: OsCode): string;
begin
  Result := SysUtils.SysErrorMessage(ACode);
end;

class function LinuxErrorTool.LastError: OsCode;
begin
  Result := libc.errno;
end;

end.
