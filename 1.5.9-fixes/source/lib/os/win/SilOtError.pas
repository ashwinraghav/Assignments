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
  WindowsErrorTool = class(SilOjError.SilErrorTool)
    class function Code(AState: OsState): OsCode; overload; override;
    class procedure Check(AState: OsState; const API: string = ''; Exception: OsExceptionClass = nil); override;
    class procedure Check(ACode: OsCode; const API: string = ''; Exception: OsExceptionClass = nil); override;
    class function Create(ACode: OsCode; const API: string = ''; Exception: OsExceptionClass = nil): OsException; override;
    class function ErrorMessage(ACode: OsCode): string; override;
    class function LastError: OsCode; override;
  end;

implementation

uses
  Windows, SysUtils,
  SilOdError,
  SilOsTypes;

{ WindowsErrorTool }

class function WindowsErrorTool.Code(AState: OsState): OsCode;
begin
  if AState then
    Result := ERROR_SUCCESS else
    Result := Windows.GetLastError();
end;

class procedure WindowsErrorTool.Check(AState: OsState; const API: string; Exception: OsExceptionClass);
begin
  if not AState then Check(Windows.GetLastError, API);
end;

class procedure WindowsErrorTool.Check(ACode: OsCode; const API: string; Exception: OsExceptionClass);
begin
  if ACode <> ERROR_SUCCESS then
    Throw(ACode, API, Exception);
end;

class function WindowsErrorTool.Create(ACode: OsCode; const API: string; Exception: OsExceptionClass): OsException;
begin
  Result := Create(ACode, SErrorOSException, [ACode, ErrorMessage(ACode), API], API, Exception);
end;

class function WindowsErrorTool.ErrorMessage(ACode: OsCode): string;
begin
  Result := SysUtils.SysErrorMessage(ACode);
end;

class function WindowsErrorTool.LastError: OsCode;
begin
  Result := Windows.GetLastError;
end;
(*)

class function WindowsErrorTool.Create(ACode: OsCode; const Text: string; Args: array of const; const API: string; Exception: OsExceptionClass): OsException;
begin
  if Exception = nil then Exception := OsException;
  Result := Exception.CreateFmt(Text, Args);
  Result.ErrorCode := ACode;
  Result.API := API;
end;

class procedure WindowsErrorTool.Throw(ACode: OsCode; const Text: string; Args: array of const; const API: string; Exception: OsExceptionClass);
begin
  raise Create(ACode, Text, Args, API);
end;

class procedure WindowsErrorTool.Throw(ACode: OsCode; const API: string; Exception: OsExceptionClass);
begin
  raise Create(ACode, API, Exception);
end;

class function WindowsErrorTool.Check(Code: OsCode; const Exclusion: array of OsCode): Boolean;
var
  I: Integer;
begin
  for I := Low(Exclusion) to High(Exclusion) do
  begin
    if Exclusion[I] = Code then
    begin
      Result := True;
      Exit;
    end;
  end;
  Result := False;
end;

class procedure WindowsErrorTool.Check(AState: OsState; const Exclusion: array of OsCode; const API: string);
begin
  Self.Check(AState, Exclusion, API, OsExceptionClass(nil));
end;

class procedure WindowsErrorTool.Check(AState: OsState; const Exclusion: array of OsCode);
begin
  Self.Check(AState, Exclusion, '');
end;

class function WindowsErrorTool.Create(ACode: OsCode; const Text: string; Args: array of const; const API: string): OsException;
begin
  Result := Self.Create(ACode, Text, Args, API, OsExceptionClass(nil));
end;

class function WindowsErrorTool.Create(ACode: OsCode; const Text: string; Args: array of const): OsException;
begin
  Result := Self.Create(ACode, Text, Args, '');
end;

class procedure WindowsErrorTool.Throw(ACode: OsCode; const Text: string; Args: array of const; const API: string);
begin
  Self.Throw(ACode, Text, Args, API, OsExceptionClass(nil));
end;

class procedure WindowsErrorTool.Throw(ACode: OsCode; const Text: string; Args: array of const);
begin
  Self.Throw(ACode, Text, Args, '');
end;

(*)

end.
