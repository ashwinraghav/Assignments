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

unit SilOjError;

{$I Defines.inc}

interface

uses
  SilBtError,
  SilOeError;

type
  SilErrorTool = class(SilBtError.Error)
    class function Code(AState: OsState): OsCode; overload; virtual; abstract;
    class procedure Check(AState: OsState; const API: string = ''; Exception: OsExceptionClass = nil); overload; virtual; abstract;
    class procedure Check(ACode: OsCode; const API: string = ''; Exception: OsExceptionClass = nil); overload; virtual; abstract;
    class function Create(ACode: OsCode; const API: string = ''; Exception: OsExceptionClass = nil): OsException; overload; virtual; abstract;
    class function ErrorMessage(ACode: OsCode): string; virtual; abstract;
    class function Check(Code: OsCode; const Exclusion: array of OsCode): Boolean; overload; virtual;
    class procedure Check(AState: OsState; const Exclusion: array of OsCode; const API: string; Exception: OsExceptionClass); overload; virtual;
    class procedure Check(AState: OsState; const Exclusion: array of OsCode; const API: string); overload; virtual;
    class procedure Check(AState: OsState; const Exclusion: array of OsCode); overload; virtual;
    class procedure Check(ACode: OsCode; const API: string; Args: array of const; Exception: OsExceptionClass = nil); overload; virtual;
    class function Create(ACode: OsCode; const Text: string; Args: array of const; const API: string; Exception: OsExceptionClass): OsException; overload; virtual;
    class function Create(ACode: OsCode; const Text: string; Args: array of const; const API: string): OsException; overload; virtual;
    class function Create(ACode: OsCode; const Text: string; Args: array of const): OsException; overload; virtual;
    class function Create(ACode: OsCode; const API: string; Args: array of const; Exception: OsExceptionClass = nil): OsException; overload; virtual;
    class procedure Throw(ACode: OsCode; const Text: string; Args: array of const; const API: string; Exception: OsExceptionClass); overload; virtual;
    class procedure Throw(ACode: OsCode; const Text: string; Args: array of const; const API: string); overload; virtual;
    class procedure Throw(ACode: OsCode; const Text: string; Args: array of const); overload; virtual;
    class procedure Throw(ACode: OsCode; const API: string = ''; Exception: OsExceptionClass = nil); overload; virtual;
    class procedure Throw(ACode: OsCode; const API: string; Args: array of const; Exception: OsExceptionClass = nil); overload; virtual;
    class function LastError: OsCode; virtual; abstract;
  end;

implementation

uses
  SilBtStr,
  SilOsTypes;

{ SilErrorTool }

class function SilErrorTool.Check(Code: OsCode; const Exclusion: array of OsCode): Boolean;
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

class procedure SilErrorTool.Check(AState: OsState; const Exclusion: array of OsCode; const API: string; Exception: OsExceptionClass);
var
  Code: OsCode;
begin
  if not AState then
  begin
    Code := Self.Code(AState);
    if not Check(Code, Exclusion) then
      Throw(Code, API, Exception);
  end;
end;

class procedure SilErrorTool.Check(AState: OsState; const Exclusion: array of OsCode; const API: string);
begin
  Self.Check(AState, Exclusion, API, OsExceptionClass(nil));
end;

class procedure SilErrorTool.Check(AState: OsState; const Exclusion: array of OsCode);
begin
  Self.Check(AState, Exclusion, '');
end;

class procedure SilErrorTool.Check(ACode: OsCode; const API: string; Args: array of const; Exception: OsExceptionClass);
begin
  Check(ACode, Str.Format(API, Args), Exception);
end;

class function SilErrorTool.Create(ACode: OsCode; const Text: string; Args: array of const; const API: string; Exception: OsExceptionClass): OsException;
begin
  if Exception = nil then Exception := OsException;
  Result := Exception.CreateFmt(Text, Args);
  Result.ErrorCode := ACode;
  Result.API := API;
end;

class function SilErrorTool.Create(ACode: OsCode; const Text: string; Args: array of const; const API: string): OsException;
begin
  Result := Self.Create(ACode, Text, Args, API, OsExceptionClass(nil));
end;

class function SilErrorTool.Create(ACode: OsCode; const Text: string; Args: array of const): OsException;
begin
  Result := Self.Create(ACode, Text, Args, '');
end;

class function SilErrorTool.Create(ACode: OsCode; const API: string; Args: array of const; Exception: OsExceptionClass): OsException;
begin
  Result := Create(ACode, Str.Format(API, Args), Exception);
end;

class procedure SilErrorTool.Throw(ACode: OsCode; const Text: string; Args: array of const; const API: string; Exception: OsExceptionClass);
begin
  raise Create(ACode, Text, Args, API);
end;

class procedure SilErrorTool.Throw(ACode: OsCode; const Text: string; Args: array of const; const API: string);
begin
  Self.Throw(ACode, Text, Args, API, OsExceptionClass(nil));
end;

class procedure SilErrorTool.Throw(ACode: OsCode; const Text: string; Args: array of const);
begin
  Self.Throw(ACode, Text, Args, '');
end;

class procedure SilErrorTool.Throw(ACode: OsCode; const API: string; Exception: OsExceptionClass);
begin
  raise Create(ACode, API, Exception);
end;

class procedure SilErrorTool.Throw(ACode: OsCode; const API: string; Args: array of const; Exception: OsExceptionClass);
begin
  Throw(ACode, Str.Format(API, Args), Exception);
end;

end.
