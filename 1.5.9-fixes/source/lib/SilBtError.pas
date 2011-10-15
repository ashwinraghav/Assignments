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

unit SilBtError;

{$I Defines.inc}

interface

uses
  SilBeError,
  SilBkTool;

type
  Error = class(Tool)
    class procedure Throw(const Text: string; Exception: ExceptionType = nil); overload;
    class procedure Throw(const Text: string; const Args: array of const; Exception: ExceptionType); overload;
    class procedure Throw(const Text: string; const Args: array of const); overload;
    class procedure Throw(const Text: PResStringRec; Exception: ExceptionType = nil); overload;
    class procedure Throw(const Text: PResStringRec; const Args: array of const; Exception: ExceptionType); overload;
    class procedure Throw(const Text: PResStringRec; const Args: array of const); overload;
    class procedure Throw(const Ex: Exception); overload;
    class function  Create(const Text: string; Exception: ExceptionType = nil): Exception; overload;
    class function  Create(const Text: string; const Args: array of const; Exception: ExceptionType): Exception; overload;
    class function  Create(const Text: string; const Args: array of const): Exception; overload;
    class function  Create(const Text: PResStringRec; Exception: ExceptionType = nil): Exception; overload;
    class function  Create(const Text: PResStringRec; const Args: array of const; Exception: ExceptionType): Exception; overload;
    class function  Create(const Text: PResStringRec; const Args: array of const): Exception; overload;
    class procedure Check(const Condition: Boolean; const Text: string = ''; const Exception: ExceptionType = nil); overload;
    class procedure Check(const Condition: Boolean; const Text: string; const Args: array of const; const Exception: ExceptionType); overload;
    class procedure Check(const Condition: Boolean; const Text: string; const Args: array of const); overload;
    class procedure Check(const Condition: Boolean; const Text: PResStringRec; const Exception: ExceptionType = nil); overload;
    class procedure Check(const Condition: Boolean; const Text: PResStringRec; const Args: array of const; const Exception: ExceptionType); overload;
    class procedure Check(const Condition: Boolean; const Text: PResStringRec; const Args: array of const); overload;
    class procedure Abort;
  end;

implementation

uses
  SysUtils;

{ Error }

class procedure Error.Throw(const Text: string; Exception: ExceptionType);
begin
  raise Create(Text, Exception);
end;

class procedure Error.Throw(const Text: string; const Args: array of const; Exception: ExceptionType);
begin
  raise Create(Text, Args, Exception);
end;

class procedure Error.Throw(const Text: string; const Args: array of const);
begin
  raise Create(Text, Args);
end;

class procedure Error.Throw(const Text: PResStringRec; Exception: ExceptionType);
begin
  raise Create(Text, Exception);
end;

class procedure Error.Throw(const Text: PResStringRec; const Args: array of const; Exception: ExceptionType);
begin
  raise Create(Text, Args, Exception);
end;

class procedure Error.Throw(const Text: PResStringRec; const Args: array of const);
begin
  raise Create(Text, Args);
end;

class procedure Error.Throw(const Ex: Exception);
begin
  raise Ex;
end;

class function Error.Create(const Text: string; Exception: ExceptionType): Exception;
begin
  if Exception = nil then Exception := SilBeError.Exception;
  Result := Exception.Create(Text);
end;

class function Error.Create(const Text: string; const Args: array of const; Exception: ExceptionType): Exception;
begin
  if Exception = nil then Exception := SilBeError.Exception;
  Result := Exception.CreateFmt(Text, Args);
end;

class function Error.Create(const Text: string; const Args: array of const): Exception;
begin
  Result := Exception.CreateFmt(Text, Args);
end;

class function Error.Create(const Text: PResStringRec; Exception: ExceptionType): Exception;
begin
  if Exception = nil then Exception := SilBeError.Exception;
  Result := Exception.CreateRes(Text);
end;

class function Error.Create(const Text: PResStringRec; const Args: array of const; Exception: ExceptionType): Exception;
begin
  if Exception = nil then Exception := SilBeError.Exception;
  Result := Exception.CreateResFmt(Text, Args);
end;

class function Error.Create(const Text: PResStringRec; const Args: array of const): Exception;
begin
  Result := Exception.CreateResFmt(Text, Args);
end;

class procedure Error.Check(const Condition: Boolean; const Text: string; const Exception: ExceptionType);
begin
  if not Condition then Throw(Text, Exception);
end;

class procedure Error.Check(const Condition: Boolean; const Text: string; const Args: array of const; const Exception: ExceptionType);
begin
  if not Condition then Throw(Text, Args, Exception);
end;

class procedure Error.Check(const Condition: Boolean; const Text: string; const Args: array of const);
begin
  if not Condition then Throw(Text, Args);
end;

class procedure Error.Check(const Condition: Boolean; const Text: PResStringRec; const Exception: ExceptionType);
begin
  if not Condition then Throw(Text, Exception);
end;

class procedure Error.Check(const Condition: Boolean; const Text: PResStringRec; const Args: array of const; const Exception: ExceptionType);
begin
  if not Condition then Throw(Text, Args, Exception);
end;

class procedure Error.Check(const Condition: Boolean; const Text: PResStringRec; const Args: array of const);
begin
  if not Condition then Throw(Text, Args);
end;

class procedure Error.Abort;
begin
  SysUtils.Abort;
end;

end.
