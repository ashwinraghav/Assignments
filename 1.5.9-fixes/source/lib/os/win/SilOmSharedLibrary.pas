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

unit SilOmSharedLibrary;

{$I Defines.inc}

interface

uses
  SilOsTypes,
  SilOiHandle,
  SilOkSharedLibrary,
  SilOmHandle;

type
  TSilWindowsSharedLibrary = class(TSilSharedLibrary)
  protected
    function DoCreateHandle(const Value: THandle; const MustFree: Boolean = True): IHandle; override;
  protected // ILibrary
    function DoGet(const Name: String): Pointer; override;
    function DoLoad(const Name: String): IHandle; override;
  end;

  TSilLibraryHandle = class(TSilHandle, IHandle)
  protected
    procedure HandleIsValid(var Result: Boolean); override;
    procedure HandleClose; override;
  end;

  TLibrary = class(TSilWindowsSharedLibrary) end {$IFDEF USE_DEPRECATED} deprecated {$ENDIF};

implementation

uses
  Windows,
  SilBtStr,
  SilOtError;

{ TSilWindowsSharedLibrary }

function TSilWindowsSharedLibrary.DoCreateHandle(const Value: THandle; const MustFree: Boolean): IHandle;
begin
  Result := TSilLibraryHandle.Create(Value, MustFree);
end;

function TSilWindowsSharedLibrary.DoGet(const Name: String): Pointer;
begin
  Result := Windows.GetProcAddress(Handle.Value, PChar(Name));
end;

function TSilWindowsSharedLibrary.DoLoad(const Name: String): IHandle;
var
  OldMode: LongWord;
  Flags: LongWord; 
begin
  OldMode := Windows.SetErrorMode(SEM_FAILCRITICALERRORS or SEM_NOOPENFILEERRORBOX);
  try
    if Str.Pos(CPathSeparator, Name) > 0 then
      Flags := LOAD_WITH_ALTERED_SEARCH_PATH else
      Flags := 0;
    Result := DoCreateHandle(Windows.LoadLibraryEx(PChar(Name), 0, Flags));
  finally
    Windows.SetErrorMode(OldMode);
  end;
end;

{ TSilLibraryHandle }

procedure TSilLibraryHandle.HandleClose;
begin
  Windows.FreeLibrary(Handle);
  inherited;
end;

procedure TSilLibraryHandle.HandleIsValid(var Result: Boolean);
begin
  inherited;
	Result := Result and (Handle > 0);
end;

end.
