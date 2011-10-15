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

unit SilOtWntPsapi;

{$I Defines.inc}

interface

uses
  SilBkTool,
  SilBeTypes,
  SilOsTypes,
  SilOhWntPsapi,
  SilOiWntPsapi;

type
  TProcessArray = array of LongWord;
  TModuleArray = array of LongWord;

  Psapi = class(Tool)
    class function Create(const FileName: string = CPsapi): IPsapi;

    class function EnumProcesses(
        const Instance: IPsapi;
          var Processes: TProcessArray;
              MaxLength: LongWord = 1000): OsState;

    class function EnumProcessModules(
        const Instance: IPsapi;
        const Process: THandle;
          var Modules: TModuleArray;
              MaxLength: LongWord = 1000): OsState;

    class function GetModuleFileName(
        const Instance: IPsapi;
        const Process: THandle;
        const Module: THandle;
          out FileName: AnsiString;
              MaxLength: LongWord = 260): LongWord; overload;

    class function GetModuleFileName(
        const Instance: IPsapi;
        const Process: THandle;
        const Module: THandle;
          out FileName: WideString;
              MaxLength: LongWord = 260): LongWord; overload;

    class function GetModuleBaseName(
        const Instance: IPsapi;
        const Process: THandle;
        const Module: THandle;
          out BaseName: AnsiString;
              MaxLength: LongWord = 260): LongWord; overload;

    class function GetModuleBaseName(
        const Instance: IPsapi;
        const Process: THandle;
        const Module: THandle;
          out BaseName: WideString;
              MaxLength: LongWord = 260): LongWord; overload;

    class function GetProcessImageFileName(
        const Instance: IPsapi;
        const Process: THandle;
          out FileName: AnsiString;
              MaxLength: LongWord = 260): LongWord; overload;

    class function GetProcessImageFileName(
        const Instance: IPsapi;
        const Process: THandle;
          out FileName: WideString;
              MaxLength: LongWord = 260): LongWord; overload; 
  end;

implementation

uses
  Windows,
  SilOmWntPsapi;

{ Psapi }

class function Psapi.Create(const FileName: string = CPsapi): IPsapi;
begin
  Result := TSilWntPsapiLibrary.Create(FileName);
end;

class function Psapi.EnumProcesses(const Instance: IPsapi; var Processes: TProcessArray; MaxLength: LongWord): OsState;
var
  Obtained: LongWord;
begin
  if Length(Processes) = 0 then SetLength(Processes, MaxLength);
  MaxLength := Length(Processes);
  Result := Instance.EnumProcesses(@Processes[0], MaxLength, Obtained);
  if Result then SetLength(Processes, Obtained div SizeOf(Processes[0]));
end;

class function Psapi.EnumProcessModules(const Instance: IPsapi; const Process: THandle; var Modules: TModuleArray; MaxLength: LongWord): OsState;
var
  Obtained: LongWord;
begin
  if Length(Modules) = 0 then SetLength(Modules, MaxLength);
  MaxLength := Length(Modules);
  Result := Instance.EnumProcessModules(Process, @Modules[0], MaxLength, Obtained);
  if Result then SetLength(Modules, Obtained div SizeOf(Modules[0]));
end;

class function Psapi.GetModuleFileName(const Instance: IPsapi; const Process, Module: THandle; out FileName: AnsiString; MaxLength: LongWord): LongWord;
begin
  SetLength(FileName, MaxLength);  
  Result := Instance.GetModuleFileNameExA(Process, Module, @FileName[1], Length(FileName) * SizeOf(FileName[1]));
  SetLength(FileName, Result);
end;

class function Psapi.GetModuleFileName(const Instance: IPsapi; const Process, Module: THandle; out FileName: WideString; MaxLength: LongWord): LongWord;
begin
  SetLength(FileName, MaxLength);  
  Result := Instance.GetModuleFileNameExW(Process, Module, @FileName[1], Length(FileName) * SizeOf(FileName[1]));
  SetLength(FileName, Result);
end;

class function Psapi.GetModuleBaseName(const Instance: IPsapi; const Process, Module: THandle; out BaseName: AnsiString; MaxLength: LongWord): LongWord;
begin
  SetLength(BaseName, MaxLength);  
  Result := Instance.GetModuleBaseNameA(Process, Module, @BaseName[1], Length(BaseName) * SizeOf(BaseName[1]));
  SetLength(BaseName, Result);
end;

class function Psapi.GetModuleBaseName(const Instance: IPsapi; const Process, Module: THandle; out BaseName: WideString; MaxLength: LongWord): LongWord;
begin
  SetLength(BaseName, MaxLength);  
  Result := Instance.GetModuleBaseNameW(Process, Module, @BaseName[1], Length(BaseName) * SizeOf(BaseName[1]));
  SetLength(BaseName, Result);
end;

class function Psapi.GetProcessImageFileName(const Instance: IPsapi; const Process: THandle; out FileName: AnsiString; MaxLength: LongWord): LongWord;
begin
  SetLength(FileName, MaxLength);  
  Result := Instance.GetProcessImageFileNameA(Process, @FileName[1], Length(FileName) * SizeOf(FileName[1]));
  SetLength(FileName, Result);
end;

class function Psapi.GetProcessImageFileName(const Instance: IPsapi; const Process: THandle; out FileName: WideString; MaxLength: LongWord): LongWord;
begin
  SetLength(FileName, MaxLength);  
  Result := Instance.GetProcessImageFileNameW(Process, @FileName[1], Length(FileName) * SizeOf(FileName[1]));
  SetLength(FileName, Result);
end;

end.
