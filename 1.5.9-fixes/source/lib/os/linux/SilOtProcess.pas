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

unit SilOtProcess;

{$I Defines.inc}

interface

uses
  libc,

  SilOsTypes,
  SilOiIpc,
  SilOeProcess,
  SilOiProcess,
  SilOjProcess,
  SilOsClasses;

type
  LinuxProcessTool = class(SilProcessTool)
  protected
    class function DoGetCurrent: IProcess; override;
    class function DoCreate(const ProcessInfo: Pointer; MustFree: Boolean; const Name: string): TSilOsProcess;
    //class function Executed(const ProcessInfo: TProcessInformation; const FileName: String): IProcess;
  public
    class function Execute(const FileName: String; Visibility: TProcessVisibility = pvNormal; Timeout: LongWord = 0): IProcess; override;
    class function Execute(const FileName, StartDir: String; Visibility: TProcessVisibility = pvNormal; Timeout: LongWord = 0): IProcess; override;
    class function PrevInstance(InstanceID: String = ''): Boolean; override;
  end;

implementation

uses
  SysUtils,
  SilBtStr,
  SilOfProcess,
  SilOmProcess,
  SilOtTool;

var
  MInstanceEvent: IEvent = nil;  // usado por PrevInstance

{ LinuxProcessTool }

class function LinuxProcessTool.Execute(const FileName: String; Visibility: TProcessVisibility; Timeout: LongWord): IProcess;
(*)var
  ProcessInfo: TProcessInformation;(*)
begin
  raise Exception.CreateFmt('%s: not implemented', ['LinuxProcessTool.Execute']);

(*)  SilOfProcess.Execute(FileName, Str.Null, ProcessInfo, Visibility, Timeout);(*)
(*)  Result := Executed( ProcessInfo, FileName );(*)
end;

class function LinuxProcessTool.Execute(const FileName, StartDir: String; Visibility: TProcessVisibility; Timeout: LongWord): IProcess;
(*)var
  ProcessInfo: TProcessInformation;(*)
begin
  raise Exception.CreateFmt('%s: not implemented', ['LinuxProcessTool.Execute']);
(*)  SilOfProcess.Execute(FileName, StartDir, ProcessInfo, Visibility, Timeout);(*)
(*)  Result := Executed( ProcessInfo, FileName );(*)
end;

(*)class function LinuxProcessTool.Executed(const ProcessInfo: TProcessInformation; const FileName: String): IProcess;
begin
  try
    Result := DoCreate(ProcessInfo, True, FileName);
  except
    Linux.CloseHandle(ProcessInfo.hProcess);
    Linux.CloseHandle(ProcessInfo.hThread);
    Result := nil;
  end;
end;(*)

class function LinuxProcessTool.DoGetCurrent: IProcess;
(*)var
  ProcessInfo: TProcessInformation;(*)
begin
(*)  ProcessInfo.hProcess := Linux.GetCurrentProcess();
  ProcessInfo.hThread := 0; {LMP - Falta asignar este parámetro}
  ProcessInfo.dwProcessId := Linux.GetCurrentProcessID();
  ProcessInfo.dwThreadId := MainThreadID;(*)
  Result := DoCreate(nil, False, ParamStr(0));
end;

class function LinuxProcessTool.DoCreate(const ProcessInfo: Pointer; MustFree: Boolean; const Name: string): TSilOsProcess;
begin
  Result := TSilOsProcess.Create(ProcessInfo, MustFree, Name);
end;

class function LinuxProcessTool.PrevInstance(InstanceID: String): Boolean;
var
  i: Integer;
begin
  if Length(InstanceID) = 0 then
  begin
    InstanceID := CommandLine;
    i := 0;
    while i <= Length(InstanceID) do
      if (InstanceID[i] in ['\', '/', ':']) then
        Str.Delete(InstanceID, i, 1) else
        Inc(i);
  end;

  if MInstanceEvent = nil then
  begin
    MInstanceEvent := OS.IPC.Event(true, false, PChar(InstanceID));
    Result := libc.errno <> 0;
    if Result then MInstanceEvent := nil;
  end else
    Result := true;
end;

initialization

finalization
  MInstanceEvent := nil;

end.

