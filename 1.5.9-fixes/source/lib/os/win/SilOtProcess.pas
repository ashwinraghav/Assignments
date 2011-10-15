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

interface

{$I Defines.inc}

uses
  Windows,
  SilOsTypes,

  SilOiIpc,
  SilOeProcess,
  SilOiProcess,
  SilOjProcess,
  SilOsClasses;

type
  WindowsProcessTool = class(SilProcessTool)
  protected
    class function DoGetCurrent: IProcess; override;
    class function DoCreate(const ProcessInfo: TProcessInformation; MustFree: Boolean; const Name: string): TSilOsProcess;
    class function Executed(const ProcessInfo: TProcessInformation; const FileName: String): IProcess;
  public
    class function Execute(const FileName: String; Visibility: TProcessVisibility = pvNormal; Timeout: LongWord = 0): IProcess; override;
    class function Execute(const FileName, StartDir: String; Visibility: TProcessVisibility = pvNormal; Timeout: LongWord = 0): IProcess; override;
    class function PrevInstance(InstanceID: String = ''): Boolean; override;
    class function GetList(const Mask: String = ''): IProcessList; override;
    class function Open(PID: LongWord): IProcess; override;
  end;

implementation

uses
  SysUtils,

  SilLiEnumerator,
  SilBtError,
  SilBtStr,
  SilOfProcess,
  SilOmProcess,
  SilOmProcessList,
  SilOtTool;

var
  MInstanceEvent: IEvent = nil;  // usado por PrevInstance

{ WindowsProcessTool }

class function WindowsProcessTool.Execute(const FileName: String; Visibility: TProcessVisibility; Timeout: LongWord): IProcess;
var
  ProcessInfo: TProcessInformation;
begin
  SilOfProcess.Execute(FileName, Str.Null, ProcessInfo, Visibility, Timeout);
  Result := Executed( ProcessInfo, FileName );
end;

class function WindowsProcessTool.Execute(const FileName, StartDir: String; Visibility: TProcessVisibility; Timeout: LongWord): IProcess;
var
  ProcessInfo: TProcessInformation;
begin
  SilOfProcess.Execute(FileName, StartDir, ProcessInfo, Visibility, Timeout);
  Result := Executed( ProcessInfo, FileName );
end;

class function WindowsProcessTool.Executed(const ProcessInfo: TProcessInformation; const FileName: String): IProcess;
begin
  try
    Result := DoCreate(ProcessInfo, True, FileName);
  except
    Windows.CloseHandle(ProcessInfo.hProcess);
    Windows.CloseHandle(ProcessInfo.hThread);
    Result := nil;
  end;
end;

class function WindowsProcessTool.DoGetCurrent: IProcess;
var
  ProcessInfo: TProcessInformation;
begin
  ProcessInfo.hProcess := Windows.GetCurrentProcess();
  ProcessInfo.hThread := 0; {LMP - Falta asignar este parámetro}
  ProcessInfo.dwProcessId := Windows.GetCurrentProcessID();
  ProcessInfo.dwThreadId := MainThreadID;

  Result := DoCreate(ProcessInfo, False, ParamStr(0));
end;

class function WindowsProcessTool.DoCreate(const ProcessInfo: TProcessInformation;
  MustFree: Boolean; const Name: string): TSilOsProcess;
begin
  Result := TSilOsProcess.Create(ProcessInfo, MustFree, Name);
end;

class function WindowsProcessTool.PrevInstance(InstanceID: String): Boolean;
var
  i: Integer;
begin
  if Length(InstanceID) = 0 then
  begin
    InstanceID := CommandLine;
    i := 1;
    while i <= Length(InstanceID) do
      if (InstanceID[i] in ['\', '/', ':']) then
        Str.Delete(InstanceID, i, 1) else
        Inc(i);
  end;

  if MInstanceEvent = nil then
  begin
    MInstanceEvent := OS.IPC.Event(true, false, PChar(InstanceID));
    Result := Windows.GetLastError <> 0;
    if Result then MInstanceEvent := nil;
  end else
    Result := true;
end;

class function WindowsProcessTool.GetList(const Mask: String): IProcessList;
begin
  Result := TSilProcessList.Create(false);

  if Win32Platform = VER_PLATFORM_WIN32_NT then
    SilOfProcess.DoGetProcessesWinNT(Result, Mask) else
    SilOfProcess.DoGetProcessesWin95(Result, Mask);
end;

class function WindowsProcessTool.Open(PID: LongWord): IProcess;
var
  ProcessInfo: TProcessInformation;

  function DoGetName: String;
  var
    List: IProcessList;
    Item: IProcess;
    Enum: IEnumerator;
  begin
    List := OS.Process.GetList;

    while List.Enumerate(Enum, Item) do
      if Item.PID = PID then
      begin
        Result := Item.Info.FullName;
        Exit;
      end;

    Result := '';
  end;
begin
  ProcessInfo.hProcess := OpenProcess(PROCESS_ALL_ACCESS, false, PID);

  if ProcessInfo.hProcess > 0 then
  begin
    ProcessInfo.hThread := 0;
    ProcessInfo.dwProcessId := PID;
    ProcessInfo.dwThreadId := 0;

    Result := TSilWindowsProcess.Create(ProcessInfo, true, DoGetName);
  end else
    raise Error.Create('Unable to open process');
end;

initialization

finalization
  MInstanceEvent := nil;

end.

