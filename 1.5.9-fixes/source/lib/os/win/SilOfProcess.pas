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

unit SilOfProcess;

{$INCLUDE Defines.inc}

interface

uses
  Windows,
  SilOsTypes,
  SilOiModule,
  SilOiProcess,
  SilOiWntPsapi,
  SilOeProcess;

function AdjustTokenPrivileges(TokenHandle: THandle; DisableAllPrivileges: BOOL;
  const NewState: TTokenPrivileges; BufferLength: DWORD;
  PreviousState: PTokenPrivileges; ReturnLength: PDWORD): BOOL; stdcall;
  external advapi32 name 'AdjustTokenPrivileges';

function EnablePrivilege(Handle: THandle; AName: string; AState: Boolean): Boolean;
function EnablePrivilegeByPid(Pid: LongWord; const Privilege: String; Value: Boolean): Boolean;

function PriorityToWindows(
           const Value: TProcessPriority): Integer;

function WindowsToPriority(
           const Value: Integer): TProcessPriority;

function Execute(const FileName, StartDir: String;
  out ProcessInfo: TProcessInformation;
  Visibility: TProcessVisibility = pvNormal; Timeout: LongWord = 0; CreationFlags: LongWord = 0): boolean;

procedure DoGetProcessesWinNT(const List: IProcessList; const Mask: String);
procedure DoGetProcessesWin95(const List: IProcessList; const Mask: String);

procedure DoGetModulesWinNT(const Process: THandle; const List: IModuleList);

function PsApi: IPsapi;

implementation

uses
  SilBtStr,
  SilOsError,
  SilOtTool,

  SilOsClasses,
  SilOsWntPsapi,

  SilOhKernel32,
  SilOiKernel32,
  SilOmKernel32;

const
  GProcessPriorities: array [TProcessPriority] of Integer =
    (
      IDLE_PRIORITY_CLASS,
      NORMAL_PRIORITY_CLASS,
      HIGH_PRIORITY_CLASS,
      REALTIME_PRIORITY_CLASS
    );

const
  GProcessVisibility: array [TProcessVisibility] of Integer =
    (
      SW_HIDE,
      SW_SHOWNORMAL,
      SW_SHOWMINIMIZED,
      SW_SHOWMAXIMIZED,
      SW_SHOWNOACTIVATE
    );

var
  MKernel32: IKernel32 = nil;
  MPsApi: IPsapi = nil;

procedure DoCheckKernel32;
begin
  if not Assigned(MKernel32) then
    MKernel32 := TKernel32.Create(CKernel32);
end;

procedure DoCheckPsApi;
begin
  if not Assigned(MPsApi) then
    MPsApi := SilOsWntPsapi.Psapi.Create();
end;

function PsApi: IPsapi;
begin
  DoCheckPsApi;
  Result := MPsApi;
end;

function PriorityToWindows(const Value: TProcessPriority): Integer;
begin
  Result := GProcessPriorities[Value];
end;

function WindowsToPriority(const Value: Integer): TProcessPriority;
begin
  for Result := Low(Result) to High(Result) do
    if GProcessPriorities[Result] = Value then
      Exit;
  Result := ppIdle;
end;

function Execute(const FileName, StartDir: String; out ProcessInfo: TProcessInformation;
  Visibility: TProcessVisibility; Timeout: LongWord; CreationFlags: LongWord): boolean;
var
  StartupInfo: TStartupInfo;
  PStart: PChar;
begin
  FillChar(StartupInfo, SizeOf(StartupInfo), 0);

  StartupInfo.cb := SizeOf(TStartupInfo);
  StartupInfo.dwFlags := STARTF_USESHOWWINDOW;
  StartupInfo.wShowWindow := GProcessVisibility[Visibility];

  if Length(StartDir) > 0 then
    PStart := PChar(StartDir) else
    PStart := nil;

  OsError.Check(Windows.CreateProcess(nil, PChar(FileName), nil, nil, false, CreationFlags,
    nil, pStart, StartupInfo, ProcessInfo), 'Windows.CreateProcess');

  if Timeout <> 0 then
  begin
    WaitForInputIdle(ProcessInfo.hProcess, INFINITE);
    WaitForSingleObject(ProcessInfo.hProcess, Timeout);
  end;

  Result := true;
end;

procedure DoGetProcessesWinNT(const List: IProcessList; const Mask: String);
var
  lpdwPIDs: array [0..255] of LongWord;
  Size, Need, Need2: LongWord;
  hMod: THandle;
  i, FileNameLen: Integer;
  Process: IProcess;
  Info: TProcessInformation;
  FileName: String;
begin
  DoCheckPsApi;
  Size := Length(lpdwPIDs) * SizeOf(LongWord);

  if MPsApi.EnumProcesses(@lpdwPIDs, Size, Need) then
  begin
    Need := Need div SizeOf(Word);

    for i := 0 to Need - 1 do
    begin
      Process := nil;
      FillChar(Info, SizeOf(Info), 0);
      Info.hProcess := Windows.OpenProcess(PROCESS_QUERY_INFORMATION or PROCESS_VM_READ, false, lpdwPIDs[i]);

      if Info.hProcess <> 0 then
      begin
        if MPsApi.EnumProcessModules(Info.hProcess, @hMod, SizeOf(hMod), Need2) then
        begin
          SetLength(FileName, MAX_PATH);
          FileNameLen := MPsApi.GetModuleFileNameExA(Info.hProcess, hMod, PChar(FileName), Length(FileName));

          if FileNameLen > 0 then
          begin
            FileName := Str.Copy(FileName, 1, FileNameLen);

            if not Str.IsEmpty(FileName) and (Str.IsEmpty(Mask) or Str.WildCard(OS.FileSystem.GetFileName(FileName), Mask, true)) then
            begin
              Info.dwProcessId := lpdwPIDs[i];
              Process := TSilOsProcess.Create(Info, true, FileName);
              List.Add(Process);
            end;
          end;
        end;

        if not Assigned(Process) then Windows.CloseHandle(Info.hProcess);
      end;
    end;
  end;
end;

procedure DoGetModulesWinNT(const Process: THandle; const List: IModuleList);
var
  Size: LongWord;
  Handles: array of THandle;
  I: Integer;
begin
  DoCheckPsApi;

  MPsApi.EnumProcessModules(Process, nil, 0, Size);

  if Size > 0 then
  begin
    SetLength(Handles, Size div SizeOf(THandle));
    MPsApi.EnumProcessModules(Process, @Handles[0], Size, Size);
    if Size = 0 then Exit;

    for I := Low(Handles) to High(Handles) do
      List.Add(TSilOsModule.Create(Handles[I]));

  end;
end;

procedure DoGetProcessesWin95(const List: IProcessList; const Mask: String);
var
  hToolHlp: THandle;
  Entry: PROCESSENTRY32;
  Process: IProcess;
  Info: TProcessInformation;
  FileName: String;
begin
  DoCheckKernel32;

  hToolHlp := MKernel32.CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);
  Entry.dwSize := SizeOf(Entry);

  if MKernel32.Process32First(hToolHlp, @Entry) then
    repeat
      FileName := Str.Trim(Entry.szExeFile);

      if not Str.IsEmpty(FileName) and (Str.IsEmpty(Mask) or Str.WildCard(OS.FileSystem.GetFileName(FileName), Mask, true)) then
      begin
        FillChar(Info, SizeOf(Info), 0);
        Info.hProcess := Windows.OpenProcess(PROCESS_QUERY_INFORMATION or PROCESS_VM_READ, false, Entry.th32ProcessID);

        if Info.hProcess <> 0 then
        begin
          Info.dwProcessId := Entry.th32ProcessID;
          Process := TSilOsProcess.Create(Info, true, FileName);
          List.Add(Process);
        end;
      end;
    until not MKernel32.Process32Next(hToolHlp, @Entry);
end;

function EnablePrivilege(Handle: THandle; AName: string; AState: Boolean): Boolean;
var
  tp: TTokenPrivileges;
begin
  Result := false;
  if not LookupPrivilegeValue(nil, PChar(AName), tp.Privileges[0].Luid) then Exit;

  tp.PrivilegeCount := 1;

  if AState then tp.Privileges[0].Attributes := SE_PRIVILEGE_ENABLED
            else tp.Privileges[0].Attributes := 0;

  Result := SilOfProcess.AdjustTokenPrivileges(Handle, false, tp, SizeOf(tp), nil, nil);
end;

function EnablePrivilegeByPid(Pid: LongWord; const Privilege: String; Value: Boolean): Boolean;
var
  hProc, hToken: THandle;
begin
  hProc := Windows.OpenProcess(PROCESS_TERMINATE, false, Pid);

  if Windows.OpenProcessToken(GetCurrentProcess, TOKEN_ADJUST_PRIVILEGES, hToken) then
    Result := EnablePrivilege(hToken, Privilege, Value) else
    Result := false;

  if hProc > 0 then
    Windows.CloseHandle(hProc);
end;


end.
