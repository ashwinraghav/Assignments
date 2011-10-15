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

unit SilOmProcess;

{$I Defines.inc}

interface

uses
  Windows,
  
  SilOkProcess,

  SilOeProcess,

  SilOiWait,
  SilOiHandle,
  SilOiModule,
  SilOiProcess,
  SilOiFile;

type
  TSilWindowsProcess = class (TSilProcess)
  private
    FMustFree: Boolean;
    FProcessInfo: TProcessInformation;
  protected //- TSilProcess
    function GetPriority: TProcessPriority; override;
    procedure SetPriority(Value: TProcessPriority); override;
    function Terminate(Timeout: Cardinal): boolean; override;
    function GetPID: Cardinal; override;
    function GetMainThreadID: Cardinal; override;
    function GetModules: IModules; override;
    function GetCurrentPath: string; override;
    function GetExitCode: LongWord; override; 
  public
    constructor Create(const ProcessInfo: TProcessInformation; MustFree: Boolean; const Name: string); reintroduce;
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils,
  Messages,

  SilOsClasses,
  SilOfProcess,

  SilOtTool,
  SilOsError, SilOkModule;

{ TSilWindowsProcess }

constructor TSilWindowsProcess.Create(
  const ProcessInfo: TProcessInformation; MustFree: Boolean; const Name: string);
begin
  FMustFree := MustFree;
  FProcessInfo := ProcessInfo;

  inherited Create( Os.Handle.Create(ProcessInfo.hProcess, MustFree), Name );
end;

destructor TSilWindowsProcess.Destroy;
begin
  if FMustFree and (FProcessInfo.hThread <> 0) then
    Windows.CloseHandle(FProcessInfo.hThread);

  inherited;
end;

function TSilWindowsProcess.GetMainThreadID: Cardinal;
begin
  result := FProcessInfo.dwThreadId;
end;

function TSilWindowsProcess.GetModules: IModules;
var
  List: IModuleList;
begin
  List := TSilOsModuleList.Create;

  if Win32Platform = VER_PLATFORM_WIN32_NT then
    SilOfProcess.DoGetModulesWinNT(Self.Handle.Value, List) else
    ; // missing

  Result := List;
end;

function TSilWindowsProcess.GetCurrentPath: string;
var
  Size: LongWord;
begin
  Size := Windows.GetCurrentDirectory(0, nil);
  SetLength(Result, Size - 1);
  if Size > 1 then
    Windows.GetCurrentDirectory(Size, PChar(Result));
end;

function TSilWindowsProcess.GetPID: Cardinal;
begin
  result := FProcessInfo.dwProcessId;
end;

function TSilWindowsProcess.GetPriority: TProcessPriority;
begin
  Result := SilOfProcess.WindowsToPriority(Windows.GetPriorityClass(Handle.Value));
end;

procedure TSilWindowsProcess.SetPriority(Value: TProcessPriority);
begin
  OsError.Check(Windows.SetPriorityClass(Self.Handle.Value, SilOfProcess.PriorityToWindows(Value)), 'Windows.SetPriorityClass');
end;

function TSilWindowsProcess.Terminate( Timeout: Cardinal ): boolean;
var
  hProc, hToken: THandle;
begin
  Result := false;

  if (Pid > 0) then
  begin
    {if ( Timeout > 0 ) then
    begin
      Windows.PostThreadMessage( FProcessInfo.dwThreadId, WM_CLOSE, 0, 0 );
    end;}
    hProc := Windows.OpenProcess(PROCESS_TERMINATE, false, Pid);

    if Windows.OpenProcessToken(GetCurrentProcess, TOKEN_ADJUST_PRIVILEGES, hToken) then
      SilOfProcess.EnablePrivilege(hToken, 'SeDebugPrivilege', true);

    if hProc > 0 then
    begin
      Result := Windows.TerminateProcess(hProc, 0);
      Windows.CloseHandle(hProc);
    end;
  end;
end;

function TSilWindowsProcess.GetExitCode: LongWord;
begin
  if not Assigned(Handle) or not Windows.GetExitCodeProcess(Handle.Value, Result) then
    Result := 0;
end;

end.

