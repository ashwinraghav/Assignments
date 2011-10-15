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
  libc,

  SilOkProcess,
  SilOeProcess,
  SilOiModule,
  SilOiWait,
  SilOiHandle,
  SilOiProcess,
  SilOiFile;

type
  TSilLinuxProcess = class (TSilProcess)
  private
    FMustFree: Boolean;
    FProcessInfo: Pointer; // TProcessInformation;
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
    constructor Create(const ProcessInfo: Pointer; MustFree: Boolean; const Name: string); reintroduce;
    destructor Destroy; override;
  end;

implementation

uses
(*)  Messages,(*)

  SysUtils,
//  SilOfProcess,

  SilOtTool,
  SilOsError;

{ TSilLinuxProcess }

constructor TSilLinuxProcess.Create(const ProcessInfo: Pointer; MustFree: Boolean; const Name: string);
begin
  FMustFree := MustFree;
  FProcessInfo := ProcessInfo;

  inherited Create(Os.Handle.Create(0, MustFree), Name);
end;

destructor TSilLinuxProcess.Destroy;
begin
(*)  if FMustFree then
    Windows.CloseHandle(FProcessInfo.hThread);(*)

  inherited;
end;

function TSilLinuxProcess.GetMainThreadID: Cardinal;
begin
  Result := 0;
(*)  result := FProcessInfo.dwThreadId;(*)
end;

function TSilLinuxProcess.GetModules: IModules;
begin
  Result := nil;
end;

function TSilLinuxProcess.GetCurrentPath: string;
begin
  Result := '';
end;

function TSilLinuxProcess.GetExitCode: LongWord;
begin
  Result := 0;
end;

function TSilLinuxProcess.GetPID: Cardinal;
begin
  Result := libc.getpid;
(*)  result := FProcessInfo.dwProcessId;(*)
end;

function TSilLinuxProcess.GetPriority: TProcessPriority;
begin
  Result := ppNormal;
(*)  Result := SilOfProcess.WindowsToPriority(Windows.GetPriorityClass(Handle.Value));(*)
end;

procedure TSilLinuxProcess.SetPriority(Value: TProcessPriority);
begin
(*)  OsError.Check(Windows.SetPriorityClass(Self.Handle.Value, SilOfProcess.PriorityToWindows(Value)), 'Windows.SetPriorityClass');(*)
end;

function TSilLinuxProcess.Terminate( Timeout: Cardinal ): boolean;
begin
(*)  Result := true;

  if (Pid > 0) then
  begin
    {if ( Timeout > 0 ) then
    begin
      Windows.PostThreadMessage( FProcessInfo.dwThreadId, WM_CLOSE, 0, 0 );
    end;}
    Windows.TerminateProcess(FProcessInfo.hProcess, 0);
  end;(*)
end;

end.

