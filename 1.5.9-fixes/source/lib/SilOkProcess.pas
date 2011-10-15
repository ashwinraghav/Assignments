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

unit SilOkProcess;

{$I Defines.inc}

interface

uses
  SilOsWaitable,
  SilOeProcess,
  SilOiWait,
  SilOiHandle,
  SilOiModule,
  SilOiProcess,
  SilOiFile,
  SilOkFileInfo;

type
  TSilProcess = class(
    //- extends
    TSilOsWaitableObject,
    //- implements
    IProcess )
  private
    FName: string;
    FInfo: IFileInfo; 
  protected // IProcess
    function GetPriority: TProcessPriority; virtual; abstract;
    procedure SetPriority(Value: TProcessPriority); virtual; abstract;
    function Terminate(Timeout: Cardinal): boolean; virtual; abstract;
    function GetInfo: IFileInfo; virtual;
    function GetPID: Cardinal; virtual; abstract;
    function GetMainThreadID: Cardinal; virtual; abstract;
    function GetModules: IModules; virtual; abstract;
    function GetCurrentPath: string; virtual; abstract;
    function GetExitCode: LongWord; virtual; abstract; 
  protected // properties
    property Priority: TProcessPriority read GetPriority write SetPriority;
    property Info: IFileInfo read GetInfo;
    property PID: Cardinal read GetPID;
    property MainThreadID: Cardinal read GetMainThreadID;
    property CurrentPath: string read GetCurrentPath;
  public
    constructor Create(const Handle: IHandle; const Name: string); overload; virtual;
    destructor Destroy; override;
  public
    property Name: string read FName;
  end;

  TSilEmptyProcess = class (TSilProcess)
  private
    FPriority: TProcessPriority;
    FModules: IModuleList;
  protected // IProcess
    function GetPriority: TProcessPriority; override;
    procedure SetPriority(Value: TProcessPriority); override;
    function Terminate(Timeout: Cardinal): boolean; override;
    function GetInfo: IFileInfo; override;
    function GetPID: Cardinal; override;
    function GetMainThreadID: Cardinal; override;
    function GetModules: IModules; override;
    function GetCurrentPath: string; override;
    function GetExitCode: LongWord; override;
  public
    constructor Create(const Name: string); reintroduce;
    destructor Destroy; override;
  end;

  TSilFileInfo = class (SilOkFileInfo.TSilFileInfo)
  private
    FUpdated: Boolean;
  private
    procedure DoCheckStatus;
  protected
    function GetTime: TDateTime; override;
    function GetAttributes: TFileAttributes; override;
    function GetSize: LongWord; override;
  end;

implementation

uses
  SilOtTool,
  SilOkModule;

{ TSilProcess }

constructor TSilProcess.Create(const Handle: IHandle; const Name: string);
begin
  if Assigned(Handle) then
    inherited Create(Handle) else
    inherited Create;

  FName := Name;
  FInfo := TSilFileInfo.Create(Name);
end;

destructor TSilProcess.Destroy;
begin
  FInfo := nil;
  inherited;
end;

function TSilProcess.GetInfo: IFileInfo;
begin
  Result := FInfo;
end;

{ TSilEmptyProcess }

constructor TSilEmptyProcess.Create(const Name: string);
var
  Dummy: IHandle;
begin
  inherited Create(Dummy, Name);

  FInfo := SilOkFileInfo.TSilFileInfo.Create(Name);
  FModules := TSilModuleList.Create;
  FPriority := ppNormal;
end;

destructor TSilEmptyProcess.Destroy;
begin
  FInfo := nil;
  FModules := nil;

  inherited;
end;

function TSilEmptyProcess.GetCurrentPath: string;
begin
  Result := '';
end;

function TSilEmptyProcess.GetExitCode: LongWord;
begin
  Result := 0;
end;

function TSilEmptyProcess.GetInfo: IFileInfo;
begin
  Result := FInfo;
end;

function TSilEmptyProcess.GetMainThreadID: Cardinal;
begin
  Result := 0;
end;

function TSilEmptyProcess.GetModules: IModules;
begin
  Result := FModules;
end;

function TSilEmptyProcess.GetPID: Cardinal;
begin
  Result := 0;
end;

function TSilEmptyProcess.GetPriority: TProcessPriority;
begin
  Result := FPriority;
end;

procedure TSilEmptyProcess.SetPriority(Value: TProcessPriority);
begin
  FPriority := Value;
end;

function TSilEmptyProcess.Terminate(Timeout: Cardinal): boolean;
begin
  Result := false;      
end;

{ TSilFileInfo }

procedure TSilFileInfo.DoCheckStatus;
var
  Info: IFileInfoDef;
begin
  if not FUpdated then
  begin
    FUpdated := true;
    Info := Self;
    OS.FileSystem.FillInfo(GetFullName, Info);
  end;
end;

function TSilFileInfo.GetAttributes: TFileAttributes;
begin
  DoCheckStatus;
  Result := inherited GetAttributes;
end;

function TSilFileInfo.GetSize: LongWord;
begin
  DoCheckStatus;
  Result := inherited GetSize;
end;

function TSilFileInfo.GetTime: TDateTime;
begin
  DoCheckStatus;
  Result := inherited GetTime;
end;

end.
