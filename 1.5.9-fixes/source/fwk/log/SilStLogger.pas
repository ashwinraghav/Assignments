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

unit SilStLogger;

{$I Defines.inc}

interface

uses
  SilBkTool,
  SilLiKey,
  SilLiTrace,
  SilLiLoggers,
  SilOiModule,
  SilLkLogger,
  SilLtGlobal;

type
  Logger = class (GlobalService)
  protected
    class function DoLoadDebug(const Key: INamedKey; CreateDefaults: Boolean): Boolean;
    class function DoLoadParams(const Key: INamedKey): INamedKey;
    class function DoLoadConfig: Boolean;
  public
    class function ID: TGuid; override;
    class function Create: IUnknown; override;
    class procedure Initialize(const Key: INamedKey; Service: LoggerType; Formatter: FormatterType; Ptr: Pointer = nil); overload;
    class procedure Initialize(const Key: string; Service: LoggerType; Formatter: FormatterType; Ptr: Pointer = nil); overload;
    class procedure Finalize;
    class function SocketClient(Port: Word = 0; Address: String = 'localhost'; Module: String = ''; Format: FormatterType = nil): ILogginManager;
  end;

implementation

uses
  Sil,
  SilTool,
  SilSmLogSocketClient,
  SilStFormatterStack;

const
  CLogger: TGUID = '{BA09CBF5-8986-449D-BCCD-F8ECB63A26B8}';
    
const
  SEnabled      = 'Enabled';
  SLevel        = 'Level';
  SFlags        = 'Flags';
  
  SDebugTracing = 'Flags.Tracing';
  SDebugLibrary = 'Flags.Library';
  SDebugCalls   = 'Flags.EnterLeave';
  SOnErrorForce = 'Flags.OnErrorForceEnter';
  SOnTraceForce = 'Flags.OnTraceForceEnter';

type
  TLoggerData = class;
  ILoggerData = interface
    ['{6F89C69D-3EBF-417F-B5F6-DFFE7D4A9424}']
    function GetData: TLoggerData;
    property Data: TLoggerData read GetData;
  end;

  TLoggerData = class(
    TSilObject,
    ILoggerData )
  private
    FInitCount: Integer;
    FTrace: ITrace;
    FService: LoggerType;
    FFormatter: FormatterType;
  protected
    function GetData: TLoggerData;
  public
    destructor Destroy; override;
  end;

var
  MInstance: ILoggerData = nil;
  MInitCount: Integer = 0;
  MInitialized: Boolean = False;
  MConfig: INamedKey = nil;
  MProcess: IProcess = nil;
  MModule: IModule = nil;

(*)
type
  TConfigChanged = class(TSilObject, IEvNamedKeyChanged)
  protected
    procedure OnChanged(const Key: INamedKey);
  public
    constructor Create(Key: INamedKey);
    destructor Destroy; override;
  end;
(*)  

{ Logger }

class function Logger.Create: IUnknown;
begin
  Result := TLoggerData.Create;
end;

class function Logger.ID: TGuid;
begin
  Result := CLogger;
end;

class procedure Logger.Initialize(const Key: string; Service: LoggerType; Formatter: FormatterType; Ptr: Pointer);
begin
  try
    Initialize(Os.Registry.Open(Key, True), Service, Formatter, Ptr);
  except
  end;
end;

class procedure Logger.Initialize(const Key: INamedKey; Service: LoggerType; Formatter: FormatterType; Ptr: Pointer);
begin
  if Sil.Os.Locked.Increment(MInitCount) = 1 then
  try
    SilTool.Sv.SharedObject.Initialize(Key);

    Sil.Os.Environment.Load(Key);
    MConfig := Key.Keys.Get('Logger', True);
    MProcess := Sil.Os.Process.Current();
    if Ptr = nil then Ptr := Self;
    MModule := Sil.Os.Module.Get(Ptr);

    DoLoadConfig();
    (*)TConfigChanged.Create(MConfig);(*)

    Global.Services.Get(Self, ILoggerData, @MInstance);
    if Sil.Os.Locked.Increment(MInstance.Data.FInitCount) = 1 then
    begin
      MInstance.Data.FService := Service;
      MInstance.Data.FFormatter := Formatter;

      if Sil.Debug.Enabled then
      begin
        Sil.Trace.Define(MInstance.Data.FService, MInstance.Data.FFormatter, MInstance.Data.FService.ReadConfig(DoLoadParams(MConfig)));
        MInstance.Data.FTrace := Sil.Trace.Enter(MModule.Info.Name);
      end else
        MInstance.Data.FTrace := nil;
    end;

    MInitialized := True;
  except
  end;
end;

class procedure Logger.Finalize;
begin
  if MInitialized and (Sil.Os.Locked.Decrement(MInitCount) = 0)  then
  try
    try
      if Sil.Os.Locked.Decrement(MInstance.Data.FInitCount) = 0 then
      begin
        Sil.Trace.Leave;
        MInstance.Data.FTrace := nil;
        Sil.Trace.Undefine(MInstance.Data.FService);
        MInstance.Data.FService := nil;
        MInstance.Data.FFormatter := nil;
      end;
    finally
      Global.Services.Release(Self, @MInstance);
      MConfig := nil;
      MModule := nil;
      MProcess := nil;
      MInitialized := False;
      SilTool.Sv.SharedObject.Finalize();
    end;
  except
  end;
end;

class function Logger.DoLoadConfig: Boolean;
var
  Debug, Temp: INamedKey;
begin
  if Assigned(MConfig) then
  begin
    Debug := MConfig.Keys.Get('Debug', True);
    DoLoadDebug(Debug, true);

    if Assigned(MProcess)  then
    begin
      Temp := Debug.Keys.Get(Sil.Os.FileSystem.ChangeFileExt(MProcess.Info.Name, ''), True);
      DoLoadDebug(Temp, false);
      if Assigned(MModule) then
      begin
        Temp := Temp.Keys.Get(MModule.Info.Name, True);
        DoLoadDebug(Temp, false);
      end;
    end;
    
    if IsLibrary and Assigned(MModule) then
    begin
      Temp := Debug.Keys.Get(MModule.Info.Name, True);
      DoLoadDebug(Temp, false);
      if Assigned(MProcess) then
      begin
        Temp := Temp.Keys.Get(Sil.Os.FileSystem.ChangeFileExt(MProcess.Info.Name, ''), True);
        DoLoadDebug(Temp, false);
      end;
    end;
  end;
  Result := Sil.Debug.Enabled;
end;

procedure CheckFlag(const Key: INamedKey; const Name: string; Flag: Cardinal);
begin
  if Key.Values.Exists(Name) then
    if Key.Values.ReadBoolean(Name) then
      Sil.Debug.Flags := Sil.Debug.Flags or Flag else
      Sil.Debug.Flags := Sil.Debug.Flags and not Flag
  else if not Key.Values.Exists('#'+Name) then
    Key.Values.WriteBoolean('#'+Name, Sil.Debug.Flags and Flag <> 0);
end;

class function Logger.DoLoadDebug(const Key: INamedKey; CreateDefaults: Boolean): Boolean;
var
  Enum: IEnumerator;
  Dummy: string;
begin
  Result := CreateDefaults or Key.Values.Enumerate(Enum, Dummy);

  if Result then
  begin
    if CreateDefaults or Key.Values.Exists(SEnabled) then
      Sil.Debug.Enabled := Key.Values.ReadBoolean(SEnabled, false, CreateDefaults);

    if CreateDefaults or Key.Values.Exists(SLevel) then
      Sil.Debug.Level := Cardinal(Key.Values.ReadInteger(SLevel, 0, CreateDefaults));

    if CreateDefaults or Key.Values.Exists(SFlags) then
      Sil.Debug.Flags := Cardinal(Key.Values.ReadLargeInt(SFlags, $ffffffff, CreateDefaults));

    CheckFlag(Key, SDebugTracing, DebugTracing);
    CheckFlag(Key, SDebugLibrary, DebugLibrary);
    CheckFlag(Key, SDebugCalls, DebugCalls);
    CheckFlag(Key, SOnErrorForce, DebugOnErrorForceEnter);
    CheckFlag(Key, SOnTraceForce, DebugOnTraceForceEnter);
  end;
end;

class function Logger.DoLoadParams(const Key: INamedKey): INamedKey;
begin
  Result := Key.Keys.Get('Parameters', True);

  if Assigned(MProcess) and Result.Keys.Exists(MProcess.Info.Name) then
    Result := Result.Keys.Get(MProcess.Info.Name);

  if Assigned(MModule) and Result.Keys.Exists(MModule.Info.Name) then
    Result := Result.Keys.Get(MModule.Info.Name);
end;

class function Logger.SocketClient(Port: Word; Address: String; Module: String; Format: FormatterType): ILogginManager;
begin
  if Str.IsEmpty(Module) then
  begin
    Module := OS.FileSystem.ChangeFileExt(OS.Process.Current.Info.Name, '');
    if IsLibrary then
      Module := Module + '-' + OS.FileSystem.ChangeFileExt(OS.FileSystem.GetFileName(Os.Module.Current.Info.Name), '');
  end;

  if not Assigned(Format) then Format := StackFmt;
  Result := TSilLogSocketClient.Create(Address, Port, Module, Format);
end;

{ TConfigChanged }
(*)
constructor TConfigChanged.Create(Key: INamedKey);
begin
  inherited Create;
  Sil.Sink.Connect(Key, Self, True);
end;

destructor TConfigChanged.Destroy;
begin
  inherited;
end;

procedure TConfigChanged.OnChanged(const Key: INamedKey);
begin
  Logger.DoLoadConfig();
end;
(*)
{ TLoggerData }

destructor TLoggerData.Destroy;
begin
  inherited;
end;

function TLoggerData.GetData: TLoggerData;
begin
  Result := Self;
end;

end.
