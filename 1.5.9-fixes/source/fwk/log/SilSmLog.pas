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

unit SilSmLog;

{$I Defines.inc}

interface

uses
  SilLkInterfaced,
  
  SilLeTrace,
  SilLiTrace,
  SilLiParameters,
  SilOiFile,
  SilOiTextFile,
  SilOiIpc,
  SilLkLogger,
  SilSeLog,
  SilSiLog;

type
  TLogService = class(
    TSilInterfacedObject,
    ILogService )
  private
    FFile: ITextFile;
    FLogBase: string;
    FLogName: string;
    FLogExt: string;
    FParams: IParameterList;
    FParamDateFormat: String;
    FParamAutoFlush: Boolean;
    FParamLogSize: LongWord;
    FParamLogSplit: Boolean;
    FParamLogCount: LongWord;
    FMutex: IMutex;
    function NewFile: string;
    function FormatBuffer(const Stamp: TDateTime; const Kind: TTraceKind; const Trace: ITraceData; const Text, Sender, Category: string; Formatter: FormatterType = nil): string;
    procedure CheckLog;
    procedure SetLogFile(FileName: string);
    function DoGetOldest(const FilePath: String): String;
    procedure WriteBuffer(const Buffer: string);
  protected // ILogService
    function GetParams: IParameters;
    procedure SetParams(const Params: IParameters);
    procedure Log(const Stamp: TDateTime; const Kind: TTraceKind; const Trace: ITraceData; const Text, Sender, Category: string; Formatter: FormatterType = nil);
  public
    constructor Create;
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils,
  SilLmParameters,
  SilBtDateTime,
  SilBcChr,
  SilBtStr,
  SilBtVart,
  SilLiStream,
  SilLiEnumerator,
  SilOiThread,
  SilLtSort,
  SilOtTool,
  SilScLog,
  SilSgLog,
  SilStLog;

function Spc(const Txt: string; const Sep: string): string;
begin
  Result := Txt;
  if Str.NotEmpty(Result) then
    Result := Result + Sep;
end;

{ TLogService }

constructor TLogService.Create;
begin
  inherited Create;

  FParams := TSilParameters.Create;
  FParamDateFormat := ShortDateFormat;
  FParamAutoFlush := false;
  FParamLogSize := 1024 * 1024 * 2;
  FParamLogSplit := false;
  FParamLogCount := 10;
end;

destructor TLogService.Destroy;
begin
  FFile := nil;
  FParams := nil;

  inherited;
end;

procedure TLogService.Log(const Stamp: TDateTime; const Kind: TTraceKind; const Trace: ITraceData; const Text, Sender, Category: string; Formatter: FormatterType = nil);
var
  Filter: Boolean;
begin
  try
    Filter := (Kind in [tkEnter, tkLeave]) and not FParams.Get(CShowStack, True);

    if not Filter then
      WriteBuffer(FormatBuffer(Stamp, Kind, Trace, Text, Sender, Category, Formatter));
  except end;
end;

function TLogService.GetParams: IParameters;
begin
  Result := FParams;
end;

procedure TLogService.SetParams(const Params: IParameters);
var
  Pinfo: IFileInfo;
  sFile, sName: string;
begin
  try
    FParams := Params as IParameterList;
    Pinfo := Os.Process.Current.Info;

    FParamDateFormat := FParams.Get(CDateFormat, ShortDateFormat);
    FParamAutoFlush := FParams.Get(CAutoFlush, false);
    FParamLogSize := FParams.Get(CLogSize, 0);
    FParamLogSplit := FParams.Get(CLogSplit, False);
    FParamLogCount := FParams.Get(CLogCount, 0);

    sName := FParams.Get(CLogFileName, '');
    if Str.NotEmpty(sName) then
    begin
      FLogBase := ExtractFileDir(sName) + '\';
      FLogName := ChangeFileExt(ExtractFileName(sName), '');
      FLogExt := ExtractFileExt(sName);

      if Str.ToChr(FLogName) = '.' then FLogName := '';
    end else
    begin
      FLogBase := Pinfo.Path;
      FLogName := '';
      FLogExt := '';
    end;

    if Vart.ToBool(FParams.Get(CSubdirByProcess, False)) then
    begin
      Str.Add(FLogBase, ChangeFileExt(ExtractFileName(Pinfo.Name), ''), PathDelim, False);

      if IsLibrary and Vart.ToBool(FParams.Get(CSubdirByModule, False)) then
        Str.Add(FLogBase, ChangeFileExt(ExtractFileName(Os.Module.Current.Info.Name), ''), PathDelim, False);

      FLogBase := Os.FileSystem.AddSlash(FLogBase);
      Os.FileSystem.ForceDirectories(FLogBase);
    end;

    if Str.IsEmpty(FLogName) then
    begin
      FLogName := ChangeFileExt(ExtractFileName(Pinfo.Name), '');

      if IsLibrary then
        FLogName := FLogName + '-' + ChangeFileExt(ExtractFileName(Os.Module.Current.Info.Name), '');
    end;

    if Str.IsEmpty(FLogExt) then
      FLogExt := '.log';

    if FParamLogSplit then
      sFile := NewFile else
      sFile := DoGetOldest(FLogBase + FLogName);

    SetLogFile(sFile);
  except end;
end;

function TLogService.FormatBuffer(const Stamp: TDateTime; const Kind: TTraceKind; const Trace: ITraceData; const Text, Sender, Category: string; Formatter: FormatterType = nil): string;
var
  SSender: string;
  SCategory: string;
  SText: string;
begin
  if Assigned(Formatter) then
    SText := Formatter.Format(Kind, Trace, Text) else
    SText := Text;

  if Str.NotEmpty(Sender) then
    SSender := Str.Copy(Str.PadR(Sender, 10), 1, 10) else
    SSender := Str.ReplicateChar(ccSPC, 10);

  if Str.NotEmpty(Category) then
    SCategory := Str.Copy(Str.PadR(Category, 10), 1, 10) else
    SCategory := Str.ReplicateChar(ccSPC, 10);

  Result :=   GLogKindChar[Kind] + ccSPC
            + Spc(FormatDateTime(FParamDateFormat, Stamp), ccSPC)
            + Spc(SSender, ccSPC)
            + Spc(SCategory, ccSPC)
            + SText;
end;

procedure TLogService.WriteBuffer(const Buffer: string);
begin
  if FFile <> nil then
    try
      FMutex.WaitFor;
      CheckLog;

      FFile.Stream.WriteLn(Buffer);

      if FParamAutoFlush then
        FFile.FlushBuffer;
    finally
      FMutex.Release;
    end;
end;

procedure TLogService.CheckLog;
begin
  if not Assigned(FFile) or ((FParamLogSize <> 0) and (FFile.Stream.Position >= FParamLogSize)) then
    SetLogFile(NewFile)
  else
  if Assigned(FFile) then
    FFile.Stream.Seek(0, soFromEnd);
end;

procedure TLogService.SetLogFile(FileName: string);
var
  Retries: Integer;
  MutexName: String;
begin
  Retries := 5;

  while Retries > 0 do
    try
      MutexName := 'mutex_' + OS.FileSystem.GetFileName(FileName);
      FMutex := OS.Ipc.Mutex(false, PChar(MutexName));

      FFile := OS.FileSystem.OpenTextFile(FileName, fmAccessReadWrite, fmShareReadWrite);
      FFile.Stream.Seek(0, soFromEnd);
      FParams[CCurrentFileName] := FileName;

      if not FParamLogSplit then
      begin
        FFile.Stream.Seek(0, soFromEnd);
        FFile.Stream.WriteLn();
        FFile.Stream.WriteLn('****************************************************');
        FFile.Stream.WriteLn('** New log start at: ' + DateTime.ToStr(DateTime.Now()));
        FFile.Stream.WriteLn('****************************************************');
      end;

      Break;
    except
      FileName := NewFile;
      Dec(Retries);
    end;
end;

function TLogService.NewFile: string;
begin
  try
    Result := OS.FileSystem.StampFileName(FLogBase + FLogName + FLogExt, '.yyyymmdd.hhnnss');
    OS.FileSystem.StampLimitCount(FLogBase + FLogName + FLogExt, FParamLogCount);
  except
    Result := '';
  end;
end;

function TLogService.DoGetOldest(const FilePath: String): String;
begin
  try
    if not OS.FileSystem.StampLast(FilePath + '.*' + FLogExt, Result) then
      Result := NewFile;
  except
    Result := '';
  end;
end;

end.
