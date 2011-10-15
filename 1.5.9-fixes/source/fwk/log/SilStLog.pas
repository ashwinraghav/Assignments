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

unit SilStLog;

{$I Defines.inc}

interface

uses
  SilBeTypes,
  SilLeTrace,
  SilLiTrace,
  SilLiKey,
  SilLiParameters,
  SilLkLogger,
  SilSeLog,
  SilSiLog;

type
  LogService = class(Logger)
  private
    class procedure DoProcessParams(const List: IParameterList);
  public
    class function ID: TGuid; override;
    class function Create: IUnknown; override;
    class procedure Initialize(const Params: IParameters); override;
    class procedure Finalize; override;
    class function Params: IParameters;
    class procedure Log(const Stamp: TDateTime; const Kind: TTraceKind; const Trace: ITraceData; const Text, Sender, Category: string; Formatter: FormatterType = nil); override;
    class function DefParams: IParameterList; override;
    class function ReadConfig(const Path: String): IParameterList; override;
    class function ReadConfig(const Key: INamedKey): IParameterList; override;
    class function ReadConfig(const Values: INamedValues): IParameterList; override;
    class function SetConfig(const Values: INamedValues): IParameterList; override;
  end;

implementation

uses
  SilBtStr,
  SilLtGlobal,
  SilBtVart,
  SilScLog,
  SilSgLog,
  SilSmLog,
  SilLtList,
  SilLiEnumerator,
  SilOtTool;

const
  CLogService: TGUID = '{A9FC89D8-DC5E-4CBE-8190-6A1328F18637}';

var
  MService: ILogService = nil;
  MInitCount: Integer = 0;

{ LogService }

class function LogService.Create: IUnknown;
begin
  Result := TLogService.Create();
end;

class function LogService.ID: TGuid;
begin
  Result := CLogService;
end;

class procedure LogService.Initialize(const Params: IParameters);
var
  P: IParameterList;
begin
  if OS.Locked.Increment(MInitCount) = 1 then
  begin
    P := DefParams;

    if Params <> nil then
      P.Merge(Params);

    Global.Services.Get(Self, ILogService, @MService);
    MService.SetParams(Params);
  end;
end;

class procedure LogService.Finalize;
begin
  if OS.Locked.Decrement(MInitCount) = 0 then
    Global.Services.Release(Self, @MService);
end;

class function LogService.Params: IParameters;
begin
  if MService <> nil then result := MService.Params;
end;

class procedure LogService.Log(const Stamp: TDateTime; const Kind: TTraceKind; const Trace: ITraceData; const Text, Sender, Category: string; Formatter: FormatterType = nil);
begin
  if MService <> nil then
    MService.Log(Stamp, Kind, Trace, Text, Sender, Category, Formatter);
end;

class function LogService.DefParams: IParameterList;
begin
  Result := ListTool.Parameters;
{$WARNINGS OFF}
  Result[CLogSize] := GLogDefParams.LogSize;
{$WARNINGS ON}
  Result[CLogSplit] := GLogDefParams.LogSplit;
  Result[CLogCount] := GLogDefParams.LogCount;
  Result[CAutoFlush] := GLogDefParams.AutoFlush;
  Result[CDateFormat] := GLogDefParams.DateFormat;
  Result[CLogFileName] := GLogDefParams.LogFileName;
  Result[CShowStack] := GLogDefParams.ShowStack;
  Result[CSubdirByProcess] := False;
  Result[CSubdirByModule] := False;
end;

class function LogService.SetConfig(const Values: INamedValues): IParameterList;
begin
end;

class function LogService.ReadConfig(const Key: INamedKey): IParameterList;
var
  e: IEnumerator;
  Item: RParameter;
begin
  try
    Result := DefParams;

    while Result.Enumerate(e, Item) do
      case Vart.VType(Item.Value) of
        varByte,
        varWord,
        varLongWord,
        varSmallint,
        varInteger: Result[Item.Name] := Key.Values.ReadInteger(Item.Name, Item.Value, true);
        varSingle,
        varDouble,
        varCurrency,
        varDate:    Result[Item.Name] := Key.Values.ReadFloat(Item.Name, Item.Value, true);
        varBoolean: Result[Item.Name] := Key.Values.ReadBoolean(Item.Name, Item.Value, true);
        varOleStr,
        varString:  Result[Item.Name] := Key.Values.ReadString(Item.Name, Item.Value, true);
      end;
  except
    Result := nil;
  end;

  DoProcessParams(Result);
end;

class function LogService.ReadConfig(const Path: String): IParameterList;
begin
  Result := ReadConfig(OS.Registry.Open(Path, true));
end;

class function LogService.ReadConfig(const Values: INamedValues): IParameterList;
var
  e: IEnumerator;
  Item: RParameter;
begin
  try
    Result := DefParams;

    while Result.Enumerate(e, Item) do
      case Vart.VType(Item.Value) of
        varByte,
        varWord,
        varLongWord,
        varSmallint,
        varInteger: Result[Item.Name] := Values.ReadInteger(Item.Name, Item.Value, true);
        varSingle,
        varDouble,
        varCurrency,
        varDate:    Result[Item.Name] := Values.ReadFloat(Item.Name, Item.Value, true);
        varBoolean: Result[Item.Name] := Values.ReadBoolean(Item.Name, Item.Value, true);
        varOleStr,
        varString:  Result[Item.Name] := Values.ReadString(Item.Name, Item.Value, true);
      end;
  except
    Result := nil;
  end;

  DoProcessParams(Result);
end;

class procedure LogService.DoProcessParams(const List: IParameterList);
begin
  List[CLogFileName] := OS.Environment.Expand(List[CLogFileName]);
end;

end.
