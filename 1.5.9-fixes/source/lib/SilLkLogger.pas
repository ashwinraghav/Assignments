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

unit SilLkLogger;

{$I Defines.inc}

interface

uses
  SilBeMemMgr,
  SilBkTool,
  SilLeTrace,
  SilLiKey,
  SilLiTrace,
  SilLiParameters,
  SilLiGlobalServices;

type
  LoggerType = class of Logger;
  FormatterType = class of Formatter;

  Logger = class(GlobalService)
    class procedure Initialize(const Params: IParameters); virtual;
    class procedure Finalize; virtual;
    class procedure Log(const Stamp: TDateTime; const Kind: TTraceKind; const Trace: ITraceData; const Text, Sender, Category: string; Formatter: FormatterType = nil); virtual; abstract;
    class function DefParams: IParameterList; virtual; abstract;
    class function ReadConfig(const Path: String): IParameterList; overload; virtual; abstract;
    class function ReadConfig(const Key: INamedKey): IParameterList; overload; virtual; abstract;
    class function ReadConfig(const Values: INamedValues): IParameterList; overload; virtual; abstract;
    class function SetConfig(const Values: INamedValues): IParameterList; overload; virtual; abstract;
  end;

  Formatter = class(Tool)
    class function FormatMemory(const Value: RMemoryInfo): string; virtual; abstract;
    class function FormatThread(const Thread: IThreadData): string; virtual; abstract;
    class function FormatTime(const Elapsed: TDateTime): string; virtual;
    class function FormatLevel(const Kind: TTraceKind; const Level: Integer): string; virtual;
    class function Format(const Kind: TTraceKind; const Item: ITraceData; const Text: string): string; virtual; abstract;
  end;

implementation

uses
  SilLgLogger,
  SilBtStr,
  SilBtDateTime;

{ Logger }

class procedure Logger.Initialize(const Params: IParameters);
begin
end;

class procedure Logger.Finalize;
begin
end;

{ Formatter }

class function Formatter.FormatLevel(const Kind: TTraceKind; const Level: Integer): string;
begin
  Result := Str.ReplicateChar(GTraceLevelChars[Kind], Level);
end;

class function Formatter.FormatTime(const Elapsed: TDateTime): string;
begin
  with DateTime.Decode(Elapsed) do
  begin
    Result := Str.Zero(Time.Hour, 2) + ':' + Str.Zero(Time.Minutes, 2) + ':' + Str.Zero(Time.Seconds, 2) + '.' + Str.Zero(Time.mSeconds, 3);
    if (Time.Seconds > 1) or (Time.Minutes + Time.Hour > 0) then
      Result := '!! ' + Result + ' !!' else
      Result := '   ' + Result + '   ';
    Result := '[' + Result + ']'
  end;
end;

end.
 