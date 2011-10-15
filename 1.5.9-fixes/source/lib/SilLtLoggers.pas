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

unit SilLtLoggers;

{$I Defines.inc}

interface

uses
  SilBeTypes,
  SilBtVoidPtr,
  SilLeTrace,
  SilLiTrace,
  SilLiLoggers,
  SilLkLogger,
  SilLiParameters;

function Manager: ILogginManager;
procedure Log(const Stamp: TDateTime; const Kind: TTraceKind; const Trace: ITraceData; const Text, Sender, Category: string);
procedure Add(const Logger: LoggerType; const Formatter: FormatterType; const Params: IParameters);
procedure Remove(const Logger: LoggerType);
procedure LogginManager(const Manager: ILogginManager) {$IFDEF USE_DEPRECATED} ; deprecated {$ENDIF};

implementation

uses
  SilLtGlobal,
  SilLvLogginManager;

var
  MInstance: ILogginManager = nil;

{ Loggers }

function Manager: ILogginManager;
begin
  if not Assigned(MInstance) then
    Global.List.Get(SilLvLogginManager.Service, ILogginManager, @MInstance, FindHInstance(@MInstance));
  Result := MInstance;
end;

procedure Log(const Stamp: TDateTime; const Kind: TTraceKind; const Trace: ITraceData; const Text, Sender, Category: string);
begin
  Manager.Log(Stamp, Kind, Trace, Text, Sender, Category);
end;

procedure Add(const Logger: LoggerType; const Formatter: FormatterType; const Params: IParameters);
begin
  Manager.Add(Logger, Formatter, Params);
end;

procedure Remove(const Logger: LoggerType);
begin
  Manager.Remove(Logger);
end;

procedure LogginManager(const Manager: ILogginManager);
begin
  if Assigned(MInstance) then
    Global.List.Release(SilLvLogginManager.Service, @MInstance, FindHInstance(@MInstance));
  MInstance := Manager;
end;

initialization

finalization
  if Assigned(MInstance) then
    Global.List.Release(SilLvLogginManager.Service, @MInstance, FindHInstance(@MInstance));

end.
