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

unit SilLmLoggers;

{$I Defines.inc}

interface

uses
  SilLkInterfaced,

  SilLeTrace,
  SilLiTrace,
  SilLiParameters,
  SilLiLoggers,
  SilLiPointerList,
  SilLiGlobalServices,
  SilOsTypes,
  SilLkLogger;

type
  TLoggerItem = class;
  
  TLogginManager = class(
  // extends
    TSilInterfacedObject,
  // implements
    ILogginManager)
  private
    FLoggers: IPointerList;
    FInitialized: Boolean;
    procedure DoDestroy;
    procedure DoInitialize;
    procedure DoFinalize;
    procedure DoRemove(const Item: TLoggerItem);
    function DoLoggerExists(const Logger: LoggerType): Boolean;
    function DoFindLogger(const Logger: LoggerType; out Item: TLoggerItem): Boolean;
  public
    constructor Create;
    destructor Destroy; override;
  public
    procedure Log(const Stamp: TDateTime; const Kind: TTraceKind; const Trace: ITraceData; const Text, Sender, Category: string);
    procedure Add(const Logger: LoggerType; const Formatter: FormatterType; const Params: IParameters);
    procedure Remove(const Logger: LoggerType);
  end;

  TLoggerItem = class
    Logger: LoggerType;
    Formatter: FormatterType;
    Params: IParameters;
    LoggerRef: IUnknown;
    constructor Create(const Logger: LoggerType; const Formatter: FormatterType; const Params: IParameters);
  end;

implementation

uses
  SilBtVart,
  SilBtStr,
  SilBtDateTime,
  SilBtVoidPtr,
  SilLiEnumerator,
  SilLtGlobal,
  SilLtTool,
  SilLtList,
  SilOtTool,
  SilLmTrace,
  SilLvLogginManager;

{ TLogginManager }

constructor TLogginManager.Create;
begin
  inherited Create;
  FLoggers := ListTool.PointerList(True, ObjectHandler);
  FInitialized := false;
end;

destructor TLogginManager.Destroy;
begin
  DoDestroy;
  FLoggers := nil;
  inherited;
end;

procedure TLogginManager.DoDestroy;
var
  Enum: IEnumerator;
  Item: TLoggerItem;
begin
  while FLoggers.Enumerate(Enum, Item) do
    DoRemove(Item);
end;

procedure TLogginManager.DoInitialize;
begin
  FInitialized := True;
end;

procedure TLogginManager.DoFinalize;
begin
  if FInitialized then
  try
    DoDestroy;
  except
  end;
  FInitialized := False;
end;

function TLogginManager.DoLoggerExists(const Logger: LoggerType): Boolean;
var
  Item: TLoggerItem;
begin
  Result := DoFindLogger(Logger, Item);
end;

procedure TLogginManager.DoRemove(const Item: TLoggerItem);
begin
  if Assigned(Item) then
  begin
    Item.Logger.Finalize;
    Global.Services.Release(Item.Logger, @Item.LoggerRef);
    FLoggers.Remove(Item);
    if FLoggers.Count = 0 then
      DoFinalize;
  end;
end;

function TLogginManager.DoFindLogger(const Logger: LoggerType; out Item: TLoggerItem): Boolean;
var
  Enum: IEnumerator;
begin
  Result := false;
  
  while not Result and FLoggers.Enumerate(Enum, Item) do
    Result := GUID.IsEqual(Item.Logger.ID, Logger.ID);
    
end;

procedure TLogginManager.Add(const Logger: LoggerType; const Formatter: FormatterType; const Params: IParameters);
var
  Item: TLoggerItem;
begin
  if Assigned(Logger) and not DoLoggerExists(Logger) then
  begin                                        
    if not FInitialized then DoInitialize;
    Item := TLoggerItem.Create(Logger, Formatter, Params);
    Global.Services.Get(Logger, IUnknown, @Item.LoggerRef, ClassType);
    FLoggers.Add(Item);
    Item.Logger.Initialize(Item.Params);
  end;
end;

procedure TLogginManager.Remove(const Logger: LoggerType);
var
  Item: TLoggerItem;
begin
  if Assigned(Logger) then
  begin
    if DoFindLogger(Logger, Item) then
      DoRemove(Item);
  end else
    DoDestroy;
end;

procedure TLogginManager.Log(const Stamp: TDateTime; const Kind: TTraceKind; const Trace: ITraceData; const Text, Sender, Category: string);
var
  Enum: IEnumerator;
  Item: TLoggerItem;
begin
  if FLoggers.Count > 0 then
    while FLoggers.Enumerate(Enum, Item) do
    try
      Item.Logger.Log(Stamp, Kind, Trace, Text, Sender, Category, Item.Formatter);
    except
    end;
end;

{ TLoggerItem }

constructor TLoggerItem.Create(const Logger: LoggerType; const Formatter: FormatterType; const Params: IParameters);
begin
  Self.Logger := Logger;
  Self.Formatter := Formatter;
  Self.Params := Params;
end;

end.
