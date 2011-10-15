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

unit SilLtTrace;

{$I Defines.inc}

interface

uses
  SilBeTypes,
  SilBeError,
  SilBtMem,
  SilBkTool,
  SilLiTrace,
  SilLkLogger,
  SilLiParameters,
  SilLiLoggers;

const
  SEmpty = '';

type
  TraceClass = class of Trace;
  Trace = class(Tool)
    class procedure LogginManager(const Manager: ILogginManager);
    class procedure Define(const Logger: LoggerType; const Format: FormatterType = nil; const Params: IParameters = nil);
    class procedure Undefine(const Logger: LoggerType = nil);
    class function Current: ITrace;
    class function EnterFmt(const Name: string; const Args: array of const; const Params: array of Variant; const Sender: string; const Category: string): ITrace; overload;
    class function EnterFmt(const Name: string; const Args: array of const; const Params: array of Variant; const Sender: string): ITrace; overload;
    class function EnterFmt(const Name: string; const Args: array of const; const Params: array of Variant): ITrace; overload;
    class function EnterFmt(const Name: string; const Args: array of const; const Sender: string; const Category: string): ITrace; overload;
    class function EnterFmt(const Name: string; const Args: array of const; const Sender: string): ITrace; overload;
    class function EnterFmt(const Name: string; const Args: array of const): ITrace; overload;
    class function EnterFmt(Caller: TObject; const Name: string; const Args: array of const; const Params: array of Variant; const Sender: string; const Category: string): ITrace; overload;
    class function EnterFmt(Caller: TObject; const Name: string; const Args: array of const; const Params: array of Variant; const Sender: string): ITrace; overload;
    class function EnterFmt(Caller: TObject; const Name: string; const Args: array of const; const Params: array of Variant): ITrace; overload;
    class function EnterFmt(Caller: TObject; const Name: string; const Args: array of const; const Sender: string; const Category: string): ITrace; overload;
    class function EnterFmt(Caller: TObject; const Name: string; const Args: array of const; const Sender: string): ITrace; overload;
    class function EnterFmt(Caller: TObject; const Name: string; const Args: array of const): ITrace; overload;
    class function EnterFmt(Caller: TClass; const Name: string; const Args: array of const; const Params: array of Variant; const Sender: string; const Category: string): ITrace; overload;
    class function EnterFmt(Caller: TClass; const Name: string; const Args: array of const; const Params: array of Variant; const Sender: string): ITrace; overload;
    class function EnterFmt(Caller: TClass; const Name: string; const Args: array of const; const Params: array of Variant): ITrace; overload;
    class function EnterFmt(Caller: TClass; const Name: string; const Args: array of const; const Sender: string; const Category: string): ITrace; overload;
    class function EnterFmt(Caller: TClass; const Name: string; const Args: array of const; const Sender: string): ITrace; overload;
    class function EnterFmt(Caller: TClass; const Name: string; const Args: array of const): ITrace; overload;
    class function Enter(const Name: string; const Params: array of Variant; const Sender: string; const Category: string): ITrace; overload;
    class function Enter(const Name: string; const Params: array of Variant; const Sender: string): ITrace; overload;
    class function Enter(const Name: string; const Params: array of Variant): ITrace; overload;
    class function Enter(const Name: string; const Sender: string = SEmpty; const Category: string = SEmpty): ITrace; overload;
    class function Enter(Caller: TObject; const Name: string; const Sender: string = SEmpty; const Category: string = SEmpty): ITrace; overload;
    class function Enter(Caller: TObject; const Name: string; const Params: array of Variant; const Sender: string; const Category: string): ITrace; overload;
    class function Enter(Caller: TObject; const Name: string; const Params: array of Variant; const Sender: string): ITrace; overload;
    class function Enter(Caller: TObject; const Name: string; const Params: array of Variant): ITrace; overload;
    class function Enter(Caller: TClass; const Name: string; const Sender: string = SEmpty; const Category: string = SEmpty): ITrace; overload;
    class function Enter(Caller: TClass; const Name: string; const Params: array of Variant; const Sender: string; const Category: string): ITrace; overload;
    class function Enter(Caller: TClass; const Name: string; const Params: array of Variant; const Sender: string): ITrace; overload;
    class function Enter(Caller: TClass; const Name: string; const Params: array of Variant): ITrace; overload;
    class procedure Leave(const Message: string = SEmpty); overload;
    class procedure Leave(const Param: Variant); overload;
    class procedure Leave(const Message: string; const Args: array of const); overload;
    class procedure Exit(const Message: string = SEmpty);
    class procedure Log(const Message: string; const Args: array of const; const Sender: string; const Category: string); overload;
    class procedure Log(const Message: string; const Args: array of const; const Sender: string); overload;
    class procedure Log(const Message: string; const Args: array of const); overload;
    class procedure Log(const Message: string; const Sender: string = SEmpty; const Category: string = SEmpty); overload;
    class procedure Log(const Message: string; const Params: IParameters; const Sender: string = SEmpty; const Category: string = SEmpty); overload;
    class procedure Error(const Message: string; const Args: array of const; const Sender: string; const Category: string); overload;
    class procedure Error(const Message: string; const Args: array of const; const Sender: string); overload;
    class procedure Error(const Message: string; const Args: array of const); overload;
    class procedure Error(const Message: string; const Sender: string = SEmpty; const Category: string = SEmpty); overload;
    class procedure Error(const Message: PResStringRec; const Args: array of const; const Sender: string; const Category: string); overload;
    class procedure Error(const Message: PResStringRec; const Args: array of const; const Sender: string); overload;
    class procedure Error(const Message: PResStringRec; const Args: array of const); overload;
    class procedure Error(const Message: PResStringRec; const Sender: string = SEmpty; const Category: string = SEmpty); overload;
    class procedure Error(Ex: Exception; const Sender: string = SEmpty; const Category: string = SEmpty); overload;
    class procedure Exception(const Name: string = SEmpty; const Sender: string = SEmpty; const Category: string = SEmpty); overload; 
    class procedure Exception(const Name: string; const Args: array of const; const Sender: string = SEmpty; const Category: string = SEmpty); overload; 
    class function Prototype(const Name: string): string; overload;
    class function Prototype(const Name: string; const Args: array of Variant): string; overload;
  end;

implementation

uses
  SilLiEnumerator,
  SilLdTrace,
  SilLmTrace,
  SilBtInt,
  SilBtStr,
  SilOfExceptFrame,
  SilBtVart;

const
  ccDot   = '.';

function CallerName(Caller: TClass): string; overload; 
begin
  if Assigned(Caller) then
    Result := Caller.ClassName else
    Result := '';
end;

function CallerName(Caller: TObject): string; overload; 
begin
  if Assigned(Caller) then
    Result := CallerName(Caller.ClassType) + '(' + Int.ToHex(LongWord(Caller), 8) + ')' else
    Result := '';
end;

function CallerName(Caller: TObject; const Name: string): string; overload;
begin
  Result := CallerName(Caller) + ccDot + Name;
end;

function CallerName(Caller: TClass; const Name: string): string; overload;
begin
  Result := CallerName(Caller) + ccDot + Name;
end;

{ Trace }

class procedure Trace.Define(const Logger: LoggerType; const Format: FormatterType; const Params: IParameters);
begin
  SilLmTrace.Define(Logger, Format, Params);
end;

class procedure Trace.Undefine(const Logger: LoggerType);
begin
  SilLmTrace.Undefine(Logger);
end;

class procedure Trace.LogginManager(const Manager: ILogginManager);
begin
  SilLmTrace.LogginManager(Manager);
end;

class function Trace.Current: ITrace;
begin
  Result := SilLmTrace.Current;
end;

class function Trace.Enter(const Name: string; const Sender, Category: string): ITrace;
begin
  Result := Enter(Name, [Vart.Unassigned], Sender, Category);
end;

class function Trace.Enter(const Name: string; const Params: array of Variant): ITrace;
begin
  Result := Enter(Name, Params, SEmpty);
end;

class function Trace.Enter(const Name: string; const Params: array of Variant; const Sender: string): ITrace;
begin
  Result := Enter(Name, Params, Sender, SEmpty);
end;

class function Trace.Enter(Caller: TObject; const Name: string; const Sender: string = SEmpty; const Category: string = SEmpty): ITrace;
begin
  Result := Enter(CallerName(Caller, Name), Sender, Category);
end;

class function Trace.Enter(Caller: TObject; const Name: string; const Params: array of Variant; const Sender: string; const Category: string): ITrace;
begin
  Result := Enter(CallerName(Caller, Name), Params, Sender, Category);
end;

class function Trace.Enter(Caller: TObject; const Name: string; const Params: array of Variant; const Sender: string): ITrace;
begin
  Result := Enter(CallerName(Caller, Name), Params, Sender);
end;

class function Trace.Enter(Caller: TObject; const Name: string; const Params: array of Variant): ITrace;
begin
  Result := Enter(CallerName(Caller, Name), Params);
end;

class function Trace.Enter(Caller: TClass; const Name, Sender, Category: string): ITrace;
begin
  Result := Enter(CallerName(Caller, Name), Sender, Category);
end;

class function Trace.Enter(Caller: TClass; const Name: string; const Params: array of Variant; const Sender, Category: string): ITrace;
begin
  Result := Enter(CallerName(Caller, Name), Params, Sender, Category);
end;

class function Trace.Enter(Caller: TClass; const Name: string; const Params: array of Variant; const Sender: string): ITrace;
begin
  Result := Enter(CallerName(Caller, Name), Params, Sender);
end;

class function Trace.Enter(Caller: TClass; const Name: string; const Params: array of Variant): ITrace;
begin
  Result := Enter(CallerName(Caller, Name), Params);
end;

class function Trace.Enter(const Name: string; const Params: array of Variant; const Sender, Category: string): ITrace;
begin
  Result := SilLmTrace.Enter(Name, Params, SilOfExceptFrame.GetHead(), Sender, Category);
end;

class function Trace.EnterFmt(const Name: string; const Args: array of const; const Params: array of Variant; const Sender, Category: string): ITrace;
begin
  Result := Enter(Str.Format(Name, Args), Params, Sender, Category);
end;

class function Trace.EnterFmt(const Name: string; const Args: array of const; const Sender, Category: string): ITrace;
begin
  Result := Enter(Str.Format(Name, Args), Sender, Category);
end;

class function Trace.EnterFmt(const Name: string; const Args: array of const; const Sender: string): ITrace;
begin
  Result := Self.EnterFmt(Name, Args, Sender, SEmpty);
end;

class function Trace.EnterFmt(const Name: string; const Args: array of const): ITrace;
begin
  Result := Self.EnterFmt(Name, Args, SEmpty);
end;

class function Trace.EnterFmt(const Name: string; const Args: array of const; const Params: array of Variant): ITrace;
begin
  Result := Self.EnterFmt(Name, Args, Params, SEmpty);
end;

class function Trace.EnterFmt(const Name: string; const Args: array of const; const Params: array of Variant; const Sender: string): ITrace;
begin
  Result := Self.EnterFmt(Name, Args, Params, Sender, SEmpty);
end;

class function Trace.EnterFmt(Caller: TObject; const Name: string; const Args: array of const; const Params: array of Variant; const Sender: string; const Category: string): ITrace;
begin
  Result := EnterFmt(CallerName(Caller, Name), Args, Params, Sender, Category);
end;

class function Trace.EnterFmt(Caller: TObject; const Name: string; const Args: array of const; const Params: array of Variant; const Sender: string): ITrace;
begin
  Result := EnterFmt(CallerName(Caller, Name), Args, Params, Sender);
end;

class function Trace.EnterFmt(Caller: TObject; const Name: string; const Args: array of const; const Params: array of Variant): ITrace;
begin
  Result := EnterFmt(CallerName(Caller, Name), Args, Params);
end;

class function Trace.EnterFmt(Caller: TObject; const Name: string; const Args: array of const; const Sender: string; const Category: string): ITrace;
begin
  Result := EnterFmt(CallerName(Caller, Name), Args, Sender, Category);
end;

class function Trace.EnterFmt(Caller: TObject; const Name: string; const Args: array of const; const Sender: string): ITrace;
begin
  Result := EnterFmt(CallerName(Caller, Name), Args, Sender);
end;

class function Trace.EnterFmt(Caller: TObject; const Name: string; const Args: array of const): ITrace; 
begin
  Result := EnterFmt(CallerName(Caller, Name), Args);
end;

class function Trace.EnterFmt(Caller: TClass; const Name: string; const Args: array of const; const Params: array of Variant; const Sender, Category: string): ITrace;
begin
  Result := EnterFmt(CallerName(Caller, Name), Args, Params, Sender, Category);
end;

class function Trace.EnterFmt(Caller: TClass; const Name: string; const Args: array of const; const Params: array of Variant; const Sender: string): ITrace;
begin
  Result := EnterFmt(CallerName(Caller, Name), Args, Params, Sender);
end;

class function Trace.EnterFmt(Caller: TClass; const Name: string; const Args: array of const; const Params: array of Variant): ITrace;
begin
  Result := EnterFmt(CallerName(Caller, Name), Args, Params);
end;

class function Trace.EnterFmt(Caller: TClass; const Name: string; const Args: array of const; const Sender, Category: string): ITrace;
begin
  Result := EnterFmt(CallerName(Caller, Name), Args, Sender, Category);
end;

class function Trace.EnterFmt(Caller: TClass; const Name: string; const Args: array of const; const Sender: string): ITrace;
begin
  Result := EnterFmt(CallerName(Caller, Name), Args, Sender);
end;

class function Trace.EnterFmt(Caller: TClass; const Name: string; const Args: array of const): ITrace;
begin
  Result := EnterFmt(CallerName(Caller, Name), Args);
end;

class procedure Trace.Leave(const Message: string);
begin
  SilLmTrace.Leave(Message);
end;

class procedure Trace.Leave(const Param: Variant);
begin
  SilLmTrace.Leave(Vart.ToStr(Param, SEmpty));
end;

class procedure Trace.Leave(const Message: string; const Args: array of const);
begin
  SilLmTrace.Leave(Message, Args);
end;

class procedure Trace.Exit(const Message: string = SEmpty);
begin
  SilLmTrace.Leave(Message);
end;

class procedure Trace.Error(const Message: string; const Args: array of const; const Sender, Category: string);
begin
  SilLmTrace.Error(Message, Args, Sender, Category);
end;

class procedure Trace.Error(const Message: string; const Args: array of const; const Sender: string);
begin
  SilLmTrace.Error(Message, Args, Sender, SEmpty);
end;

class procedure Trace.Error(const Message: string; const Args: array of const);
begin
  SilLmTrace.Error(Message, Args, SEmpty, SEmpty);
end;

class procedure Trace.Error(const Message: string; const Sender, Category: string);
begin
  SilLmTrace.Error(Message, [], Sender, Category);
end;

class procedure Trace.Error(const Message: PResStringRec; const Args: array of const; const Sender, Category: string);
begin
  SilLmTrace.Error(System.LoadResString(Message), Args, Sender, Category);
end;

class procedure Trace.Error(const Message: PResStringRec; const Args: array of const; const Sender: string);
begin
  SilLmTrace.Error(System.LoadResString(Message), Args, Sender, SEmpty);
end;

class procedure Trace.Error(const Message: PResStringRec; const Args: array of const);
begin
  SilLmTrace.Error(System.LoadResString(Message), Args, SEmpty, SEmpty);
end;

class procedure Trace.Error(const Message: PResStringRec; const Sender, Category: string);
begin
  SilLmTrace.Error(System.LoadResString(Message), [], Sender, Category);
end;

class procedure Trace.Error(Ex: Exception; const Sender, Category: string);
begin
  SilLmTrace.Error(STraceError, [Ex.ClassName, Ex.Message], Sender, Category);
end;

class procedure Trace.Exception(const Name, Sender, Category: string);
var
  Ex: TObject;
{$IFNDEF D60}
  RaisePtr: Pointer;
{$ENDIF}
  From: string;
begin
{$IFDEF D60}
  Ex := System.AcquireExceptionObject;
{$ELSE}
  RaisePtr := RaiseList;

  if Assigned(RaisePtr) then
    Ex := PRaiseFrame(RaisePtr)^.ExceptObject else
    Ex := nil;
{$ENDIF}

  if Assigned(Ex) then
  begin
    if Str.NotEmpty(Name) then
      From := Name else
      From := Current.Data.Name;

    if Ex is SilBeError.Exception then
      Self.Error(STraceExceptionClassFrom, [Ex.ClassName, SilBeError.Exception(Ex).Message, From]) else
      Self.Error(STraceUnknownExceptionFrom, [From]);

{$IFDEF D60}
    ReleaseExceptionObject;
{$ELSE}
    PRaiseFrame(RaisePtr)^.ExceptObject := nil;
{$ENDIF}
  end else
    Self.Error(SUnknownErrorFrom, [From]);
end;

class procedure Trace.Exception(const Name: string; const Args: array of const; const Sender, Category: string);
begin
  Exception(Str.Format(Name, Args), Sender, Category);
end;

class procedure Trace.Log(const Message: string; const Args: array of const; const Sender, Category: string);
begin
  SilLmTrace.Log(Message, Args, Sender, Category);
end;

class procedure Trace.Log(const Message: string; const Sender, Category: string);
begin
  SilLmTrace.Log(Message, Sender, Category);
end;

class procedure Trace.Log(const Message: string; const Params: IParameters; const Sender: string; const Category: string);
var
  Enum: IEnumerator;
  Item: RParameter;
  Buf: String;
begin
  Buf := Message + Str.IIf(Params.Count > 0, Str.Format(' params: (%d)', [Params.Count]));

  while Params.Enumerate(Enum, Item) do
    if Vart.VType(Item.Value) <> varUnknown then
      Buf := Buf + ' ' + Item.Name + '=' + Vart.ToStr(Item.Value);

  SilLmTrace.Log(Buf, Sender, Category);
end;

class function Trace.Prototype(const Name: string): string;
begin
  Result := Prototype(Name, [Vart.Unassigned]);
end;

class function Trace.Prototype(const Name: string; const Args: array of Variant): string;
var
  I: Integer;
  S: string;
begin
  S := SEmpty;

  for I := Low(Args) to High(Args) do
  begin
    if S <> SEmpty then S := S + ', ';
    S := S + Vart.ToStr(Args[I]);
  end;

  Result := Name + '(' + S + ')';
end;

class procedure Trace.Log(const Message: string; const Args: array of const);
begin
  Self.Log(Message, Args, SEmpty);
end;

class procedure Trace.Log(const Message: string; const Args: array of const; const Sender: string);
begin
  Self.Log(Message, Args, Sender, SEmpty);
end;

end.
