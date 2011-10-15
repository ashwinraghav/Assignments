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

unit SilOjProcess;

{$I Defines.inc}

interface

uses
  SilBkTool,
  SilOeProcess,
  SilOiModule,
  SilOiProcess;

type
  SilProcessTool = class(Tool)
  protected 
    class function DoGetCurrent: IProcess; virtual; abstract;
  public
    class function Current: IProcess;
    class function PrevInstance(InstanceID: String = ''): Boolean; virtual; abstract;
    class function CommandLine: String; virtual;
    class function Execute(
                     const FileName: String;
                           Visibility: TProcessVisibility = pvNormal;
                           Timeout: LongWord = 0): IProcess; overload; virtual; abstract;

    class function Execute(
                     const FileName, StartDir: String;
                           Visibility: TProcessVisibility = pvNormal;
                           Timeout: LongWord = 0): IProcess; overload; virtual; abstract;
    class function GetList(const Mask: String = ''): IProcessList; virtual; abstract;
    class function Open(PID: LongWord): IProcess; virtual; abstract;
    class function Params(const CmdLine: string = ''): RProcessParameters; virtual;
    class function CreateList(Locked: Boolean = false): IProcessList;
    class function Create(const Name: String = ''): IProcess; 
  end;

implementation

uses
  SilBcChr,
  SilBtStr,
  SilBtVart,
  SilLiParameters,
  SilLtList,
  SilOiIpc, 
  SilOsTypes,
  SilOtTool,
  SilOkProcess,
  SilOmProcessList;

var
  MCurrentProcess: IProcess = nil;

{ SilProcessTool }

class function SilProcessTool.CommandLine: String;
begin
  Result := Str.Trim(Str.Replace(System.CmdLine, '"', ''));
end;

class function SilProcessTool.Create(const Name: String): IProcess;
begin
  Result := TSilEmptyProcess.Create(Name);
end;

class function SilProcessTool.CreateList(Locked: Boolean): IProcessList;
begin
  Result := TSilProcessList.Create(Locked);
end;

class function SilProcessTool.Current: IProcess;
begin
  if MCurrentProcess = nil then
    MCurrentProcess := DoGetCurrent();

  Result := MCurrentProcess;
end;

class function SilProcessTool.Params(const CmdLine: string): RProcessParameters;
var
  Buffer, Temp, Item, Option, Data: string;
  Value: Variant;
  N, Pos: Integer;
  Literals: IParameters;
begin
  if Str.IsEmpty(CmdLine) then
    Buffer := System.CmdLine else
    Buffer := CmdLine;

  Buffer := ListTool.ReplaceLiterals(Buffer, Literals);
  N := 0;

  Result.Arguments := ListTool.StringList();
  Result.Options := ListTool.Parameters();

  while Str.Enumerate(Buffer, ' ', Temp, N) do
  begin
    Temp := Str.Trim( Temp );
    if Length(Temp) = 0 then
      continue;

    if Str.FirstChar(Temp) in SwitchChars then
    begin
      Str.Split(Temp, Str.FirstChar(Temp), Item, Item, False);
      if Str.FirstChar(Item) in SwitchChars then // soporta el caso de opt con --
      begin
        Temp := Item;
        Str.Split(Temp, Str.FirstChar(Temp), Item, Item, False);
      end;

      Pos := Str.DelimiterPos(':=', Item);

      if Pos <> 0 then
        Str.Split(Item, Pos, Option, Data)
      else
        Option := Item;

      if Str.IsEmpty(Data) then
      begin
        case Str.LastChar(Option) of
          '+': Value := True;
          '-': Value := False;
          else Value := Vart.Unassigned;
        end;
        if Vart.IsOK(Value) then
          Option := Str.TrimRight(Option, Str.LastChar(Option), 1);
      end else
        Value := Literals.Get(Data, Data);

      Result.Options[Option] := Value;
    end else
      Result.Arguments.Add(Literals.Get(Temp, Temp));
  end;
end;

initialization

finalization
  MCurrentProcess := nil;

end.
