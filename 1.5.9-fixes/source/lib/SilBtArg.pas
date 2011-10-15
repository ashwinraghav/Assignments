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

unit SilBtArg;

interface

{$INCLUDE Defines.inc}

uses
  SilBeArg,
  SilBkTool,
  SilBtStr,
  SilBtVart;

type
  ArgTool = class(Tool)
    class function ToStr(const Arg: TArgument; const Separator: string; const Default: string {$IFDEF USE_DEFPARAMS} = '' {$ENDIF}): string;
    class function Make(const Name: string; const Value: Variant): TArgument; {$IFDEF USE_OVERLOAD} overload; {$ENDIF}
    class function MakeOpt(const Name: string; const Value, Option: Variant): TArgument;
    class function MakeName(const Name: string): TArgument;
    class function Null: TArgument;
    class function IsNull(const Arg: TArgument): Boolean;
    class function IsOK(const Arg: TArgument): Boolean;
    class function New(const Arg: TArgument): PArgument;
  {$IFDEF USE_OVERLOAD}
    class function Make(const Name: string; const Value, Option: Variant): TArgument; {$IFDEF USE_OVERLOAD} overload; {$ENDIF}
    class function Make(const Name: string): TArgument; overload;
  {$ENDIF}
  end;

implementation

{ ArgTool }

class function ArgTool.ToStr(const Arg: TArgument; const Separator, Default: string): string;
begin
  Result := '';
  if Arg.Name <> '' then Str.Add(Result, Arg.Name + Separator);
  Str.Add(Result, Vart.ToStr(Arg.Value, Default));
end;

class function ArgTool.Make(const Name: string; const Value: Variant): TArgument;
begin
  Result := MakeOpt(Name, Value, Vart.Unassigned);
end;

class function ArgTool.MakeOpt(const Name: string; const Value, Option: Variant): TArgument;
begin
  Result.Name := Name;
  Result.Value := Value;
  Result.Option := Option;
end;

class function ArgTool.MakeName(const Name: string): TArgument;
begin
  Result := Make(Name, Vart.Unassigned);
end;

class function ArgTool.Null: TArgument;
begin
  Result := SilBeArg.NullArg;
end;

class function ArgTool.IsNull(const Arg: TArgument): Boolean;
begin
  Result := Str.IsEmpty(Arg.Name) and Vart.IsEmpty(Arg.Value) and Vart.IsEmpty(Arg.Option);  
end;

class function ArgTool.IsOK(const Arg: TArgument): Boolean;
begin
  Result := not Str.IsEmpty(Arg.Name) or not Vart.IsEmpty(Arg.Value) or not Vart.IsEmpty(Arg.Option);
end;

class function ArgTool.New(const Arg: TArgument): PArgument;
begin
  System.New(Result);
  Result^ := Arg;
end;

{$IFDEF USE_OVERLOAD}

class function ArgTool.Make(const Name: string; const Value, Option: Variant): TArgument;
begin
  Result := MakeOpt(Name, Value, Option);
end;

class function ArgTool.Make(const Name: string): TArgument;
begin
  Result := MakeName(Name);
end;

{$ENDIF}

end.
