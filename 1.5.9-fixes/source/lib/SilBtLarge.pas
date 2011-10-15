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

unit SilBtLarge;

interface

{$INCLUDE Defines.inc}

uses
  SilBeTypes,
  SilBkTool;

type
  LargeClass = class of Large;
  Large = class(Tool)
    class function Make(const Low: LongWord; const High: LongWord {$IFDEF USE_DEFPARAMS} = 0 {$ENDIF} ): LargeInt;
    class function Low(const Value: LargeInt): LongWord;
    class function High(const Value: LargeInt): LongWord;
    class function ToStr(const Value: LargeInt): string;
    class function Sign(const Number: LargeInt): Integer;
    class function Compare(const e1, e2: LargeInt): Integer;
    class function IIf(const Expr: Boolean; const RTrue: LargeInt; const RFalse: LargeInt{$IFDEF USE_DEFPARAMS} = 0 {$ENDIF}): LargeInt;
    class function BytesToStr(const Count: LargeInt): String;
  end;

implementation

uses
  SilBtStr;

{ Large }

class function Large.Make(const Low, High: LongWord): LargeInt;
var
  Aux: LargeInt;
begin
  Aux := High;
  Result := Low + Aux shl 32;
end;

class function Large.High(const Value: LargeInt): LongWord;
begin
  Result := LongWord(Value shr 32);
end;

class function Large.Low(const Value: LargeInt): LongWord;
begin
  Result := LongWord(Value and $FFFFFFFF);
end;

class function Large.ToStr(const Value: LargeInt): string;
begin
  System.Str(Value, Result); 
end;

class function Large.Sign(const Number: LargeInt): Integer;
begin
  if Number >= 0 then
    Result :=  1 else
    Result := -1;
end;

class function Large.Compare(const e1, e2: LargeInt): Integer;
begin
  if e1 > e2 then Result := 1 else
  if e1 < e2 then Result := -1 else Result := 0;
end;

class function Large.IIf(const Expr: Boolean; const RTrue, RFalse: LargeInt): LargeInt;
begin
  if Expr then
    Result := RTrue else
    Result := RFalse;
end;

class function Large.BytesToStr(const Count: LargeInt): String;
begin
  if Count < $100000 then
    Result := Str.Format('%.1f KB', [Count / $400]) else
    Result := Str.Format('%.1f MB', [Count / $100000]);
end;

end.
 