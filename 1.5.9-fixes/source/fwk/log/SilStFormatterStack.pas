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

unit SilStFormatterStack;

interface

uses
  Sil,
  SilLgLogger;

type
  StackFmt = class(Formatter)
    class function Format(const Kind: TTraceKind; const Item: ITraceData; const Text: string): string; override;
  end;

implementation

function FormatMemory(const Value: RMemoryInfo): string;
begin
  Result := '[mem=' + Int.PadL(Value.Used, 9) + ' thr=' + Int.PadL(Sil.OS.Thread.List.Count, 3) + ']';
end;

function FormatThread(const Thread: IThreadData): string;
begin
  Result := '[' + Str.PadR(Thread.Name, 10) + '=' + Int.ToHex(Thread.ID, 8) + ']';
end;

function FormatModule(const Module: string): string;
begin
  Result := Str.PadR(Module, 15 + 3 + 1);  
end;

function FormatLapse(const Elapsed: TDateTime): string;
begin
  with DateTime.Decode(Elapsed) do
  begin
    Result := Str.Zero(Time.Hour, 2) + ':' + Str.Zero(Time.Minutes, 2) + ':' + Str.Zero(Time.Seconds, 2) + '.' + Str.Zero(Time.mSeconds, 3);

    if Time.Hour > 0 then
      Result := Result + '!!!-h' else
    if Time.Minutes > 0 then
      Result := Result + '!!!-n' else
    if Time.Seconds > 1 then
      Result := Result + '!!!-s' else
      Result := Result + '     ';

    Result := '[lap=' + Result + ']'
  end;               
end;

function FormatLine(const Kind: TTraceKind; const Item: ITraceData; const Text: string): string;
var
  Buff, Thread: String;
begin
  Result := '';
  Thread := FormatThread(Item.Thread);
  case Kind of
    tkLeave:  Buff := FormatLapse(Item.Elapsed);
    else      Buff := FormatMemory(Item.Memory);
  end;

  Sil.Str.Add(Result, Thread, ccSPC);
  Sil.Str.Add(Result, Buff, ccSPC);              
  Sil.Str.Add(Result, Str.ReplicateChar(ccSPC, 4), ccSPC);
  Sil.Str.Add(Result, FormatModule(Item.Module), ccSPC);
  Sil.Str.Add(Result, ':' + Str.ReplicateChar(ccSPC, Item.Level) + GTraceLevelChars[Kind], ccSPC);
  Sil.Str.Add(Result, Text, ccSPC);
end;

class function StackFmt.Format(const Kind: TTraceKind; const Item: ITraceData; const Text: string): string;
begin
  Result := FormatLine(Kind, Item, Text);
end;

end.
