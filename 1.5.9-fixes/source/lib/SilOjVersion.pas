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

unit SilOjVersion;

{$I Defines.inc}

interface

uses
  SilBeTypes,
  SilBkTool,
  SilOiVersion;

type
  SilVersionTool = class(Tool)
    class function ToStr(const Number: IVersionNumber; const Formats: string = CShortVersion; const Default: string = ''): string; virtual;
    class function Compare(const V1, V2: IVersionNumber): Integer; virtual;
    class function Equals(const V1, V2: IVersionNumber): Boolean; virtual;
    class function AsVersion(const Number: LargeInt): IVersionNumber; virtual;
    class function FromStr(const S: string): IVersionNumber; virtual;
    class function Info(FileName: string = ''): IVersionInfo; virtual;
  end;

implementation

uses
  SilBtStr,
  SilBtChr,
  SilBtLarge,
  SilOtTool,
  SilOsClasses, SilOiModule, SilOiFile;

{ SilVersionTool }

class function SilVersionTool.ToStr(const Number: IVersionNumber; const Formats, Default: string): string;

  function Check(var S: string; var N: TVersionItem): Boolean;
  const
    ItemStr: string = 'mnrb';
  var
    Ch: Char;
    I, X: Integer;
  begin
    Result := False;
    for I := 1 to Str.Len(S) do
    begin
      Ch := Chr.Lower(S[I]);
      if Ch in ['a'..'z'] then
      begin
        X := Pos(Ch, ItemStr);
        Result := X <> 0;
        if Result then
        begin
          Str.Replace(S, Ch, 'd');
          N := TVersionItem(X - 1);
        end;
        Break;
      end;
    end;
  end;

  var
    S: string;
    I: Integer;
    N: TVersionItem;
    Replace: Boolean;
begin
  if Assigned(Number) then
    begin
      I := 1; Replace := False;
      Result := Str.Null;
      repeat
        S := Str.Token(Formats, '%', I);
        if not Str.IsEmpty(S) and Replace and Check(S, N) then
          S := Str.Format('%'+S, [ Number.Item[N] ]);
        Str.Add(Result, S);
        if not Replace then Replace := True;
      until I = 0;
    end
  else
    Result := Default;
end;

class function SilVersionTool.Compare(const V1, V2: IVersionNumber): Integer;
begin
  ASSERT(Assigned(V1) and Assigned(V2), 'SilVersionTool.Compare');
  Result := Integer(V1.Value - V2.Value);
end;

class function SilVersionTool.Equals(const V1, V2: IVersionNumber): Boolean;
begin
  Result := Compare(V1, V2) = 0;
end;

class function SilVersionTool.AsVersion(const Number: LargeInt): IVersionNumber;
begin
  Result := TSilOsVersionNumber.Create(Number);
end;

class function SilVersionTool.FromStr(const S: string): IVersionNumber;
var
  I: Integer;
  J, N, R, B: Word;
begin
  I := 1;
  J := Str.ToInt(Str.Token(S, '.', I), 0);
  N := Str.ToInt(Str.Token(S, '.', I), 0);
  R := Str.ToInt(Str.Token(S, '.', I), 0);
  B := Str.ToInt(Str.Token(S, '.', I), 0);
  Result := AsVersion(Large.Make(LongWord(R shl 16) + B, LongWord(J shl 16) + N));
end;

class function SilVersionTool.Info(FileName: string): IVersionInfo;
begin
  if Str.IsEmpty(FileName) then FileName := OS.Module.Current.Info.FullName;
  Result := TSilOsVersionInfo.Create(FileName);
end;

end.
