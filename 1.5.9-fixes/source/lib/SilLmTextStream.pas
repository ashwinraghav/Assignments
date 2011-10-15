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

unit SilLmTextStream;

{$I Defines.inc}

interface

uses
  SilLkObject,
  SilLiStream;

type
  TSilTextStream = class (TSilObject, ITextStream)
  private
    FStream: IStream;
    FRandom: IRandomStream;
  protected // IStream
    function GetSize: LongWord;
    function Read(var Buffer; Count: LongWord): LongWord;
    function Write(const Buffer; Count: LongWord): LongWord;
    property Size: LongWord read GetSize;
  protected // ITextStream
    function GetPosition: LongWord;
    procedure SetPosition(Pos: LongWord);
    procedure SetSize(NewSize: LongWord);
    function Seek(Offset: Integer; Origin: TSeekOrigin): LongWord;
    procedure Truncate;
  protected // ITextStream
    function ReadStr(var Buffer: String): LongWord;
    function WriteStr(const Buffer: String): LongWord;
    function ReadLn(var Buffer: String): Boolean;
    function WriteLn(const Buffer: String = ''): Boolean;
  public
    constructor Create(const Source: IStream);
    destructor Destroy; override;
  end;

implementation

uses
  SilBcChr,
  SilBtError,
  SilLtReference,
  SilLmMemoryStream;

{ TSilTextStream }

constructor TSilTextStream.Create(const Source: IStream);
begin
  inherited Create;
  if not Assigned(Source) then
    FStream := TMemoryStream.Create() else
    FStream := Source;
  Ref.GetInterface(FStream, IRandomStream, FRandom);
end;

destructor TSilTextStream.Destroy;
begin
  FStream := nil;
  FRandom := nil;
  inherited;
end;

function TSilTextStream.GetPosition: LongWord;
begin
  if Assigned(FRandom) then
    Result := FRandom.Position
  else
    Result := 0;
end;

procedure TSilTextStream.SetPosition(Pos: LongWord);
begin
  if Assigned(FRandom) then
    FRandom.Position := Pos;
end;

procedure TSilTextStream.SetSize(NewSize: LongWord);
begin
  if Assigned(FRandom) then
    FRandom.Size := NewSize;
end;

function TSilTextStream.Seek(Offset: Integer; Origin: TSeekOrigin): LongWord;
begin
  if Assigned(FRandom) then
    Result := FRandom.Seek(Offset, Origin)
  else
    Result := 0;
end;

procedure TSilTextStream.Truncate;
begin
  if Assigned(FRandom) then
    FRandom.Truncate;
end;

function TSilTextStream.GetSize: LongWord;
begin
  Result := FStream.Size;
end;

function TSilTextStream.Read(var Buffer; Count: LongWord): LongWord;
begin
  Result := FStream.Read(Buffer, Count);
end;

function TSilTextStream.ReadLn(var Buffer: String): Boolean;
const
  FindEol: array [0..1] of Char = (ccCR, ccLF);
var
  i: Integer;
  Found: Byte;
begin
  i := 1;
  Found := 0;
  SetLength(Buffer, 1024);

  while FStream.Read(Buffer[i], 1) = 1do
  begin
    if Buffer[i] = FindEol[Found] then
    begin
      Inc(Found);

      if Found > 1 then
      begin
        SetLength(Buffer, i - 2);
        Result := true;
        Exit;
      end;
    end else
    if Found > 0 then Found := 0;

    if i >= Length(Buffer) then SetLength(Buffer, i + 1024);
    Inc(i);
  end;

  Result := i > 1;
  if Result then SetLength(Buffer, i - 1);
end;

function TSilTextStream.ReadStr(var Buffer: String): LongWord;
begin
  Result := FStream.Read(Buffer[1], Length(Buffer));
end;

function TSilTextStream.Write(const Buffer; Count: LongWord): LongWord;
begin
  Result := FStream.Write(Buffer, Count);
end;

function TSilTextStream.WriteLn(const Buffer: String): Boolean;
begin
  Result := WriteStr(Buffer + ccCRLF) > 0;
end;

function TSilTextStream.WriteStr(const Buffer: String): LongWord;
begin
  Result := FStream.Write(Buffer[1], Length(Buffer));
end;

end.