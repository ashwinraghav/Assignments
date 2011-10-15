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

unit SilLmMemoryStream;

{$I Defines.inc}

interface

uses
  SilBeTypes,
  SilLkInterfaced,
  SilLiStream,
  SilLiClone;

type
  TMemoryStream = class (
    // extends
    TSilInterfacedObject,
    // implements
    IStream,
    IMemoryStream,
    IRandomStream,
    ICloneable)
  private
    FMemory: PString;
    FPosition: LongWord;
    FCounter: PInteger;
  protected // IStream
    function GetSize: LongWord; virtual; 
    function Read(var Buffer; Count: LongWord): LongWord; virtual; 
    function Write(const Buffer; Count: LongWord): LongWord; virtual; 
  protected // IRandomStream
    function GetPosition: LongWord; virtual;
    procedure SetPosition(Pos: LongWord); virtual;
    procedure SetSize(NewSize: LongWord); virtual;
    function Seek(Offset: Integer; Origin: TSeekOrigin): LongWord; virtual;
    procedure Truncate; virtual;
  protected // IMemoryStream
    function GetMemory: PChar; virtual;
    function GetRemaining: LongWord; virtual;
    function GetCurrent: PChar; virtual;
  protected // ICloneable
    function Clone: IUnknown;  virtual;
  protected
    constructor CreateClone(Buffer: PString; Counter: PInteger);
  public
    constructor Create(Buffer: PChar = nil; Size: LongWord = 0);
    destructor Destroy; override;
  end;

implementation

uses
  SilBtMem,
  SilAfLockedIncrement;

{ TMemoryStream }

constructor TMemoryStream.Create(Buffer: PChar; Size: LongWord);
begin
  inherited Create;
  New(FMemory);
  New(FCounter);
  FPosition := 1;
  FCounter^ := 1;
  if Size > 0 then
    if Assigned(Buffer) then
      SetString(FMemory^, Buffer, Size) else
      SetSize(Size);
end;

constructor TMemoryStream.CreateClone(Buffer: PString; Counter: PInteger);
begin
  inherited Create;
  FMemory := Buffer;
  FCounter := Counter;
  LockedInc(FCounter^);
  FPosition := 1;
end;

destructor TMemoryStream.Destroy;
begin
  if LockedDec(FCounter^) = 0 then
  begin
    Dispose(FMemory);
    Dispose(FCounter);
  end;
  inherited;
end;

function TMemoryStream.Clone: IUnknown;
begin
  Result := TMemoryStream.CreateClone(FMemory, FCounter);
end;

function TMemoryStream.GetMemory: PChar;
begin
  Result := PChar(FMemory^)
end;

function TMemoryStream.GetPosition: LongWord;
begin
  Result := FPosition - 1;
end;

function TMemoryStream.GetSize: LongWord;
begin
  Result := Length(FMemory^);
end;

function TMemoryStream.Read(var Buffer; Count: LongWord): LongWord;
begin
  if FPosition > LongWord(Length(FMemory^)) then
  begin
    Result := 0;
    Exit;
  end else
  if FPosition + Count > LongWord(Length(FMemory^)) then
    Result := LongWord(Length(FMemory^)) - FPosition + 1 else
    Result := Count;
  Move(FMemory^[FPosition], Buffer, Result);
  Inc(FPosition, Result);
end;

function TMemoryStream.Seek(Offset: Integer; Origin: TSeekOrigin): LongWord;
var
  iOffset: LargeInt;
begin
  iOffset := Offset;

  case Origin of
    soFromBeginning:  SetPosition(Offset);
    soFromCurrent:    SetPosition(FPosition + iOffset);
    soFromEnd:        SetPosition(Length(FMemory^) + iOffset + 1);
  end;

  Result := FPosition;
end;

procedure TMemoryStream.SetPosition(Pos: LongWord);
begin
  if Pos + 1 > LongWord(Length(FMemory^)) then
    FPosition := LongWord(Length(FMemory^)) + 1
  else
    FPosition := Pos + 1;
end;

procedure TMemoryStream.SetSize(NewSize: LongWord);
begin
  SetLength(FMemory^, NewSize);

  if FPosition > NewSize + 1 then
    FPosition := NewSize + 1;
end;

procedure TMemoryStream.Truncate;
begin
  if FPosition <= LongWord(Length(FMemory^)) then
    SetLength(FMemory^, FPosition - 1);
end;

function TMemoryStream.Write(const Buffer; Count: LongWord): LongWord;
begin
  if FPosition + Count - 1 > LongWord(Length(FMemory^)) then
    SetSize(FPosition + Count - 1);

  Move(Buffer, FMemory^[FPosition], Count);
  Inc(FPosition, Count);
  Result := Count;
end;

function TMemoryStream.GetCurrent: PChar;
begin
  Result := PChar(LongWord(GetMemory()) + FPosition - 1);
end;

function TMemoryStream.GetRemaining: LongWord;
begin
  Result := GetSize - GetPosition;
end;

end.
