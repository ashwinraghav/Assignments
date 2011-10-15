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

unit SilLmBufferStream;

{$I Defines.inc}

interface

uses
  SilLiStream,
  SilLmMemoryStream;

type
  TBufferStream = class(TMemoryStream, IBufferStream)
  private
    FSource: IStream;
    FRandom: IRandomStream;
    FSize: Integer;
    FLimit: LongWord;
  protected
    procedure DoCheckSource;
    function DoGetPtr(Position: LongWord): PChar;
    function DoGetRemaining: LongWord;
		function DoGetPosition: LongWord; 
    function DoGetCurrent: PChar;
  protected // IBufferStream
    function GetSource: IStream;
    procedure SetSource(const Value: IStream);
    function GetRemaining: LongWord; override;
    function GetCurrent: PChar; override;
		function DoGetSize: LongWord; 
  protected
		function GetSize: LongWord; override;
		function Read(var Buffer; Count: LongWord): LongWord; override;
		function Write(const Buffer; Count: LongWord): LongWord; override;
  protected
		function GetPosition: LongWord; override;
  public
    constructor Create(const Source: IStream = nil; Buffer: PChar = nil; Size: LongWord = 0; Limit: Integer = 0);
    destructor Destroy; override;
  end;

implementation

{ TBufferStream }

constructor TBufferStream.Create(const Source: IStream; Buffer: PChar; Size: LongWord; Limit: Integer);
begin
  inherited Create(Buffer, 0);
  FSize := Size;  
  FLimit := Limit;
  SetSize(FSize);
  SetSource(Source);    
end;

destructor TBufferStream.Destroy;
begin
  SetSource(nil);  
  inherited;
end;

function TBufferStream.GetSize: LongWord;
begin
  DoCheckSource;
  Result := DoGetSize();
end;

function TBufferStream.Read(var Buffer; Count: LongWord): LongWord;
begin
  DoCheckSource;
  Result := inherited Read(Buffer, Count);
end;

function TBufferStream.Write(const Buffer; Count: LongWord): LongWord;
begin
  Result := inherited Write(Buffer, Count);
(*)  if Assigned(FSource) then
    FSource.Write(Buffer, Count); (*)
end;

function TBufferStream.GetCurrent: PChar;
begin
  DoCheckSource;
  Result := DoGetCurrent;
end;

function TBufferStream.GetRemaining: LongWord;
begin
  DoCheckSource;
  Result := DoGetRemaining;
end;

function TBufferStream.GetSource: IStream;
begin
  Result := FSource;
end;

procedure TBufferStream.SetSource(const Value: IStream);
begin
  if FSource <> nil then FRandom := nil;
  FSource := Value;
  if FSource <> nil then FSource.QueryInterface(IRandomStream, FRandom);
  DoCheckSource;
end;

function TBufferStream.GetPosition: LongWord;
begin
  DoCheckSource;
  Result := DoGetPosition();
end;

procedure TBufferStream.DoCheckSource;
var
  NumBytes, OldSize: LongWord;
begin
  if Assigned(FSource) and (DoGetRemaining() <= FLimit) then
  begin
    NumBytes := FSource.Size;
    if NumBytes > 0 then
    begin 
      OldSize := DoGetSize();
      if Assigned(FRandom) and (OldSize >= FRandom.Position) then
        Dec(OldSize, FRandom.Position);
      SetSize(OldSize + NumBytes);
      FSource.Read(DoGetPtr(OldSize)^, NumBytes);
    end;
  end;
end;

function TBufferStream.DoGetPtr(Position: LongWord): PChar;
begin
  Result := PChar(LongWord(GetMemory()) + Position);
end;

function TBufferStream.DoGetRemaining: LongWord;
begin
  Result := DoGetSize() - DoGetPosition();
end;

function TBufferStream.DoGetPosition: LongWord;
begin
  Result := inherited GetPosition();
end;

function TBufferStream.DoGetCurrent: PChar;
begin
  Result := DoGetPtr(DoGetPosition());
end;

function TBufferStream.DoGetSize: LongWord;
begin
  Result := inherited GetSize();
end;

end.
