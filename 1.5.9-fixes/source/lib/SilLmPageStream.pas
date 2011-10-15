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

unit SilLmPageStream;

{$I Defines.inc}

interface

uses
  SilBeTypes,
  SilLkObject,
  SilLiStream;

type
  TSilPageStream = class (
    // extends
    TSilObject,
    // implements
    IStream,
    IBufferStream,
    IMemoryStream)
  private
    FDirect: Boolean;
    FSource: IStream;
    FBuffer: IMemoryStream;
  protected // IStream
    function GetSize: LongWord;
    function Read(var Buffer; Count: LongWord): LongWord;
    function Write(const Buffer; Count: LongWord): LongWord;
  protected // IRandomStream
    function GetPosition: LongWord;
    procedure SetPosition(Pos: LongWord);
    procedure SetSize(NewSize: LongWord);
    function Seek(Offset: Integer; Origin: TSeekOrigin): LongWord;
    procedure Truncate;
  protected // IMemoryStream
    function GetMemory: PChar;
    property Memory: PChar read GetMemory;
  protected // IBufferStream
    function GetSource: IStream;
    procedure SetSource(const Value: IStream);
    function GetRemaining: LongWord;
    function GetCurrent: PChar;
  public
    constructor Create(const Source: IStream);
    destructor Destroy; override;
  end;

implementation

uses
  SilBtStr,
  SilLtReference,
  SilLtStream;

{ TSilPageStream }

constructor TSilPageStream.Create(const Source: IStream);
begin
  inherited Create;
  SetSource(Source);
end;

destructor TSilPageStream.Destroy;
begin
  FSource := nil;
  FBuffer := nil;

  inherited;
end;

function TSilPageStream.GetSize: LongWord;
begin
  Result := FBuffer.Size;
end;

function TSilPageStream.Read(var Buffer; Count: LongWord): LongWord;
var
  Ptr: PChar;
  BufferCount: LongWord;
begin
  if not FDirect then
  begin
    Result := 0;
    Ptr := PChar(@Buffer);

    if FBuffer.Position < FBuffer.Size then
      Result := FBuffer.Read(Ptr^, Count);

    if (Result < Count) and Assigned(FSource) then
    begin
      BufferCount := FSource.Read(Ptr[Result], Count - Result);
      FBuffer.Write(Ptr[Result], BufferCount);
      Inc(Result, BufferCount);
    end;
  end else
    Result := FBuffer.Read(Buffer, Count);
end;

function TSilPageStream.Write(const Buffer; Count: LongWord): LongWord;
begin
  Result := FBuffer.Write(Buffer, Count);
end;

function TSilPageStream.Seek(Offset: Integer; Origin: TSeekOrigin): LongWord;
begin
  Result := FBuffer.Seek(Offset, Origin);
end;

function TSilPageStream.GetPosition: LongWord;
begin
  Result := FBuffer.Position;
end;

procedure TSilPageStream.SetPosition(Pos: LongWord);
begin
  FBuffer.Position := Pos;
end;

procedure TSilPageStream.SetSize(NewSize: LongWord);
begin
  FBuffer.Size := NewSize;
end;

procedure TSilPageStream.Truncate;
begin
  FBuffer.Truncate;
end;

function TSilPageStream.GetMemory: PChar;
begin
  Result := FBuffer.Memory;
end;

function TSilPageStream.GetCurrent: PChar;
begin
  Result := FBuffer.Memory + FBuffer.Position;
end;

function TSilPageStream.GetRemaining: LongWord;
begin
  Result := FBuffer.Size - FBuffer.Position;
end;

function TSilPageStream.GetSource: IStream;
begin
  Result := FSource;
end;

procedure TSilPageStream.SetSource(const Value: IStream);
begin
  FDirect := Ref.GetInterface(Value, IRandomStream, FBuffer);

  if not FDirect then
  begin
    FSource := Value;
    FBuffer := Stream.Memory();
  end;
end;

end.
