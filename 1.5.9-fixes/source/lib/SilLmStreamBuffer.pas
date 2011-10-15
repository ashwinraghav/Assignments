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

unit SilLmStreamBuffer;

{$I Defines.inc}

interface

uses
  SilLkInterfaced,
  SilLiStream;

type
  TStreamBuffer = class(TSilInterfacedObject, IStream, IRandomStream)
  private
    FStream: IRandomStream;
    FBuffer: PChar;
    FBufPtr: PChar;
    FBufEnd: PChar;
    FBufSize: Integer;
    procedure ReadBuffer;
  protected
    function GetPosition: LongWord;
    procedure SetPosition(Pos: LongWord);
    function GetSize: LongWord;
    procedure SetSize(NewSize: LongWord); 
    function Read(var Buffer; Count: LongWord): LongWord;
    function Write(const Buffer; Count: LongWord): LongWord;
    function Seek(Offset: Integer; Origin: TSeekOrigin): LongWord;
    procedure Truncate;
  public
    constructor Create(const AStream: IRandomStream; BufferSize: Integer);
    destructor Destroy; override;
  end;


implementation

uses
  SysUtils;

{ TStreamBuffer }

constructor TStreamBuffer.Create(const AStream: IRandomStream; BufferSize: Integer);
begin
  Assert(BufferSize > 0, 'BufferSize > 0');
  Assert(AStream <> nil, 'AStream <> nil');
  inherited Create;
  FStream := AStream;
  FBufSize := BufferSize;
  FBuffer := AllocMem(FBufSize);
  FBufPtr := FBuffer;
  FBufEnd := FBuffer;
end;

destructor TStreamBuffer.Destroy;
begin
  FBufPtr := nil;
  FBufEnd := nil;
  FreeMem(FBuffer);
  FStream := nil;
  inherited;
end;

function TStreamBuffer.GetPosition: LongWord;
begin
  Result := 0;  
end;

function TStreamBuffer.GetSize: LongWord;
begin
  Result := FStream.Size;
end;

function TStreamBuffer.Read(var Buffer; Count: LongWord): LongWord;
begin
  Result := 0;  
end;

function TStreamBuffer.Seek(Offset: Integer; Origin: TSeekOrigin): LongWord;
begin
  Result := FStream.Seek(Offset, Origin);
  ReadBuffer;
end;

procedure TStreamBuffer.SetPosition(Pos: LongWord);
begin
  FStream.Position := Pos;
  ReadBuffer;
end;

procedure TStreamBuffer.SetSize(NewSize: LongWord);
begin
  FStream.Size := NewSize;
end;

function TStreamBuffer.Write(const Buffer; Count: LongWord): LongWord;
begin
  Result := 0;  
end;

procedure TStreamBuffer.ReadBuffer;
begin

end;

procedure TStreamBuffer.Truncate;
begin

end;

end.
