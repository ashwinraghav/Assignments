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

unit SilLmWOStream;

{$include Defines.inc}

interface

uses
  SilLiStream,
  SilLkObject;

type
  PNode = ^TNode;
  TNode = record
    Size: LongWord;
    Buffer: PChar;
  end;

  TWOStream = class (TSilObject, IWriteOnlyStream)
  private
    FSize: LongWord;
    FLines: Integer;
    FList: array of Pointer;
  protected
    function GetSize: LongWord;
    function Read(var Buffer; Count: LongWord): LongWord;
    function Write(const Buffer; Count: LongWord): LongWord;
    function Buffer: String;
  public
    constructor Create;
    destructor Destroy; override;
  end;

implementation

constructor TWOStream.Create;
begin
  inherited Create;
  SetLength(FList, 0);
  FSize := 0;
  FLines := 0;
end;

destructor TWOStream.Destroy;
var
  i: Integer;
  Node: PNode;
begin
  for i := 0 to FLines - 1 do
  begin
    Node := PNode(FList[i]);
    FreeMem(Node.Buffer);
    FreeMem(Node);
  end;

  inherited;
end;

function TWOStream.GetSize: LongWord;
begin
  Result := FSize;
end;

function TWOStream.Read(var Buffer; Count: LongWord): LongWord;
begin
  Result := 0;
end;

function TWOStream.Write(const Buffer; Count: LongWord): LongWord;
var
  Len: LongWord;
  Node: PNode;
begin
  if FLines >= Length(FList) then
  begin
    Len := Length(FList);
    SetLength(FList, Len + 100);
  end;

  if FLines > 0 then
    Node := FList[FLines - 1]
  else
    Node := nil;

  if Assigned(Node) and (Node.Size < 512) then
  begin
    ReallocMem(Node.Buffer, Node.Size + Count);
    Move(Buffer, PChar(Node.Buffer + Node.Size)^, Count);
    Inc(Node.Size, Count);
  end else
  begin
    GetMem(Node, SizeOf(TNode));
    Node.Size := Count;
    GetMem(Node.Buffer, Count);
    Move(Buffer, Node.Buffer^, Count);
    FList[FLines] := Node;
    Inc(FLines);
  end;

  Inc(FSize, Count);
  Result := Count;
end;

function TWOStream.Buffer: String;
var
  i: Integer;
  Node: PNode;
  PBuf: PChar;
begin
  SetString(Result, nil, FSize);
  PBuf := PChar(Result);

  for i := 0 to FLines - 1 do
  begin
    Node := PNode(FList[i]);
    Move(Node.Buffer^, PBuf^, Node.Size);
    Inc(PBuf, Node.Size);
  end;
end;

end.
 