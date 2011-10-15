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

unit SilSfLayerProtocolFile;

interface

{$include Defines.inc}

uses
  Sil,
  SilSiLayerProtocolFile;

function GlobalPath(const Path: String): String;
function DoReadFileInfo(const Packet: IPacket; out Info: IFileInfo): Boolean;
procedure DoWriteFileInfo(const Packet: IPacket; const Info: IFileInfo; Skip: Word);
function DoReadFileInfoList(const Packet: IPacket; out Info: IFileInfoList): Boolean;
procedure DoWriteFileInfoList(const Packet: IPacket; const Info: IFileInfoList; Skip: Word);

implementation

uses SilLiFiler;

function GlobalPath(const Path: String): String;
var
  i: Integer;
begin
  Result := Path;
  if Str.IsEmpty(Path) then Exit;

  if (Length(Result) > 1) and (Result[2] = ':') then
  begin
    Result[2] := Result[1];
    Result[1] := '/';
  end;

  for i := 1 to Length(Result) do if Result[i] = '\' then Result[i] := '/';
end;

procedure DoWriteFileInfo(const Packet: IPacket; const Info: IFileInfo; Skip: Word);
var
  Attr: TFileAttributes;
begin
  if Assigned(Info) then
  begin
    Packet.Writer.WriteBoolean(true);
    Packet.Writer.WriteString(GlobalPath(Str.Copy(Info.FullName, Skip + 1)));
    Packet.Writer.WriteDouble(Info.Time);
    Attr := Info.Attributes;
    Packet.Writer.Write(Attr, SizeOf(TFileAttributes));
    Packet.Writer.WriteLongWord(Info.Size);
  end else
    Packet.Writer.WriteBoolean(false);
end;

function DoReadFileInfo(const Packet: IPacket; out Info: IFileInfo): Boolean;
var
  Attr: TFileAttributes;
  FileInfo: IFileInfoDef;
begin
  Result := Packet.Reader.ReadBoolean;

  if Result then
  begin
    FileInfo := Sil.OS.FileSystem.FileInfo(Packet.Reader.ReadString);
    FileInfo.Time := Packet.Reader.ReadDouble;
    Packet.Reader.Read(Attr, SizeOf(TFileAttributes));
    FileInfo.Attributes := Attr;
    FileInfo.Size := Packet.Reader.ReadLongWord;
    Info := FileInfo;
  end;
end;

procedure DoWriteFileInfoList(const Packet: IPacket; const Info: IFileInfoList; Skip: Word);
var
  Enum: IEnumerator;
  Item: IFileInfo;
begin
  Packet.Writer.WriteLongWord(Info.Count);

  while Info.Enumerate(Enum, Item) do
    DoWriteFileInfo(Packet, Item, Skip);
end;

function DoReadFileInfoList(const Packet: IPacket; out Info: IFileInfoList): Boolean;
var
  Count: LongWord;
  Item: IFileInfo;
begin
  Info := OS.FileSystem.FileInfoList;
  Count := Packet.Reader.ReadLongWord;
  Result := Count > 0;

  while Count > 0 do
  begin
    DoReadFileInfo(Packet, Item);
    Info.Add(Item);
    Dec(Count);
  end;
end;

end.
