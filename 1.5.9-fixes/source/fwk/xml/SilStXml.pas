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

unit SilStXml;

{$I Defines.inc}

interface

uses
  SilBkTool,
  SilOiFile,
  SilBeTypes,
  SilLiStream,
  SilSiXml,
  SilSiXmlParser,
  SilSiXmlReader,
  SilLiEnumerator;

type
  Xml = class (Tool)
    class function Tree: IXmlTree;
    class function Parser(const Stream: IStream): IXmlParser;
    class function ReadFile(const FileName: String; const Listener: IUnknown = nil; MustExists: Boolean = true): IXmlTree; overload;
    class function ReadFile(const FileName: String; const Listener: IUnknown; Access: TFileAccessMode = fmAccessReadWrite; Share: TFileShareMode = fmShareRead; MustExists: Boolean = true): IXmlTree; overload;
    class function ReadStream(const Stream: IStream; const Listener: IUnknown = nil): IXmlTree;
    class function WriteFile(const Tree: IXmlTree; const FileName: String; LineFeed: Boolean = true): Boolean;
    class function WriteStream(const Tree: IXmlTree; const Stream: IStream; LineFeed: Boolean = true): Boolean;
    class function ToStr(const Tree: IXmlTree; LineFeed: Boolean = false): String;
    class function FromStr(const Buffer: String): IXmlTree;
    class function ReadBuffer(Buffer: PChar; Size: Integer): IXmlTree;
    class procedure Merge(const Dest, Source: IXmlTree); overload;
    class procedure Merge(const Dest, Source: IXmlTag); overload;
  end;

implementation

uses
  SilLkObject,
  SilLtList,
  SilLiStringList,

  SilLtConnection,
  SilSmXml,
  SilSmXmlParser,
  SilSmXmlReader,
  SilLtStream,
  SilOsTool;

{ Xml }

class function Xml.ReadFile(const FileName: String; const Listener: IUnknown; Access: TFileAccessMode; Share: TFileShareMode; MustExists: Boolean): IXmlTree;
var
  Src: IFile;
begin
  Src := OsFile.OpenFile(FileName, Access, Share, MustExists);
  Result := Xml.ReadStream(Src.Stream, Listener);
end;

class function Xml.ReadFile(const FileName: String; const Listener: IUnknown; MustExists: Boolean): IXmlTree;
begin
  Result := Xml.ReadFile(FileName, Listener, fmAccessReadWrite, fmShareReadWrite, MustExists);
end;

class function Xml.ReadStream(const Stream: IStream; const Listener: IInterface): IXmlTree;
var
  Parser: IXmlParser;
  Reader: IXmlReader;
begin
  Parser := Self.Parser(Stream);
  Reader := TXmlReader.Create;

  if Stream.Size > 0 then
  begin
    Sink.Connect(Parser, Reader);
    Sink.Connect(Parser, Listener);

    try
      Parser.Read(Reader.Tree);
      Result := Reader.Tree;
      Result.Modified := false;
    finally
      Sink.Disconnect(Parser, Reader);
      Sink.Disconnect(Parser, Listener);
    end;
  end else
    Result := Reader.Tree;
end;

class function Xml.ReadBuffer(Buffer: PChar; Size: Integer): IXmlTree;
begin
  Result := ReadStream(Stream.Memory(Size, Buffer));
end;

class function Xml.Parser(const Stream: IStream): IXmlParser;
begin
  Result := TXmlParser.Create(Stream);
end;

class function Xml.Tree: IXmlTree;
begin
  Result := TXmlTree.Create;
end;

class function Xml.WriteFile(const Tree: IXmlTree; const FileName: String; LineFeed: Boolean): Boolean;
var
  Src: IFile;
begin
  Src := OsFile.OpenFile(FileName, fmAccessReadWrite, fmShareReadWrite, false);
  Result := Tree.Write(Src.Stream, LineFeed);
  if Result then Src.Stream.Truncate;
end;

class function Xml.WriteStream(const Tree: IXmlTree; const Stream: IStream; LineFeed: Boolean): Boolean;
begin
  Result := Tree.Write(Stream, LineFeed);
end;

class function Xml.ToStr(const Tree: IXmlTree; LineFeed: Boolean): String;
var
  Buf: IWriteOnlyStream;
begin
  Buf := Stream.WriteOnly;
  Tree.Write(Buf, LineFeed);
  Result := Buf.Buffer;
end;

class function Xml.FromStr(const Buffer: String): IXmlTree;
begin
  Result := Xml.ReadStream(Stream.Memory(Buffer));
end;

class procedure Xml.Merge(const Dest, Source: IXmlTree);
begin
  Merge(Dest.Root.AsTag, Source.Root.AsTag);
end;

class procedure Xml.Merge(const Dest, Source: IXmlTag);
var
  SourceChilds, MergeChilds: IXmlNodes;
  Enum: IEnumerator;
  Node: IXmlNode;
begin
  SourceChilds := Source.Childs;
  MergeChilds := Dest.Childs;

  while SourceChilds.Enumerate(Enum, Node) do
    if Node.NodeKind = nkTag then
      MergeChilds.AddTag(Node.AsTag);
end;

end.
