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

unit SilSmXmlReader;

{$I Defines.inc}

interface

uses
  SilLkInterfaced,
  SilSiXml,
  SilSiXmlParser,
  SilSiXmlReader;

type
  TXmlReader = class (
    // extends
    TSilInterfacedObject,
    // implements
    IXmlReader,
    IXmlParserEvents)
  private
    FTree: IXmlTree;
  protected // IXmlReader
    function GetTree: IXmlTree;
  protected // IXmlParserEvents
    procedure OnXmlNodeBegin(var Event: RXmlNodeBeginEvent);
    procedure OnXmlNodeEnd(var Event: RXmlNodeEndEvent);
    procedure OnXmlNodeData(var Event: RXmlNodeDataEvent);
    procedure OnXmlComment(var Event: RXmlCommentEvent);
    procedure OnXmlError(var Event: RXmlErrorEvent);
  public
    constructor Create;
    destructor Destroy; override;
  end;                 

implementation

uses
  SilSmXml;

{ TXmlReader }

constructor TXmlReader.Create;
begin
  inherited Create;
  FTree := TXmlTree.Create;
end;

destructor TXmlReader.Destroy;
begin
  FTree := nil;
  inherited;
end;

function TXmlReader.GetTree: IXmlTree;
begin
  Result := FTree;
end;

procedure TXmlReader.OnXmlComment(var Event: RXmlCommentEvent);
begin
  Event.CurrentNode.AsComment.Lines.Add(Event.TextLine);
end;

procedure TXmlReader.OnXmlError(var Event: RXmlErrorEvent);
begin

end;

procedure TXmlReader.OnXmlNodeBegin(var Event: RXmlNodeBeginEvent);
var
  i: Integer;
  Node: IXmlNode;
begin
  case Event.Action of
    paSetRoot:  Node := Event.Tree.CreateRoot;
    paAddNode:  Node := Event.CurrentNode.Childs.Add(Event.NodeKind);
  end;

  if Event.NodeKind = nkTag then
    with Node.AsTag do
    begin
      Name := Event.Name;
      TagKind := Event.TagKind;

      for i := 0 to Length(Event.Arguments) - 1 do
        with Event.Arguments[i] do
          Arguments.CreateNew(Name, Value);
    end;

  Event.CurrentNode := Node;
end;

procedure TXmlReader.OnXmlNodeData(var Event: RXmlNodeDataEvent);
begin
  if Event.CurrentNode <> nil then
    Event.CurrentNode.AsTag.Data.Add(Event.TextLine);
end;

procedure TXmlReader.OnXmlNodeEnd(var Event: RXmlNodeEndEvent);
begin
  if Event.CurrentNode <> nil then
    Event.CurrentNode := Event.CurrentNode.Parent;
end;

end.
 