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

unit SilSiXmlParser;

{$I Defines.inc}

interface

uses
  SilLiStream,
  SilSiXml;

type
  TXmlArgument = record
    Name: String;
    Value: String;
  end;

  TXmlArguments = array of TXmlArgument;
  TXmlParserAction = (paSetRoot, paAddNode);

  IXmlParser = interface
    ['{61EE3C95-0491-4AD5-8217-893DFF008874}']
    function GetStream: IStream;
    procedure SetStream(const Value: IStream);
    function Read(const Tree: IXmlTree = nil): Boolean;
    property Stream: IStream read GetStream write SetStream;
  end;

  RXmlNodeBeginEvent = record
    Sender: IXmlParser;
    Action: TXmlParserAction;
    Tree: IXmlTree;
    CurrentNode: IXmlNode;
    NodeKind: TXmlNodeKind;
    Name: String;
    Arguments: TXmlArguments;
    TagKind: TXmlTagKind;
    Percent: Byte;
  end;

  RXmlNodeEndEvent = record
    Sender: IXmlParser;
    Tree: IXmlTree;
    CurrentNode: IXmlNode;
    Percent: Byte;
  end;

  RXmlNodeDataEvent = record
    Sender: IXmlParser;
    Tree: IXmlTree;
    CurrentNode: IXmlNode;
    TextLine: String;
    Percent: Byte;
  end;

  RXmlCommentEvent = RXmlNodeDataEvent;

  RXmlErrorEvent = record
    Sender: IXmlParser;
    NodeKind: TXmlNodeKind;
    Name: String;
    Percent: Byte;
  end;

  IXmlParserEvents = interface
    ['{1B2254A9-7F23-4058-8C37-0834E217EC11}']
    procedure OnXmlNodeBegin(var Event: RXmlNodeBeginEvent);
    procedure OnXmlNodeEnd(var Event: RXmlNodeEndEvent);
    procedure OnXmlNodeData(var Event: RXmlNodeDataEvent);
    procedure OnXmlComment(var Event: RXmlCommentEvent);
    procedure OnXmlError(var Event: RXmlErrorEvent);
  end;

implementation

end.
 