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

unit SilXml;

{$I Defines.inc}

interface

uses
  SilSiXml,
  SilSiXmlParser,
  SilSiXmlReader,
  SilStXml;

type
  TXmlNodeKind                  = SilSiXml.TXmlNodeKind;

const
  nkUnknown                     = SilSiXml.nkUnknown;
  nkTag                         = SilSiXml.nkTag;
  nkComment                     = SilSiXml.nkComment;

type
  TXmlTagKind                   = SilSiXml.TXmlTagKind;

const
  tkUnknown                     = SilSiXml.tkUnknown;
  tkBlock                       = SilSiXml.tkBlock;
  tkInline                      = SilSiXml.tkInline;

type
  IXmlTree                      = SilSiXml.IXmlTree;
  IXmlNodes                     = SilSiXml.IXmlNodes;
  IXmlNode                      = SilSiXml.IXmlNode;
  IXmlArguments                 = SilSiXml.IXmlArguments;
  IXmlTag                       = SilSiXml.IXmlTag;
  IXmlComment                   = SilSiXml.IXmlComment;

type
  TXmlArgument                  = SilSiXmlParser.TXmlArgument;
  TXmlArguments                 = SilSiXmlParser.TXmlArguments;

type
  TXmlParserAction              = SilSiXmlParser.TXmlParserAction;

const
  paSetRoot                     = SilSiXmlParser.paSetRoot;
  paAddNode                     = SilSiXmlParser.paAddNode;

type
  IXmlParser                    = SilSiXmlParser.IXmlParser;
  RXmlNodeBeginEvent            = SilSiXmlParser.RXmlNodeBeginEvent;
  RXmlNodeEndEvent              = SilSiXmlParser.RXmlNodeEndEvent;
  RXmlNodeDataEvent             = SilSiXmlParser.RXmlNodeDataEvent;
  RXmlCommentEvent              = SilSiXmlParser.RXmlCommentEvent;
  RXmlErrorEvent                = SilSiXmlParser.RXmlErrorEvent;
  IXmlParserEvents              = SilSiXmlParser.IXmlParserEvents;

type
  IXmlReader                    = SilSiXmlReader.IXmlReader;                      

type
  Xml                           = SilStXml.Xml;

type
  Tool                          = Xml;

implementation
end.
