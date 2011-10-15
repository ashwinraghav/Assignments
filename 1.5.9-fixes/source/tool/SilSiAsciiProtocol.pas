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

unit SilSiAsciiProtocol;

interface

type
  IAsciiProtocol = interface
    ['{B8BDC0EE-68BD-4AE4-9647-B282A5E7C090}']
    function WriteLn(const Command: String): Boolean;
    function ReadLn(out Response: String): Boolean;
  end;

  IAsciiProtocolCommand = interface
    ['{B038493F-63F1-4EDE-94B1-5D44576FF89C}']
    function GetCommand: IUnknown;
    procedure SetCommand(const Value: IUnknown);
    property Command: IUnknown read GetCommand write SetCommand;
  end;

  RWriteLineEvent = record
    Sender: IUnknown;
    Text: String;
    Result: Boolean;
  end;

  RReadLineEvent = RWriteLineEvent;

  IAsciiProtocolEvents = interface
    ['{5CE221E3-EE9C-45AD-89E5-F980E50751A5}']
    procedure OnWriteLine(var Event: RWriteLineEvent);
    procedure OnReadLine(var Event: RReadLineEvent);
  end;

implementation

end.
 