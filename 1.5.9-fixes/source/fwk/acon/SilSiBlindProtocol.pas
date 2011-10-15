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

unit SilSiBlindProtocol;

interface

uses
  SilOcTypes,
  SilSiAbstractConnection,
  SilSiProtocolBase;

type
  IBlindProtocol = interface (IProtocol)
    ['{EC2E982F-C541-11D4-989F-00104B0FA1EF}']
    procedure Text(const Buffer: String);
    function Request(const Obj: TGuid; out Id: LongWord; Timeout: LongWord = INFINITE): Boolean;
    function Supports(const Obj: TGuid; Timeout: LongWord = INFINITE): Boolean;
    procedure AddFilter(ProtocolID: Cardinal);
    procedure RemoveFilter(ProtocolID: Cardinal);
  end;

  RBlindTextEvent = record
    Sender: IUnknown;
    Buffer: String;
  end;

  RBlindProtocolIDEvent = record
    Sender: IUnknown;
    Connection: IAbstractConnection;
    Obj: TGuid;
    Id: LongWord;
  end;

  IBlindProtocolEvents = interface
    ['{EC2E9830-C541-11D4-989F-00104B0FA1EF}']
    procedure OnText(var Event: RBlindTextEvent);
    procedure OnRequest(var Event: RBlindProtocolIDEvent);
  end;

implementation

end.
 