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

unit SilSiLayerProtocolBlind;

interface

{$include Defines.inc}

uses
  Sil;

type
  IBlindProtocol = interface
    ['{784F49AF-6E27-4B0E-B92B-CCEE22B62002}']
    procedure SendText(const Buffer: String; const Context: IUnknown = nil);
    function Request(const Protocol: TGuid; out Id: LongWord; Timeout: LongWord = INFINITE; const Context: IUnknown = nil): Boolean;
  end;

  RBlindTextEvent = record
    Sender: IBlindProtocol;
    Context: IUnknown;
    Buffer: String;
  end;

  RBlindRequestEvent = record
    Sender: IBlindProtocol;
    Context: IUnknown;
    Protocol: TGuid;
    Id: LongWord;
    Result: Boolean;
  end;

  IBlindProtocolHook = interface
    ['{EC2E9830-C541-11D4-989F-00104B0FA1EF}']
    procedure OnText(var Event: RBlindTextEvent);
    procedure OnRequest(var Event: RBlindRequestEvent);
  end;

implementation

end.
 