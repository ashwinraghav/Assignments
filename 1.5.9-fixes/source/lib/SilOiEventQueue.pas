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

unit SilOiEventQueue;

{$I Defines.inc}

interface

uses
  SilOiWait,
  SilOiThread;

type
  TEvent = type Cardinal;
  TEventParams = array of Variant;

type
  TEventQueueMessage = record
    Event: TEvent;
    Params: TEventParams;
    Result: Integer;
  end;

type
  IPendingEvent = interface(IWaitable)
    ['{9D07C294-0F07-11D4-AD6F-00902794F778}']        
  end;

  IEventQueue = interface
    ['{9D07C291-0F07-11D4-AD6F-00902794F778}']
    function GetThread: IThread;
    function Post(Event: TEvent; Params: array of Variant): Boolean;
    function Send(Event: TEvent; Params: array of Variant; Timeout: LongWord = 0): Integer;
    procedure Cancel;
    property Thread: IThread read GetThread;
  end;

  IEventSink = interface
    ['{9D07C293-0F07-11D4-AD6F-00902794F778}']
    procedure Process(Event: TEvent; const Params: array of Variant; var Result: Integer);
  end;

  IEventReader = interface
    ['{69061861-3D60-11D4-9887-00104B0FA1EF}']
    function GetMessage: Boolean;
    function PeekMessage: Boolean;
  end;
 
implementation

end.
 