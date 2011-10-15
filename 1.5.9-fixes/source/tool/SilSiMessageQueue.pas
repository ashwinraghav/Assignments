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

unit SilSiMessageQueue;

{$I Defines.inc}

interface

uses
  SilOsTypes, SilOiThread;

type
  IThreadMessage = interface;
  IQueuedThreadMessage = interface;
  IThreadMessageReply = interface;
  IThreadMessageQueue = interface;
  IThreadMessageQueueSink = interface;
  IThreadMessageQueueClient = interface;

  // Interface de mensaje entre threads
  IThreadMessage = interface
    ['{339CE57A-2CE6-11D5-BA09-006008AE4EDF}']
    function GetData: Variant;
    function GetID: Cardinal;
    function GetPriority: integer;
    function GetSource: IUnknown;
    procedure Reply( AID: Cardinal; const AData: Variant );
    // properties
    property ID: Cardinal read GetID;
    property Data: Variant read GetData;
    property Source: IUnknown read GetSource;
    property Priority: integer read GetPriority;
  end;

  // Interface de un mensaje encolable
  IQueuedThreadMessage = interface( IThreadMessage )
    ['{339CE57C-2CE6-11D5-BA09-006008AE4EDF}']
    function GetNext: IQueuedThreadMessage;
    function GetQueue: IThreadMessageQueue;
    function GetReplied: boolean;
    function GetReplyTo: IThreadMessageQueueClient;
    procedure SetNext(const Value: IQueuedThreadMessage);
    procedure SetQueue(const Value: IThreadMessageQueue);
    procedure SetReplyTo(const Value: IThreadMessageQueueClient);
    procedure SetReplied(const Value: boolean);
    // properties
    property Next: IQueuedThreadMessage read GetNext write SetNext;
    property Queue: IThreadMessageQueue read GetQueue write SetQueue;
    property ReplyTo: IThreadMessageQueueClient read GetReplyTo write SetReplyTo;
    property Replied: boolean read GetReplied write SetReplied;
  end;

  // Respuesta a un mensaje de un thread
  IThreadMessageReply = interface
    ['{339CE57B-2CE6-11D5-BA09-006008AE4EDF}']
    function GetOrigin: IThreadMessage;
    function GetID: Cardinal;
    function GetData: Variant;
    // properties
    property Origin: IThreadMessage read GetOrigin;
    property ID: Cardinal read GetID;
    property Data: Variant read GetData;
  end;

  // Cola de mensajes
  IThreadMessageQueue = interface
    ['{339CE57D-2CE6-11D5-BA09-006008AE4EDF}']
    function Send(const AMessage: IThreadMessage; Timeout: LongWord = INFINITE): boolean;
    function Post(const AMessage: IThreadMessage;
      const AReplyTo: IThreadMessageQueueClient = nil ): boolean;
    procedure Reply(const AReply: IThreadMessageReply);
    function Connect(const ASink: IThreadMessageQueueSink): boolean;
    procedure Disconnect(const ASink: IThreadMessageQueueSink);
    procedure Stop;
    function GetThread: IThread;
    // properties
    property Thread: IThread read GetThread;
  end;

  // Destino de una cola de mensajes
  IThreadMessageQueueSink = interface
    ['{339CE57E-2CE6-11D5-BA09-006008AE4EDF}']
    procedure OnMessage( const AMessage: IThreadMessage );
  end;

  // Cliente de una cola de mensajes
  IThreadMessageQueueClient = interface
    ['{339CE57F-2CE6-11D5-BA09-006008AE4EDF}']
    procedure OnReply( const AReply: IThreadMessageReply );
  end;

  // Mensaje para ser procesado mediante TObject.Dispatch
  RThreadMessage = packed record
    ID: Word;
    Source: IUnknown;
    Data: Variant;
    Priority: integer;
  end;

implementation

end.
