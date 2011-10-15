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

unit SilSmMessageQueue;

{$I Defines.inc}

interface

uses
  SilLkInterfaced,
  SilSiMessageQueue,
  SilOiThread,
  SilOiIpc,
  SilOsTypes;

type
  // Mensaje entre threads
  TThreadMessage = class(
    // extends
    TSilInterfacedObject,
    // implements
    IThreadMessage )
  private
    FSource: IUnknown;
    FID: Cardinal;
    FData: Variant;
    FPriority: integer;
  protected
    procedure DoReply( AID: Cardinal; const AData: Variant ); virtual; abstract;
  protected // IThreadMessage
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
  public
    constructor Create( const ASource: IUnknown; AID: Cardinal; const AData: Variant; APrio: integer ); reintroduce;
    destructor Destroy; override;
  end;

  // Interface de un mensaje encolable
  TQueuedThreadMessage = class (
    // extends
    TThreadMessage,
    // implements
    IThreadMessage,
    IQueuedThreadMessage )
  private
    FReplied: boolean;
    FQueue: IThreadMessageQueue;
    FNext: IQueuedThreadMessage;
    FReplyTo: IThreadMessageQueueClient;
  protected
    procedure DoReply( AID: Cardinal; const AData: Variant ); override;
  protected // IQueuedThreadMessage
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
  public
    destructor Destroy; override;
  end;

  // Respuesta a un mensaje de un thread
  TThreadMessageReply = class(
    // extends
    TSilInterfacedObject,
    // implements
    IThreadMessageReply )
  private
    FOrigin: IThreadMessage;
    FID: Cardinal;
    FData: Variant;
  protected // IThreadMessageReply
    function GetData: Variant;
    function GetID: Cardinal;
    function GetOrigin: IThreadMessage;
    // properties
    property Origin: IThreadMessage read GetOrigin;
    property ID: Cardinal read GetID;
    property Data: Variant read GetData;
  public
    constructor Create( const AOrigin: IThreadMessage; AID: Cardinal; AData: Variant );
    destructor Destroy; override;
  end;

  // Cola de mensajes
  TThreadMessageQueue = class (
    // extends
    TSilInterfacedObject,
    // implements
    IThreadMessageQueue,
    IRunnable,
    IThreadHook )
  private
    FSink: IThreadMessageQueueSink;
    FThread: IThread;
    FStarted: IEvent;
    FNotify: IEvent;
    FChain: IQueuedThreadMessage;
    FFinished: IEvent;
  protected // IThreadMessageQueue
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
  protected // IRunnable
    procedure Run(const AThread: IThread);
  protected // IThreadHook
    function Initialize(const Context: IThread): Boolean;
    procedure Finalize(const Context: IThread);
    procedure Suspended(const Context: IThread);
    procedure Resumed(const Context: IThread);
  public
    constructor Create(const ASink: IThreadMessageQueueSink;
      const AThreadName: string = ''); reintroduce;
    destructor Destroy; override;
  end;

  // Despachante de mensajes
  TThreadMessageDispatched = class(
    // extends
    TSilInterfacedObject,
    // implements
    IThreadMessageQueueSink )
  private
    FObject: TObject;
  protected // IThreadMessageQueueSink
    procedure OnMessage( const AMessage: IThreadMessage );
  public
    constructor Create( const AObject: TObject ); reintroduce;
    destructor Destroy; override;
  end;


implementation

uses
{$IFDEF D60} //LINUX}
  Variants,
{$ENDIF}
  SilBtStr,
  SilOtTool,
  SilLtLock,
  SilLtReference;

type
  // Para sincronización de respuestas
  ISyncThreadMessageReply = interface
    ['{2428DF12-2EA3-11D5-BA09-006008AE4EDF}']
    function GetReplyClient: IThreadMessageQueueClient;
    function GetEvent: IEvent;
    function GetTheAnswer: IThreadMessageReply;
    // properties
    property ReplyClient: IThreadMessageQueueClient read GetReplyClient;
    property Event: IEvent read GetEvent;
    property TheAnswer: IThreadMessageReply read GetTheAnswer;
  end;

  // Sincronizador de respuesta
  TSyncThreadMessageReply = class(
    // extends
    TSilInterfacedObject,
    // implements
    IThreadMessageQueueClient,
    ISyncThreadMessageReply )
  private
    FEvent: IEvent;
    FTheAnswer: IThreadMessageReply;
  protected // IThreadMessageQueueClient
    procedure OnReply( const AReply: IThreadMessageReply );
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
  protected // ISyncThreadMessageReply
    function GetReplyClient: IThreadMessageQueueClient;
    function GetEvent: IEvent;
    function GetTheAnswer: IThreadMessageReply;
    // properties
    property ReplyClient: IThreadMessageQueueClient read GetReplyClient;
    property Event: IEvent read GetEvent;
    property TheAnswer: IThreadMessageReply read GetTheAnswer;
  end;

{ TThreadMessage }

constructor TThreadMessage.Create(const ASource: IUnknown; AID: Cardinal;
  const AData: Variant; APrio: integer);
begin
  inherited Create;

  FSource := ASource;
  FID := AID;
  FData := AData;
  FPriority := APrio;
end;

destructor TThreadMessage.Destroy;
begin
  FData := Unassigned;
  inherited;
end;

function TThreadMessage.GetData: Variant;
begin
  result := FData;
end;

function TThreadMessage.GetID: Cardinal;
begin
  result := FID;
end;

function TThreadMessage.GetPriority: integer;
begin
  result := FPriority;
end;

function TThreadMessage.GetSource: IUnknown;
begin
  result := FSource;
end;

procedure TThreadMessage.Reply(AID: Cardinal; const AData: Variant);
begin
  DoReply( AID, AData );
  FSource := nil;
end;

{ TQueuedThreadMessage }

destructor TQueuedThreadMessage.Destroy;
begin
  FQueue := nil;
  FReplyTo := nil;
  FNext := nil;
  inherited;
end;

procedure TQueuedThreadMessage.DoReply(AID: Cardinal;
  const AData: Variant);
var
  reply: IThreadMessageReply;
begin
  if FReplied then exit;
  FReplied := true;
  if ( FQueue <> nil ) then
  begin
    reply := TThreadMessageReply.Create( self, AID, AData );
    FQueue.Reply( reply );
  end;

  FQueue := nil;
  FReplyTo := nil;
end;

function TQueuedThreadMessage.GetNext: IQueuedThreadMessage;
begin
  result := FNext;
end;

function TQueuedThreadMessage.GetQueue: IThreadMessageQueue;
begin
  result := FQueue;
end;

function TQueuedThreadMessage.GetReplied: boolean;
begin
  result := FReplied;
end;

function TQueuedThreadMessage.GetReplyTo: IThreadMessageQueueClient;
begin
  result := FReplyTo;
end;

procedure TQueuedThreadMessage.SetNext(const Value: IQueuedThreadMessage);
begin
  FNext := Value;
end;

procedure TQueuedThreadMessage.SetQueue(const Value: IThreadMessageQueue);
begin
  FQueue := Value;
end;

procedure TQueuedThreadMessage.SetReplied(const Value: boolean);
begin
  FReplied := Value;
end;

procedure TQueuedThreadMessage.SetReplyTo(
  const Value: IThreadMessageQueueClient);
begin
  FReplyTo := Value;
end;

{ TThreadMessageReply }

constructor TThreadMessageReply.Create(const AOrigin: IThreadMessage;
  AID: Cardinal; AData: Variant);
begin
  inherited Create();

  FOrigin := AOrigin;
  FID := AID;
  FData := AData;
end;

destructor TThreadMessageReply.Destroy;
begin
  FOrigin := nil;
  FData := Unassigned;
  inherited;
end;

function TThreadMessageReply.GetData: Variant;
begin
  result := FData;
end;

function TThreadMessageReply.GetID: Cardinal;
begin
  result := FID;
end;

function TThreadMessageReply.GetOrigin: IThreadMessage;
begin
  result := FOrigin;
end;

{ TThreadMessageQueue }

constructor TThreadMessageQueue.Create(const ASink: IThreadMessageQueueSink;
  const AThreadName: string);
begin
  inherited Create();

  FSink := ASink;
  FStarted := OS.IPC.Event();
  FNotify := OS.IPC.Event();
  FFinished := OS.IPC.Event();
  FThread := OS.Thread.Spawn(Str.IIf(Str.IsEmpty(AThreadName), ClassName, AThreadName), self);
  FStarted.WaitFor(INFINITE, true);
end;

destructor TThreadMessageQueue.Destroy;
begin
  if Assigned(FThread) then
  begin
    FFinished.Signal;
    FThread.Termination.WaitFor(INFINITE, true);
  end;

  Locked;
  FSink := nil;
  FStarted := nil;
  FNotify := nil;
  FThread := nil;
  FFinished := nil;

  inherited;
end;

function TThreadMessageQueue.Send(const AMessage: IThreadMessage;
  Timeout: LongWord): boolean;
var
  sigidx: integer;
  msg: IQueuedThreadMessage;
  syncreply: ISyncThreadMessageReply;
begin
  msg := ( AMessage as IQueuedThreadMessage );

  syncreply := TSyncThreadMessageReply.Create;
  result := Post( msg, syncreply.ReplyClient ) and
    OS.Wait.Any( [ FThread.Termination, syncreply.Event ], Timeout, sigidx ) and
    ( sigidx = 1 );
  if result then
    Reply( syncreply.TheAnswer );
end;

function TThreadMessageQueue.Post(const AMessage: IThreadMessage;
  const AReplyTo: IThreadMessageQueueClient): Boolean;
var
  prio: integer;
  msg, tmpmsg: IQueuedThreadMessage;
begin
  Locked;

  msg := ( AMessage as IQueuedThreadMessage );
  msg.Queue := self;
  msg.ReplyTo := AReplyTo;
  msg.Replied := false;

  // Intercala el mensaje en la cola según la prioridad
  if ( FChain <> nil ) then
  begin
    tmpmsg := FChain;
    prio := AMessage.Priority;
    while ( tmpmsg.Next <> nil ) and ( tmpmsg.Next.Priority >= prio ) do
      tmpmsg := tmpmsg.Next;
    msg.Next := tmpmsg.Next;
    tmpmsg.Next := msg;
  end
  else
    FChain := msg;

  FNotify.Signal;
  result := true;
end;

procedure TThreadMessageQueue.Reply(const AReply: IThreadMessageReply);
var
  msg: IQueuedThreadMessage;
  clnt: IThreadMessageQueueClient;
begin
  if ( AReply = nil ) or ( AReply.Origin = nil ) then
    exit;

  msg := ( AReply.Origin as IQueuedThreadMessage );
  if ( msg.ReplyTo <> nil ) then
    msg.ReplyTo.OnReply( AReply ) else
  if Reference.GetInterface( AReply.Origin.Source, IThreadMessageQueueClient, clnt ) then
    clnt.OnReply( AReply );
end;

function TThreadMessageQueue.Connect(const ASink: IThreadMessageQueueSink): boolean;
begin
  FSink := ASink;
  result := true;
end;

procedure TThreadMessageQueue.Disconnect(const ASink: IThreadMessageQueueSink);
begin
  if Reference.SameObject( FSink, ASink ) then
    FSink := nil;
end;

procedure TThreadMessageQueue.Run(const AThread: IThread);
var
  sigidx: Integer;
  lck: ILock;
  curr: IQueuedThreadMessage;
begin
  FStarted.Signal;

  while OS.Wait.Any( [ FFinished, FNotify ], INFINITE, sigidx ) do
    if ( sigidx = 1 ) then
    begin
      lck := Locked;
      FNotify.Reset;
      while ( FChain <> nil ) do
      begin
        curr := FChain;
        FChain := FChain.Next;
        lck.Release;
        if ( FSink <> nil ) then
          FSink.OnMessage( curr );

        if not curr.Replied then
          curr.Reply( 0, 0 );
        curr := nil;

        lck := Locked;
      end;
      lck.Release;
    end
    else
      break;
end;

procedure TThreadMessageQueue.Stop;
begin
  FSink := nil;
  Finalize( FThread );
end;

procedure TThreadMessageQueue.Finalize(const Context: IThread);
begin
  Locked;
  FFinished.Signal;
end;

function TThreadMessageQueue.Initialize(const Context: IThread): Boolean;
begin
  result := true;
end;

procedure TThreadMessageQueue.Resumed(const Context: IThread);
begin

end;

procedure TThreadMessageQueue.Suspended(const Context: IThread);
begin

end;

function TThreadMessageQueue.GetThread: IThread;
begin
  result := FThread;
end;

{ TSyncThreadMessageReply }

constructor TSyncThreadMessageReply.Create;
begin
  inherited Create;
  FEvent := OS.IPC.Event();
end;

destructor TSyncThreadMessageReply.Destroy;
begin
  FEvent := nil;
  inherited;
end;

function TSyncThreadMessageReply.GetEvent: IEvent;
begin
  result := FEvent;
end;

function TSyncThreadMessageReply.GetReplyClient: IThreadMessageQueueClient;
begin
  result := self;
end;

function TSyncThreadMessageReply.GetTheAnswer: IThreadMessageReply;
begin
  result := FTheAnswer;
end;

procedure TSyncThreadMessageReply.OnReply(
  const AReply: IThreadMessageReply);
begin
  FTheAnswer := AReply;
  FEvent.Signal;
end;

{ TThreadMessageDispatched }

constructor TThreadMessageDispatched.Create(const AObject: TObject);
begin
  inherited Create;

  FObject := AObject;
end;

destructor TThreadMessageDispatched.Destroy;
begin
  inherited;
end;

procedure TThreadMessageDispatched.OnMessage(
  const AMessage: IThreadMessage);
var
  msg: RThreadMessage;
begin
  msg.ID := AMessage.ID;
  msg.Source := AMessage.Source;
  msg.Data := AMessage.Data;
  msg.Priority := AMessage.Priority;
  FObject.Dispatch( msg );
  msg.Source := nil;
end;

end.

