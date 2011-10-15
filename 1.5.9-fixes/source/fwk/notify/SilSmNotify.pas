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

unit SilSmNotify;

interface

uses
  Sil,
  SilSiNotify;

type
  TEventNotifier = class (
    // extends
    TSilInterfacedObject,
    // implements
    IEventNotifier,
    IRunnable)
  private
    FConfig: IEventConfig;
    FParams: IParameterList;
    FThread: IThread;
    FQueue: IInterfaceQueue;
    FServers: IInterfaceList;
    FListener: IEventNotifierListener;
    procedure DoNotify(const Event: IEventData);
  protected // IRunnable
    procedure Run(const Sender: IThread);
  protected // IEventNotifier
    function GetParameters: IParameterList;
    procedure Initialize(const Config: IEventConfig);
    procedure Finalize;
    procedure RegisterServer(const Server: IEventServer; const Listener: IEventNotifierListener);
    procedure UnregisterServer(const Server: IEventServer);
    function NewEvent(const Name: String = ''): IEventData; overload;
    function NewEvent(const Params: IParameters; const RecipientsSeparator: string = ';'): IEventData; overload;
    procedure Notify(const Event: IEventData); overload;
    function Notify(const Event: String; const Text: String = ''): IEventData; overload;
    function Notify(const Event: String; const Text: String; const Args: array of const): IEventData; overload;
    function Notify(const Event: String; const Subject, Text: string): IEventData; overload;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TEventData = class (
    // extends
    TSilInterfacedObject,
    // implements
    IEventData)
  private
    FName: String;
    FSender: String;
    FSubject: String;
    FText: String;
    FResponse: String;
    FReceived: Boolean;
    FRecipients: IStringList;
    FAttachments: IAttachmentList;
    FSent: IEvent;
  private
    procedure DoInit;
  protected // IEventData
    function GetName: String;
    procedure SetName(const Value: String);
    function GetSender: String;
    procedure SetSender(const Value: String);
    function GetSubject: String;
    procedure SetSubject(const Value: String);
    function GetText: String;
    procedure SetText(const Value: String);
    function GetResponse: String;
    procedure SetResponse(const Value: String);
    function GetRecipients: IStringList;
    function GetReceived: Boolean;
    procedure SetReceived(Value: Boolean);
    function GetAttachments: IAttachmentList;
    procedure Signal;
    function WaitFor(Interval: Longword = INFINITE): TSyncWaitResult;
  public
    constructor Create; overload;
    constructor Create(const Params: IParameters; const RecipientsSeparator: string); overload;
    destructor Destroy; override;
  end;

  TSilAttachment = class (TSilObject, IAttachment)
  private
    FName: String;
    FStream: IRandomStream;
  protected // IAttachment
    function GetName: String;
    procedure SetName(const Value: String);
    function GetStream: IRandomStream;
    procedure SetStream(const Value: IRandomStream);
  end;

  TSilAttachmentList = class (TSilObject, IAttachmentList)
  private
    FList: IInterfaceList;
  protected // IAttachmentList
    function CreateNew: IAttachment; reintroduce;
    function Enumerate(var Enum: IEnumerator; out Item: IAttachment): Boolean;
    procedure Clear;
    function Count: Integer;
  public
    constructor Create;
    destructor Destroy; override;
  end;

implementation

uses SilLtList, SilLiParameters;

{ TEventNotifier }

constructor TEventNotifier.Create;
begin
  inherited Create;

  FServers := Sil.List.InterfaceList(true);
  FQueue := Sil.List.InterfaceQueue;
  FParams := Sil.List.Parameters;
end;

destructor TEventNotifier.Destroy;
begin
  FListener := nil;
  FServers := nil;
  FQueue := nil;
  FParams := nil;
  FConfig := nil;

  inherited;
end;

procedure TEventNotifier.Initialize(const Config: IEventConfig);
begin
  FConfig := Config;
  FThread := Sil.OS.Thread.Spawn('sil.notify', Self);
end;

procedure TEventNotifier.Finalize;
var
  Enum: IEnumerator;
  Item: IUnknown;
begin
  FQueue.Cancel;

  if FThread <> nil then
  begin
    FThread.Termination.WaitFor(INFINITE, true);
    FThread := nil;
  end;

  while FServers.Enumerate(Enum, Item) do
    UnregisterServer(Item as IEventServer);
end;

procedure TEventNotifier.Run(const Sender: IThread);
var
  Event: IEventData;
  sSub, sSnd: String;
begin
  while FQueue.Get(IEventData, Event) do
    if Assigned(Event) then
      try
        sSub := FParams['subject'];
        sSnd := FParams['sender'];

        if Str.IsEmpty(Event.Subject) and Str.NotEmpty(sSub) then
          Event.Subject := Str.Format('[%s] %s', [sSub, Event.Name]);

        if Str.IsEmpty(Event.Sender) and Str.NotEmpty(sSnd) then
          Event.Sender := sSnd;

        DoNotify(Event);

        if FListener <> nil then
          FListener.OnEventSent(Event);
      except
      end;
end;

procedure TEventNotifier.DoNotify(const Event: IEventData);
var
  Enum: IEnumerator;
  Item: IUnknown;
begin
  while FServers.Enumerate(Enum, Item) do
    (Item as IEventServer).Notify(Event);

  Event.Signal;
end;

procedure TEventNotifier.RegisterServer(const Server: IEventServer; const Listener: IEventNotifierListener);
begin
  if FServers.IndexOf(Server) = -1 then
  begin
    FServers.Add(Server);
    Server.Initialize(FConfig);
    FListener := Listener;
  end;
end;

procedure TEventNotifier.UnregisterServer(const Server: IEventServer);
begin
  if FServers.IndexOf(Server) > -1 then
  begin
    FServers.Remove(Server);
    Server.Finalize;
  end;
end;

function TEventNotifier.NewEvent(const Name: String): IEventData;
begin
  Result := TEventData.Create;
  Result.Name := Name;
end;

function TEventNotifier.NewEvent(const Params: IParameters; const RecipientsSeparator: string = ';'): IEventData; 
begin
  Result := TEventData.Create( Params, RecipientsSeparator );
end;

function TEventNotifier.Notify(const Event, Text: String; const Args: array of const): IEventData;
begin
  Result := Notify(Event, '', Str.Format(Text, Args));
end;

function TEventNotifier.Notify(const Event, Text: String): IEventData;
begin
  Result := Notify(Event, '', Text);
end;

function TEventNotifier.Notify(const Event, Subject, Text: string): IEventData;
begin
  Result := NewEvent(Event);
  Result.Subject := Subject;
  Result.Text := Text;
  Notify(Result);
end;

procedure TEventNotifier.Notify(const Event: IEventData);
begin
  FQueue.Put(Event);
end;

function TEventNotifier.GetParameters: IParameterList;
begin
  Result := FParams;
end;

{ TEventData }

constructor TEventData.Create;
begin
  inherited Create;
  FRecipients := Sil.List.StringList;
  DoInit;
end;

constructor TEventData.Create(const Params: IParameters; const RecipientsSeparator: string);
var
  RecipientsStr: string;
begin
  inherited Create;
  FName := Params.Get( 'Name', '' );
  FSender := Params.Get( 'Sender', '' );
  FSubject := Params.Get( 'Subject', '' );
  FText := Params.Get( 'Text', '' );
  RecipientsStr := Params.Get( 'Recipients', '' );
  FRecipients := Sil.List.StringList(RecipientsStr, RecipientsSeparator);

  DoInit;
end;

procedure TEventData.DoInit;
begin
  FSent := Sil.OS.Ipc.Event;
  FAttachments := TSilAttachmentList.Create;
end;

destructor TEventData.Destroy;
begin
  FRecipients := nil;
  FAttachments := nil;
  inherited;
end;

function TEventData.GetAttachments: IAttachmentList;
begin
  Result := FAttachments;
end;

function TEventData.GetName: String;
begin
  Result := FName;
end;

function TEventData.GetRecipients: IStringList;
begin
  Result := FRecipients;
end;

function TEventData.GetSender: String;
begin
  Result := FSender;
end;

function TEventData.GetSubject: String;
begin
  Result := FSubject;
end;

function TEventData.GetText: String;
begin
  Result := FText;
end;

procedure TEventData.SetName(const Value: String);
begin
  FName := Value;
end;

procedure TEventData.SetSender(const Value: String);
begin
  FSender := Value;
end;

procedure TEventData.SetSubject(const Value: String);
begin
  FSubject := Value;
end;

procedure TEventData.SetText(const Value: String);
begin
  FText := Value;
end;

procedure TEventData.Signal;
begin
  FSent.Signal;
end;

function TEventData.WaitFor(Interval: Longword): TSyncWaitResult;
begin
  Result := FSent.WaitFor(Interval, true);
end;

function TEventData.GetResponse: String;
begin
  Result := FResponse;
end;

procedure TEventData.SetResponse(const Value: String);
begin
  FResponse := Value;
end;

function TEventData.GetReceived: Boolean;
begin
  Result := FReceived;
end;

procedure TEventData.SetReceived(Value: Boolean);
begin
  FReceived := Value;
end;

{ TSilAttachment }

function TSilAttachment.GetName: String;
begin
  Result := FName;
end;

function TSilAttachment.GetStream: IRandomStream;
begin
  Result := FStream;
end;

procedure TSilAttachment.SetName(const Value: String);
begin
  FName := Value;
end;

procedure TSilAttachment.SetStream(const Value: IRandomStream);
begin
  FStream := Value;
end;

{ TSilAttachmentList }

constructor TSilAttachmentList.Create;
begin
  inherited;
  FList := Sil.List.InterfaceList;
end;

destructor TSilAttachmentList.Destroy;
begin
  Clear;
  FList := nil;

  inherited;
end;

procedure TSilAttachmentList.Clear;
begin
  if Assigned(FList) then FList.Clear;
end;

function TSilAttachmentList.CreateNew: IAttachment;
begin
  Result := TSilAttachment.Create;
  FList.Add(Result);
end;

function TSilAttachmentList.Enumerate(var Enum: IEnumerator; out Item: IAttachment): Boolean;
begin
  Result := FList.Enumerate(Enum, Item);
end;

function TSilAttachmentList.Count: Integer;
begin
  Result := FList.Count;
end;

end.
