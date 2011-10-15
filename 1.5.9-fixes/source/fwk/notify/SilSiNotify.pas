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

unit SilSiNotify;

interface

uses
  SilOcTypes,
  SilOeWait,
  SilLiEnumerator,
  SilLiStream,
  SilLiKey,
  SilLiParameters,
  SilLiStringList;

type
  IEventNotifier = interface;
  IEventServer = interface;
  IEventData = interface;
  IEventConfig = interface;
  IAttachmentList = interface;
  IAttachment = interface;

  IEventNotifierListener = interface
    ['{222294ED-69B4-4328-9632-FA19A60F3E77}']
    procedure OnEventSent(const Event: IEventData);
  end;

  IEventNotifier = interface
    ['{6DF57A36-90A9-47B7-9A34-C8783FF6D9D5}']
    function GetParameters: IParameterList;
    procedure Initialize(const Config: IEventConfig);
    procedure Finalize;
    procedure RegisterServer(const Server: IEventServer; const Listener: IEventNotifierListener = nil);
    procedure UnregisterServer(const Server: IEventServer);
    function NewEvent(const Name: String = ''): IEventData; overload;
    function NewEvent(const Params: IParameters; const RecipientsSeparator: string = ';'): IEventData; overload;
    procedure Notify(const Event: IEventData); overload;
    function Notify(const Event: String; const Text: String = ''): IEventData; overload;
    function Notify(const Event: String; const Text: String; const Args: array of const): IEventData; overload;
    function Notify(const Event: String; const Subject, Text: string): IEventData; overload;
    property Parameters: IParameterList read GetParameters;
  end;

  IEventServer = interface
    ['{CA0F776E-70E2-43F1-85E0-9AB913A55EF3}']
    function GetName: String;
    procedure Initialize(const Config: IEventConfig);
    procedure Finalize;
    procedure Notify(const Event: IEventData);
    property Name: String read GetName;
  end;

  IAttachment = interface
    ['{AA948006-46D2-4FDB-A6D1-48481B09172C}']
    function GetName: String;
    procedure SetName(const Value: String);
    function GetStream: IRandomStream;
    procedure SetStream(const Value: IRandomStream);
    property Name: String read GetName write SetName;
    property Stream: IRandomStream read GetStream write SetStream;
  end;

  IAttachmentList = interface
    ['{6373FD63-4855-4762-A45D-F35C910AAAEA}']
    function CreateNew: IAttachment;
    function Enumerate(var Enum: IEnumerator; out Item: IAttachment): Boolean;
    function Count: Integer;
    procedure Clear;
  end;

  IEventData = interface
    ['{73DA2190-0CFB-4876-8192-437839E3AD75}']
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
    function GetReceived: Boolean;
    procedure SetReceived(Value: Boolean);
    function GetAttachments: IAttachmentList;
    function GetRecipients: IStringList;
    procedure Signal;
    function WaitFor(Interval: Longword = INFINITE): TSyncWaitResult;
    property Name: String read GetName write SetName;
    property Sender: String read GetSender write SetSender;
    property Subject: String read GetSubject write SetSubject;
    property Text: String read GetText write SetText;
    property Response: String read GetResponse write SetResponse;
    property Received: Boolean read GetReceived write SetReceived;
    property Recipients: IStringList read GetRecipients;
    property Attachments: IAttachmentList read GetAttachments;
  end;

  IEventConfig = interface
    ['{9DCBA46C-9AFC-495F-83F0-03C971771D4D}']
    function GetDefParams: IParameters;
    function Configure(const Event: IEventData; const Server: IEventServer): Boolean;
    property DefParams: IParameters read GetDefParams;
  end;

implementation

end.
