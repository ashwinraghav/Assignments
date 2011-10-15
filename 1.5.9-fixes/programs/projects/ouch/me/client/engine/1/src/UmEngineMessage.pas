unit UmEngineMessage;

interface

uses
  Sil,

  UiOuchProtocol,
  UiOuchEngine;

type
  REngineData = record
    MessageID: TGUID;
    Timestamp: TDateTime;
    Recipients: IOuchUserList;
    Senders: IOuchUserList;
    Text: string;
    Priority: Word;
    Kind: TOuchMessageKind;
  end;

  TEngineMessage = class(
    TInterfacedObject,
    IOuchMessage,
    IOuchMessageData )
  private
    FEngine: IOuchEngine;
    FData: REngineData;    
  protected // IOuchMessage
    function GetData: IOuchMessageData;
  protected // IOuchMessageData
    function GetID: TGUID;
    function GetRecipients: IOuchUserList;
    function GetSenders: IOuchUserList;
    function GetTimestamp: TDateTime;
    function GetText: string;
    function GetPriority: Word;
    function GetKind: TOuchMessageKind;
  public
    constructor Create(
                 const Engine: IOuchEngine;
                 const Timestamp: TDateTime;
                 const MessageID: TGUID;
                 const Recipients: IOuchUsers;
                 const Senders: IOuchUsers;
                 const Text: string;
                 const Priority: Word;
                 const Kind: TOuchMessageKind);
    destructor Destroy; override;
  end;

implementation

{ TEngineMessage }

constructor TEngineMessage.Create(const Engine: IOuchEngine; const Timestamp: TDateTime; const MessageID: TGUID; const Recipients,
  Senders: IOuchUsers; const Text: string; const Priority: Word;
  const Kind: TOuchMessageKind);
begin
  inherited Create;
  FEngine := Engine;
  FData.MessageID := MessageID;
  FData.Timestamp := Timestamp;
  FData.Senders := Engine.Toolkit.NewUserList(Senders);
  FData.Text := Text;
  FData.Recipients := Engine.Toolkit.NewUserList(Recipients);
  FData.Priority := Priority;
  FData.Kind := Kind;
end;

destructor TEngineMessage.Destroy;
begin
  FData.Senders := nil;
  FData.Recipients := nil;
  FEngine := nil;
  inherited;
end;

function TEngineMessage.GetData: IOuchMessageData;
begin
  Result := Self;
end;

function TEngineMessage.GetID: TGUID;
begin
  Result := FData.MessageID;
end;

function TEngineMessage.GetKind: TOuchMessageKind;
begin
  Result := FData.Kind;
end;

function TEngineMessage.GetPriority: Word;
begin
  Result := FData.Priority;
end;

function TEngineMessage.GetRecipients: IOuchUserList;
begin
  Result := FData.Recipients;
end;

function TEngineMessage.GetSenders: IOuchUserList;
begin
  Result := FData.Senders;
end;

function TEngineMessage.GetText: string;
begin
  Result := FData.Text;
end;

function TEngineMessage.GetTimestamp: TDateTime;
begin
  Result := FData.Timestamp;
end;

end.
