unit UmOuchMessage;

interface

uses
  Sil,
  UiOuch;

type
  TOuchMessage = class(
    TSilObject,
    IOuchMessage )
  private
    FEngine: Pointer;
    FFrom: IOuchAccount;
    FTime: TDateTime;
    FText: string;
    FRecipients: IOuchAccountList;
    FReplyTo: IOuchMessage;
    FBlindCopy: Boolean;
  private
    function DoGetEngine: IOuchEngine;
  protected // IOuchMessage
    function GetFrom: IOuchAccount;
    procedure SetFrom(const Value: IOuchAccount);
    function GetTime: TDateTime;
    procedure SetTime(const Value: TDateTime);
    function GetText: string;
    procedure SetText(const Value: string);
    function GetRecipients: IOuchAccountList;
    procedure SetRecipients(const Value: IOuchAccountList);
    function GetBlindCopy: Boolean;
    procedure SetBlindCopy(Value: Boolean);
    function GetReplyTo: IOuchMessage;
    procedure SetReplyTo(const Value: IOuchMessage);
  public
    constructor Create(const Engine: IOuchEngine);
    destructor Destroy; override;
  public
    property Engine: IOuchEngine read DoGetEngine;
  end;

implementation

uses
  UmOuchContacts;

{ TOuchMessage }

constructor TOuchMessage.Create(const Engine: IOuchEngine);
begin
  inherited Create;
  FEngine := Pointer(Engine);
  FTime := Sil.DateTime.Now;
  FFrom := Engine.Account; 
  FRecipients := Engine.Factory.AccountList();
end;

destructor TOuchMessage.Destroy;
begin
  FRecipients := nil;
  FFrom := nil;
  FEngine := nil;
  inherited;
end;

function TOuchMessage.GetFrom: IOuchAccount;
begin
  Result := FFrom;
end;

function TOuchMessage.GetRecipients: IOuchAccountList;
begin
  Result := FRecipients;
end;

function TOuchMessage.GetText: string;
begin
  Result := FText;
end;

function TOuchMessage.GetTime: TDateTime;
begin
  Result := FTime;
end;

procedure TOuchMessage.SetTime(const Value: TDateTime);
begin
  FTime := Value;
end;

procedure TOuchMessage.SetFrom(const Value: IOuchAccount);
begin
  FFrom := Value;
end;

procedure TOuchMessage.SetRecipients(const Value: IOuchAccountList);
begin
  FRecipients := Value;
end;

procedure TOuchMessage.SetText(const Value: string);
begin
  FText := Value;
end;

function TOuchMessage.GetBlindCopy: Boolean;
begin
  Result := FBlindCopy;
end;

procedure TOuchMessage.SetBlindCopy(Value: Boolean);
begin
  FBlindCopy := Value;
end;

function TOuchMessage.DoGetEngine: IOuchEngine;
begin
  Result := IOuchEngine(FEngine);
end;

function TOuchMessage.GetReplyTo: IOuchMessage;
begin
  Result := FReplyTo;
end;

procedure TOuchMessage.SetReplyTo(const Value: IOuchMessage);
begin
  FReplyTo := Value;
end;

end.
