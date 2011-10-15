unit UiProtocol;

interface

uses
  Sil;

const
  PI_CLIENT             = 7250;
  PI_SERVER             = 7250;

const
  PS_BASE               = 1024;
  PS_CREATEACCOUNT      = Succ(PS_BASE);
  PS_GETACCOUNT         = Succ(PS_CREATEACCOUNT);
  PS_LOGON              = Succ(PS_GETACCOUNT);
  PS_SENDMESSAGE        = Succ(PS_LOGON);
  PS_RESPONSE           = Succ(PS_SENDMESSAGE);
  PS_SENDINFO           = Succ(PS_RESPONSE);
  PS_QUERYINFO          = Succ(PS_SENDINFO);
  PS_QUERYUSERS         = Succ(PS_QUERYINFO);
  PS_QUERYOFFLINES      = Succ(PS_QUERYUSERS);
  PS_REQUESTCONNECTION  = Succ(PS_QUERYOFFLINES);
  PS_ACCEPTCONNECTION   = Succ(PS_REQUESTCONNECTION);

type
  IChatSession = interface;
  IClientSideEvents = interface;
  IServerSideEvents = interface;

  TUserStatus = (usAny, usOnline, usOffline);

  PUser = ^TUser;
  TUser = record
    Id: TGuid;
    Updated: TDateTime;
  end;

  TUserArray = array of TUser;

  IClientSide = interface
    ['{2E48FCAF-9776-4FA2-A4C0-C36F116B6999}']
    procedure CreateAccount(const Nick, Password: String; out UserId: TGuid);
    procedure GetAccount(const Nick, Password: String; out UserId: TGuid);
    procedure Logon(const UserId: TGuid; const Password: String);
    procedure SendMessage(const MsgId: TGUID; const Recipients: TGuidArray; const Text: String; const Time: TDateTime; Bc: Boolean);
    procedure SendResponse(const MsgId: TGUID; const ToId: TGuid; const Time: TDateTime);
    procedure SendInfo(const Data: IParameters);
    procedure QueryInfo(const UserId: TGuid; out Data: IParameterList);
    procedure QueryUsers(Status: TUserStatus; out Users: TUserArray);
    procedure QueryOfflines;
    procedure RequestConnection(const Params: IParameters);
    procedure AcceptConnection(const Params: IParameters);
  end;

  RClientSideMessageEvent = record
    Sender: IClientSide;
    MsgId: TGuid;
    UserId: TGuid;
    Text: String;
    Time: TDateTime;
    Recipients: TGuidArray;
  end;

  RClientSideResponseEvent = record
    Sender: IClientSide;
    MsgId: TGuid;
    Recipient: TGuid;
    Time: TDateTime;
  end;

  RClientSideInfoEvent = record
    Sender: IClientSide;
    UserId: TGuid;
    Data: IParameters;
  end;

  RClientSideConnectionEvent = record
    Sender: IClientSide;
    Params: IParameters;
  end;

  IClientSideEvents = interface
    ['{03DA26CC-4164-4A3A-A5E6-7B15EEBBE294}']
    procedure OnReceiveMessage(var Event: RClientSideMessageEvent);
    procedure OnReceiveResponse(var Event: RClientSideResponseEvent);
    procedure OnReceiveInfo(var Event: RClientSideInfoEvent);
    procedure OnConnectionRequest(var Event: RClientSideConnectionEvent);
    procedure OnConnectionAccept(var Event: RClientSideConnectionEvent);
  end;

  IChatSession = interface
    ['{03F43A3F-FB06-4A13-8727-38E2041BD430}']
    procedure SendText(const PrivateRecipients: TGuidArray; const Text: String);
    procedure Leave;
    procedure Drop;
  end;

  IServerSide = interface
    ['{C812F962-DD04-4B7A-9AF7-BA9629282B09}']
    procedure SendMessage(const MsgId: TGUID; const UserId: TGuid; const Text: String; const Time: TDateTime; const Recipients: TGuidArray);
    procedure SendResponse(const MsgId: TGUID; const FromId: TGuid; const Time: TDateTime);
    procedure SendInfo(const UserId: TGuid; const Data: IParameters);
    procedure RequestConnection(const Params: IParameters);
    procedure AcceptConnection(const Params: IParameters);
  end;

  RServerSideCreateAccountEvent = record
    Sender: IServerSide;
    Nick: String;
    Password: String;
    UserId: TGuid;
  end;

  RServerSideGetAccountEvent = record
    Sender: IServerSide;
    Nick: String;
    Password: String;
    UserId: TGuid;
  end;

  RServerSideLogonEvent = record
    Sender: IServerSide;
    UserId: TGuid;
    Password: String;
  end;

  RServerSideMessageEvent = record
    Sender: IServerSide;
    MsgId: TGUID;  
    Recipients: TGuidArray;
    Text: String;
    Time: TDateTime;
    Bc: Boolean;
  end;

  RServerSideInfoEvent = record
    Sender: IServerSide;
    Data: IParameters;
  end;

  RServerSideResponseEvent = record
    Sender: IServerSide;
    MsgId: TGUID; 
    Recipient: TGuid;
    Time: TDateTime;
  end;

  RServerSideQueryInfoEvent = record
    Sender: IServerSide;
    UserId: TGuid;
    Data: IParameters;
  end;

  RServerSideQueryUsersEvent = record
    Sender: IServerSide;
    Status: TUserStatus;
    Users: TUserArray;
  end;

  RServerSideQueryOfflinesEvent = record
    Sender: IServerSide;
  end;

  RServerSideConnectionEvent = record
    Sender: IServerSide;
    Params: IParameters;
  end;

  IServerSideEvents = interface
    ['{B01342F5-610C-4639-BEDE-0744F87539F2}']
    procedure OnCreateAccount(var Event: RServerSideCreateAccountEvent);
    procedure OnGetAccount(var Event: RServerSideGetAccountEvent);
    procedure OnLogon(var Event: RServerSideLogonEvent);
    procedure OnSendMessage(var Event: RServerSideMessageEvent);
    procedure OnSendInfo(var Event: RServerSideInfoEvent);
    procedure OnSendResponse(var Event: RServerSideResponseEvent);
    procedure OnQueryInfo(var Event: RServerSideQueryInfoEvent);
    procedure OnQueryUsers(var Event: RServerSideQueryUsersEvent);
    procedure OnQueryOfflines(var Event: RServerSideQueryOfflinesEvent);
    procedure OnConnectionRequest(var Event: RServerSideConnectionEvent);
    procedure OnConnectionAccept(var Event: RServerSideConnectionEvent);
  end;

implementation
end.
