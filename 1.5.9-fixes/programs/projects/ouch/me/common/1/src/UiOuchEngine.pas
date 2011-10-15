unit UiOuchEngine;

interface

uses
  Sil,
  UiOuchProtocol;

type
  IOuchEngine = interface;
  IOuchException = interface;
  IOuchToolkit = interface;
  IOuchConnection = interface;
  IOuchConnectionEvents = interface;
  IOuchSession = interface;
  IOuchSessionData = interface;
  IOuchGroups = interface;
  IOuchGroupList = interface;
  IOuchGroup = interface;
  IOuchGroupData = interface;
  IOuchGroupDef = interface;
  IOuchTransport = interface;
  IOuchMessage = interface;
  IOuchMessageData = interface;
  IOuchUsers = interface;
  IOuchUserList = interface;
  IOuchUser = interface;
  IOuchUserData = interface;
  IOuchUserDef = interface;
  IOuchLocalUser = interface;

  EOuchException = class(Exception);

  EOuchError = class(EOuchException)
    Result: Integer;
  end;

(*
 * Engine de comunicaciones del Ouch
 *)

  IOuchEngine = interface
    ['{B2D80D59-A371-4496-9456-B93A357355C3}']
    function GetToolkit: IOuchToolkit;
    procedure Connect(const Parameters: IParameters; const Listener: IUnknown);
    property Toolkit: IOuchToolkit read GetToolkit;
  end;

(*
 * Engine de comunicaciones del Ouch
 *)

  IOuchException = interface
    ['{F63E0B6F-86CF-4DA0-870F-852B6623D7FA}']
  end;

(*
 * Provee un medio para generar instancias de los objetos utilitarios
 *)

  IOuchToolkit = interface
    ['{06AC7A80-8136-46C0-A4CD-5B02D601C1C1}']

    function NewMessage(
                 const Timestamp: TDateTime;
                 const MessageID: TGUID;
                 const Recipients: IOuchUsers;
                 const Senders: IOuchUsers;
                 const Text: string;
                 const Priority: Word = 0;
                 const Kind: TOuchMessageKind = mkInstantMessage
               ): IOuchMessage;

    function NewUserList(
                const Users: IOuchUsers = nil
              ): IOuchUserList; overload;

    function NewUserList(
                const Users: TOuchUserArray
              ): IOuchUserList; overload;

    function NewGroupList(
                const Groups: IOuchGroups = nil
              ): IOuchGroupList; overload;

    function NewGroupList(
                const Groups: TOuchGroupArray
              ): IOuchGroupList; overload;

    function NewUser(
                const ID: TGUID;
                const Data: IParameters = nil
              ): IOuchUserDef; overload;

    function NewUser(
                const Data: ROuchUser
              ): IOuchUserDef; overload;

    function NewLocalUser(
                const UserID: TGUID;
                const Password: string;
                const Data: IParameters = nil
              ): IOuchLocalUser; overload;

    function NewLocalUser(
                const Data: ROuchUser;
                const Password: string
              ): IOuchLocalUser; overload;

    function NewGroup(
                const GroupID: TGUID;
                const Parent: TGUID;
                const Name: string
              ): IOuchGroupDef; overload;

    function NewGroup(
                const Data: ROuchGroup
              ): IOuchGroupDef; overload;
  end;

(*
 * Representa una conexión establecida con un host
 * genérico que puede ser un server u otro cliente
 *)

  IOuchConnection = interface
    ['{BF69B8C5-E91A-4148-B722-921159FBC582}']
    procedure Define(const User: IOuchLocalUser);
    procedure Logon(const User: IOuchLocalUser; const Listener: IUnknown);
    procedure Disconnect;
  end;

(*
 * Eventos generados por la conexión
 *)
 
  ROuchConnectionEvent = record
    Sender: IOuchConnection;
    Thread: IThread;
  end;

  IOuchConnectionEvents = interface
    ['{7F7D2337-C842-476D-A392-320D7EABD14C}']
    procedure OnConnectionClosed(const Event: ROuchConnectionEvent);
    procedure OnConnectionEstablished(const Event: ROuchConnectionEvent);
    procedure OnConnectionFailed(const Event: ROuchConnectionEvent);
  end;

(*
 * Sesión autenticada para un usuario dado
 *)

  IOuchSession = interface
    ['{2EE0FBBD-583F-4AE7-BBDB-2DB49F025F23}']
    function GetData: IOuchSessionData;
    function GetTransport: IOuchTransport;
    procedure Close;
    procedure Query(const Listener: IUnknown);
    procedure Join(const Group: IOuchGroupData; const Password: string; const Listener: IUnknown);
    property Data: IOuchSessionData read GetData;
    property Transport: IOuchTransport read GetTransport;
  end;

  ROuchSessionEvent = record
    Sender: IOuchSession;
    Thread: IThread;
  end;

  ROuchSessionReply = record
    Sender: IOuchSession;
    Thread: IThread;
    Reply: ROuchReply;
  end;

  ROuchSessionGroups = record
    Sender: IOuchSession;
    Thread: IThread;
    Reply: ROuchReply;
    Groups: IOuchGroups;
  end;

  ROuchSessionJoin = record
    Sender: IOuchSession;
    Thread: IThread;
    Reply: ROuchReply;
    Group: IOuchGroup;
  end;

  IOuchSessionEvents = interface
    ['{934F8070-D6E5-4422-AB5C-387F8CA26DBD}']
    procedure OnSessionEstablished(const Event: ROuchSessionReply);
    procedure OnSessionFailed(const Event: ROuchSessionReply);
    procedure OnSessionError(const Event: ROuchSessionReply);
    procedure OnSessionGroups(const Event: ROuchSessionGroups);
    procedure OnSessionJoin(const Event: ROuchSessionJoin);
    procedure OnSessionClosed(const Event: ROuchSessionEvent);
  end;

(*
 * Datos asociados a una sesión
 *)

  IOuchSessionData = interface
    ['{62671D74-29E5-403A-93AD-9F3C2F0D9369}']
    function GetUser: IOuchLocalUser;
    function GetHandle: TGUID;
    property User: IOuchLocalUser read GetUser;
    property Handle: TGUID read GetHandle;
  end;

(*
 * Lista de grupos enumerable y lookupable
 *)

  IOuchGroups = interface(IItemization)
    ['{685D38CF-EF1A-4903-8B9A-5AD713FDDCA0}']
    function GetCount: Integer;
    function GetItem(Index: Integer): IOuchGroupData;
    function Get(const ID: TGUID): IOuchGroupData;
    function Find(const GroupID: TGUID; const IID: TGUID; out Group): Boolean;
    function ToArray: TGuidArray;
    property Count: Integer read GetCount;
    property Items[Ix: Integer]: IOuchGroupData read GetItem; default;
  end;

(*
 * Lista de grupos que agrega la psibilidad de ser modificada
 *)

  IOuchGroupList = interface(IOuchGroups)
    ['{7D1BCE52-DBBB-4D8F-B977-1909A4437D4F}']
    function Add(const Item: IOuchGroupData): Integer;
    function Remove(const Item: IOuchGroupData): Integer;
    procedure Delete(Index: Integer);
  end;

(*
 * Un grupo obtenido del server vía la lisa anterior
 *)

  IOuchGroup = interface
    ['{7A1DD158-26D2-4129-B751-01B61715AAC7}']
    function GetData: IOuchGroupData;
    function GetUsers: IOuchUsers;
    function Talk(const User: TGUID): IOuchUser;
    property Data: IOuchGroupData read GetData;
    property Users: IOuchUsers read GetUsers;
  end;

(*
 * Datos de un grupo
 *)

  IOuchGroupData = interface
    ['{9162D185-5CDC-499F-8105-332F6C070CAD}']
    function GetID: TGUID;
    function GetParent: TGUID;
    function GetName: string;
    function GetPtr: Pointer;
    procedure SetPtr(const Value: Pointer);
    property ID: TGUID read GetID;
    property Parent: TGUID read GetParent;
    property Name: string read GetName;
    property Ptr: Pointer read GetPtr write SetPtr;
  end;

  IOuchGroupDef = interface(IOuchGroupData)
    ['{6CD2907C-72AF-4A64-A56D-5DE8B9A6122E}']
    procedure SetID(const Value: TGUID);
    procedure SetParent(const Value: TGUID);
    procedure SetName(const Value: string);
    property ID: TGUID read GetID write SetID;
    property Parent: TGUID read GetParent write SetParent;
    property Name: string read GetName write SetName;
  end;

(*
 * Transporte de comunicaciones entre dos endpoints   
 *)

  IOuchTransport = interface
    ['{B1E368A8-7186-4432-AA4B-93EF16B92D0F}']
    procedure Notify(const Status: TOuchUserStatus);
    procedure SendMessage(const Msg: IOuchMessage);
  end;

  IOuchTransportEvents = interface
    ['{A1A98A8D-3465-4A02-9FC4-7BF76D000B75}']
    procedure OnSendMessageReply(const Result: ROuchReply; const Msg: IOuchMessage);
    //procedure OnNotifyReply(const NextValidate: TDateTime; const
  end;

(*
 * Encapsula la informacion sobre un mensaje, ya sea de uno a enviar, o de uno recibido.
 *)

  IOuchMessage = interface
    ['{AD8E1613-F22D-40D8-9BCE-9172A954455E}']
    function GetData: IOuchMessageData;
    property Data: IOuchMessageData read GetData;
  end;

(*
 * Datos del mensaje
 *)

  IOuchMessageData = interface
    ['{BCDAB6E8-D0DF-4376-9A3C-3156AB46D4DF}']
    function GetID: TGUID;
    function GetRecipients: IOuchUserList;
    function GetSenders: IOuchUserList;
    function GetTimestamp: TDateTime;
    function GetText: string;
    function GetPriority: Word;
    function GetKind: TOuchMessageKind;
    property ID: TGUID read GetID;
    property Recipients: IOuchUserList read GetRecipients;
    property Senders: IOuchUserList read GetSenders;
    property Timestamp: TDateTime read GetTimestamp;
    property Text: string read GetText;
    property Priority: Word read GetPriority;
    property Kind: TOuchMessageKind read GetKind;
  end;

(*
 * Lista de usuarios que solo permite acceso de lectura
 *)

  IOuchUsers = interface(IItemization)
    ['{A0BBFB86-9E79-4A22-AB93-21B19CA1FD94}']
    function GetCount: Integer;
    function GetItem(Index: Integer): IOuchUserData;
    function Get(const ID: TGUID): IOuchUserData;
    function Find(const GroupID: TGUID; const IID: TGUID; out Group): Boolean;
    function ToArray: TGuidArray;
    property Count: Integer read GetCount;
    property Items[Ix: Integer]: IOuchUserData read GetItem; default;
  end;

(*
 * Lista de usuarios que solo permite acceso de lectura
 *)

  IOuchUserList = interface(IOuchUsers)
    ['{98A7925A-52FA-46C7-B2FE-A15BD8DBB030}']
    function Add(const Item: IOuchUserData): Integer;
    function Remove(const Item: IOuchUserData): Integer;
    procedure Delete(Index: Integer);
  end;

(*
 * Encapsulamiento de un usuario 
 *)

  IOuchUser = interface
    ['{7B676B4F-86E1-4DB0-8D67-62E0F658BF62}']
    function GetData: IOuchUserData;
    function GetTransport: IOuchTransport;
    property Data: IOuchUserData read GetData;
    property Transport: IOuchTransport read GetTransport;
  end;

(*
 * Datos de un usuario
 *)

  IOuchUserData = interface
    ['{A86D6E4A-1373-4409-96FA-0DBAAB3E3AD7}']
    function GetID: TGUID;
    function GetData: IParameters;
    function GetPtr: Pointer;
    procedure SetPtr(const Value: Pointer);
    property ID: TGUID read GetID;
    property Data: IParameters read GetData;
    property Ptr: Pointer read GetPtr write SetPtr;
  end;

  IOuchUserDef = interface(IOuchUserData)
    ['{C3A67A1E-28D7-46C5-96B8-D666226014D2}']
    procedure SetID(const Value: TGUID);
    function GetList: IParameterList;
    property ID: TGUID read GetID write SetID;
    property List: IParameterList read GetList;
  end;

  IOuchLocalUser = interface(IOuchUserDef)
    ['{D153197B-C612-4396-AB77-6DF3EDDA5AB2}']
    function GetPassword: string;
    procedure SetPassword(const Value: string);
    function Edit: Boolean;
    procedure Apply;
    procedure Cancel;
    property Password: String read GetPassword write SetPassword;
  end;

  ROuchUserDataEvent = record
    Sender: IOuchUserData;
    Thread: IThread;
  end;

  IOuchUserDataEvents = interface
    ['{92BE0609-743B-46F6-9C34-FBF18D7F0789}']
    procedure OnUserDataChanged(const Event: ROuchUserDataEvent);
  end;

const
  KOuchEngineLibraryName  = 'OUCHENGINE.DLL';
  KOuchEngineFunctionName = 'Engine';

type
  TOuchEngineFunction = function: IOuchEngine; stdcall;

implementation
end.
 