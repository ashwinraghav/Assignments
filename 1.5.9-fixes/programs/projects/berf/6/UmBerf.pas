unit UmBerf;

interface

{$include Defines.inc}

uses
  Sil,
  SilXml,
  SilLog,

  UiBerf;

type
  TBerfList = class (
    // extends
    TSilObject,
    // implements
    IBerfList)
  private
    FConfig: IXmlTree;
    FServers: IInterfaceList;
    FProxies: IInterfaceList;
    FAcls: IInterfaceList;
  protected // IBerfList
    function Configure(const FileName: String): Boolean;
    function Start: Boolean;
    procedure Stop;
  end;

implementation

uses
  UmProxyInfo,
  UmGroupInfo,
  UmServer;

const
  CLogKey: String = '$System\SOFTWARE\Siderca\SIL\';

procedure DoInitLog;
begin
  SilLog.Logger.Initialize(CLogKey, SilLog.LogService, SilLog.StackFmt);
end;

procedure DoFinLog;
begin
  SilLog.Logger.Finalize;
end;

{ TBerfList }

function TBerfList.Configure(const FileName: String): Boolean;

  function DoFindAuth(const Name: String): IAuthInfo;
  var
    Enum: IEnumerator;
  begin
    while FProxies.Enumerate(Enum, Result) do
      if Sil.Text.IsEqual(Result.Name, Name) then
        Exit;

    Result := nil;
  end;

  function DoFindAcl(const Name: String): IAclInfo;
  var
    Enum: IEnumerator;
  begin
    while FAcls.Enumerate(Enum, Result) do
      if Sil.Text.IsEqual(Result.Name, Name) then
        Exit;

    Result := nil;
  end;

  procedure DoReadDefineAuth(const Childs: IXmlNodes);
  var
    Enum: IEnumerator;
    Tag: IXmlTag;
    Auth: IAuthInfo;
  begin
    while Childs.Enumerate(Enum, Tag) do
      if Sil.Text.IsEqual(Tag.Name, 'define-auth') then
      begin
        with Tag.Arguments do
        begin
          Auth := TAuthInfo.Create(ReadString('name'));
          Auth.Method := ReadString('method');
        end;

        with Tag.Childs do
        begin
          Auth.Domain := ReadString('domain');
          Auth.User := ReadString('user');
          Auth.Password := ReadString('password');
        end;

        FProxies.Add(Auth);
      end;
  end;

  procedure DoReadDefineAcl(const Childs: IXmlNodes);
  var
    Enum: IEnumerator;
    Tag: IXmlTag;
    Acl: IAclInfo;
  begin
    while Childs.Enumerate(Enum, Tag) do
      if Sil.Text.IsEqual(Tag.Name, 'define-acl') then
      begin
        with Tag.Arguments do
        begin
          Acl := TAclInfo.Create(ReadString('name'));
          Acl.IsDefaultAllow := Sil.Text.IsEqual(ReadString('default-action'), 'allow');
        end;

        with Tag.Childs do
        begin
          Acl.AllowNames.AddStrings(ReadStrings('allow'));
          Acl.DenyNames.AddStrings(ReadStrings('deny'));
        end;

        FAcls.Add(Acl);
      end;
  end;

  procedure DoReadListen(const Childs: IXmlNodes);
  var
    Enum: IEnumerator;
    Tag: IXmlTag;
    Server: IServer;
  begin
    while Childs.Enumerate(Enum, Tag) do
      if Sil.Text.IsEqual(Tag.Name, 'listen') then
      begin
        with Tag.Arguments do
        begin
          Server := TServer.Create(Self);
          Server.Local := ReadString('localport');
          Server.Remote := ReadString('remotehost');
          Server.Auth := DoFindAuth(ReadString('auth'));
          Server.Acl := DoFindAcl(ReadString('acl'));
        end;

        FServers.Add(Server);
      end;
  end;

begin
  Sil.Trace.Enter('TBerfList.Configure', [FileName]);

  FServers := Sil.List.InterfaceList(true);
  FProxies := Sil.List.InterfaceList(true);
  FAcls := Sil.List.InterfaceList(true);

  try
    FConfig := SilXml.Tool.ReadFile(FileName, nil, fmAccessRead, fmShareRead, true);
    Result := true;

    if Result then
      with FConfig.Root do
      begin
        DoReadDefineAuth(Childs);
        DoReadDefineAcl(Childs);
        DoReadListen(Childs);
      end;
  except
    on e: Exception do
    begin
      Sil.Trace.Error(e);
      Result := false;
    end;
  end;

  Sil.Trace.Leave;
end;

function TBerfList.Start: Boolean;
var
  Enum: IEnumerator;
  Server: IServer;
begin
  DoInitLog;
  Sil.Trace.Enter('TBerfList.Start');

  while FServers.Enumerate(Enum, Server) do
    Server.Start;

  Result := true;
  Sil.Trace.Leave;
end;

procedure TBerfList.Stop;
var
  Enum: IEnumerator;
  Item: IUnknown;
begin
  while FServers.Enumerate(Enum, Item) do
    (Item as IServer).Stop;

  FServers.Clear;
  FServers := nil;

  DoFinLog;
end;

end.

