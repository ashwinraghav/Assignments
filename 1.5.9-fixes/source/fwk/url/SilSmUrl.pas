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

unit SilSmUrl;

{$I Defines.inc}

interface

uses
  Sil,
  SilCoder,
  SilSiUrl;

  // Información contenida en una URL
  
  // [protocol:] // [user@]<server>[:password] / <path>[?par1=val1[&parn=valn]]

(*)
    URL         = ( absoluteURL | relativeURL )
    absoluteURL = protocol ":" relativeURL
    protocol    = 1*( alpha | digit | "+" | "-" | "." )

    relativeURL = net_path | abs_path | rel_path

    net_path    = "//" net_loc [ abs_path ]

    net_loc     = [ user [ ":" password ] "@" ] host [":" port]
    user        = 1*( char )
    password    = 1*( char )
    host        = 1*( char )
    port        = 1*( digit )

    abs_path    = "/"  rel_path

    rel_path    = [ path ] [ ";" params ] [ "?" query ]
    
    path        = fsegment *( "/" segment )
    
    fsegment    = 1*pchar
    segment     =  *pchar
    
    params      = param *( ";" param )
    param       = *( pchar | "/" )
    query       =  *( uchar | reserved )
    fragment    =  *( uchar | reserved )
(*)

type
  TSilUrlData = class(
    TSilObject,
    IUrlData )
  protected // IUrlData
    function GetIsEmpty: Boolean; virtual;
    function GetIsAssigned: Boolean; virtual; 
    function GetText: string; virtual; abstract;
    procedure SetText(const Value: string); virtual; abstract; 
    procedure Clear; virtual; 
  public
    constructor Create(const Text: string = '');
    destructor Destroy; override; 
  end; 

  TSilUrl = class(
    TSilUrlData,
    IUrl )
  private
    FProtocol: IUrlProtocol;
    FAddress: IUrlAddress;
    FPath: IUrlPath;
    FParams: IUrlParams;
    FQuery: IUrlQuery;
  private
    procedure DoParseText(const Value: string);
    function DoParseProtocol(var Buffer: string): string;
    function DoParseAddress(var Buffer: string): string;
    function DoParsePath(var Buffer: string): string;
    function DoParseParams(var Buffer: string): string;
    function DoParseQuery(var Buffer: string): string;
  protected
    function GetIsAssigned: Boolean; override;  
    function GetText: string; override;
    procedure SetText(const Value: string); override;
    procedure Clear; override;  
  protected
    procedure DoChanged;
  protected // IUrl
    function GetProtocol: IUrlProtocol;
    function GetAddress: IUrlAddress;
    function GetPath: IUrlPath;
    function GetParams: IUrlParams;
    function GetQuery: IUrlQuery;
  public
    constructor Create(const Text: string = ''); reintroduce;
    destructor Destroy; override;
  end;

  TSilUrlProtocol = class(
    TSilUrlData,
    IUrlProtocol )
  private
    FProtocol: string;
  protected // IUrlProtocol
  protected // IUrlData
    function GetIsAssigned: Boolean; override;  
    function GetText: string; override;
    procedure SetText(const Value: string); override;
  end;

  TSilUrlAddress = class(
    TSilUrlData,
    IUrlAddress )
  private
    FLogin: IUrlLogin;
    FHost: IUrlHost;
  protected // IUrlAddress
    function GetHost: IUrlHost;
    function GetLogin: IUrlLogin;
  protected // IUrlData 
    function GetIsAssigned: Boolean; override;  
    function GetText: string; override;
    procedure SetText(const Value: string); override;
    procedure Clear; override;
  public
    constructor Create(const Text: string = '');
  end;

  TSilUrlHost = class(
    TSilUrlData,
    IUrlHost )
  private
    FServer: string;
    FPort: string;
  protected // IUrlHost
    function GetServer: string;
    procedure SetServer(const Value: string);
    function GetPort: string;
    procedure SetPort(Value: string);
  protected // IUrlData
    function GetIsAssigned: Boolean; override;  
    function GetText: string; override;
    procedure SetText(const Value: string); override;
    procedure Clear; override;  
  end;

  TSilUrlLogin = class(
    TSilUrlData,
    IUrlLogin )
  private
    FUser: string;
    FPassword: string;
  protected // IUrlLogin
    function GetUser: string;
    procedure SetUser(const Value: string);
    function GetPassword: string;
    procedure SetPassword(const Value: string);
  protected // IUrlData 
    function GetIsAssigned: Boolean; override;
    function GetText: string; override;
    procedure SetText(const Value: string); override;
    procedure Clear; override;  
  end;

  TSilUrlPath = class(
    TSilUrlData,
    IUrlPath )
  private
    FItems: IStringList;
    FAbsolute: Boolean;
  private
    function DoCheckItems: Boolean;
  protected // IUrlPath
    function GetCount: Integer;
    function GetIsRelative: Boolean;
    function GetIsAbsolute: Boolean;
    function GetFirst: string;
    function GetLast: string;
    function GetItem(Index: Integer): string;
    procedure SetItem(Index: Integer; const Value: string);
    function Enumerate(var Enum: IEnumerator; out Item: string): Boolean;
    function Add(const Item: string; Index: Integer = -1): Integer;
    function Remove(const Item: string): Integer;
    procedure Delete(Index: Integer);
  protected // IUrlData
    function GetIsAssigned: Boolean; override;
    function GetText: string; override;
    procedure SetText(const Value: string); override;
    procedure Clear; override;  
  end;

  TSilUrlItems = class(
    TSilUrlData,
    IUrlItems )
  private
    FList: IParameterList;
  private
    function DoCheckList: Boolean;
    function DoBuild(const Sep: string): string;
    procedure DoParse(const Text: string; const Sep: string);
  protected // IUrlItems
    function GetList: IParameterList;
    procedure SetList(const Value: IParameterList);
  protected // IUrlData
    function GetIsAssigned: Boolean; override;
    procedure Clear; override;  
  end;

  TSilUrlParams = class(
    TSilUrlItems,
    IUrlParams )
  protected // IUrlParams
  protected // IUrlData
    function GetText: string; override;
    procedure SetText(const Value: string); override;
  end;

  TSilUrlQuery = class(
    TSilUrlItems,
    IUrlQuery )
  protected // IUrlQuery
  protected // IUrlData
    function GetText: string; override;
    procedure SetText(const Value: string); override;
  end;

  // Url de una tabla o base de datos
  TUrlDb = class(
    TSilUrl,
    IUrlDB )
  protected // IUrlDB
    function GetDatabase: string;
    function GetTable: string;
    procedure SetDatabase(const Value: string);
    procedure SetTable(const Value: string);
  protected // properties
    property Database: string read GetDatabase write SetDatabase;
    property Table: string read GetTable write SetTable;
  end;

implementation

uses
  SilSmMimeCoder, SilLiStringList;


function UrlDecode(Value: string): string; forward;
function UrlEncode(const Value: string): string; forward;

{ TSilUrlData }

constructor TSilUrlData.Create(const Text: string);
begin
  inherited Create;
  if Str.IsAssigned(Text) then
    SetText(Text);
end;

destructor TSilUrlData.Destroy;
begin
  inherited;
end;

function TSilUrlData.GetIsAssigned: Boolean;
begin
  Result := Str.IsAssigned(Self.GetText());
end;

function TSilUrlData.GetIsEmpty: Boolean;
begin
  Result := not GetIsAssigned;
end;

procedure TSilUrlData.Clear;
begin
  SetText('');
end;

{ TSilUrl }

constructor TSilUrl.Create(const Text: string);
begin
  inherited Create('');
  FProtocol := TSilUrlProtocol.Create();
  FAddress := TSilUrlAddress.Create();
  FPath := TSilUrlPath.Create();
  FParams := TSilUrlParams.Create();
  FQuery := TSilUrlQuery.Create();
  SetText(Text);
end;

destructor TSilUrl.Destroy;
begin
  FQuery := nil;
  FParams := nil;
  FPath := nil;
  FAddress := nil;
  FProtocol := nil;
  inherited;
end;

procedure TSilUrl.DoChanged;
begin
end;

function TSilUrl.GetText: string;
begin
  Result := '';
  if FProtocol.IsAssigned then  Str.Add(Result, FProtocol.Text + ':');
  if FAddress.IsAssigned  then  Str.Add(Result, '//' + FAddress.Text);
  if FPath.IsAssigned     then  Str.Add(Result, '/'  + FPath.Text);
  if FParams.IsAssigned   then  Str.Add(Result, ';'  + FParams.Text);
  if FQuery.IsAssigned    then  Str.Add(Result, '?'  + FQuery.Text);
end;

procedure TSilUrl.SetText(const Value: string);
begin
  DoParseText(Value);
  DoChanged;
end;

function TSilUrl.GetAddress: IUrlAddress;
begin
  Result := FAddress;
end;

function TSilUrl.GetParams: IUrlParams;
begin
  Result := FParams;
end;

function TSilUrl.GetPath: IUrlPath;
begin
  Result := FPath;
end;

function TSilUrl.GetProtocol: IUrlProtocol;
begin
  Result := FProtocol;
end;

function TSilUrl.GetQuery: IUrlQuery;
begin
  Result := FQuery;
end;

function TSilUrl.GetIsAssigned: Boolean;
begin
  Result := FProtocol.IsAssigned
        or  FAddress.IsAssigned
        or  FPath.IsAssigned
        or  FParams.IsAssigned
        or  FQuery.IsAssigned;
end;

procedure TSilUrl.Clear;
begin
  FProtocol.Clear;
  FAddress.Clear;
  FPath.Clear;
  FParams.Clear;
  FQuery.Clear;
end;

procedure TSilUrl.DoParseText(const Value: string);
var
  Buffer: string;
begin
  Buffer := UrlDecode(Value);
  FProtocol.Text := DoParseProtocol(Buffer);
  FAddress.Text := DoParseAddress(Buffer);
  FPath.Text := DoParsePath(Buffer);
  FParams.Text := DoParseParams(Buffer);
  FQuery.Text := DoParseQuery(Buffer);
end;

function TSilUrl.DoParseProtocol(var Buffer: string): string;
var
  Temp: string;
begin
  if Str.Pos('://', Buffer) > 0 then
    if Str.Split(Buffer, ':', Result, Temp, False) then
      Buffer := Temp;
end;

function TSilUrl.DoParseAddress(var Buffer: string): string;
var
  Last: Integer;
  Temp: string;
begin
  if Str.Between(Buffer, '//', '/', Result, 1, @Last) then
    Buffer := Str.Copy(Buffer, Last)
  else if Str.Split(Buffer, '/', Result, Temp) then
    Buffer := Temp
  else
    Buffer := '';
end;

function TSilUrl.DoParsePath(var Buffer: string): string;
var
  Temp: string;
begin
  if Str.Pos(';', Buffer) > 0 then
    begin
      Str.Split(Buffer, ';', Result, Temp, True);
      Buffer := ';' + Temp;
    end
  else if Str.Pos('?', Buffer) > 0 then
    begin
      Str.Split(Buffer, '?', Result, Temp, True);
      Buffer := '?' + Temp;
    end
  else
    begin
      Result := Buffer;
      Buffer := '';
    end;
end;

function TSilUrl.DoParseParams(var Buffer: string): string;
var
  Temp: string;
begin
  if Str.FirstChar(Buffer) = ';' then
    if Str.Split(Str.Copy(Buffer, 2), '?', Result, Temp, True) then
      Buffer := '?' + Temp else
      Buffer := '';
end;

function TSilUrl.DoParseQuery(var Buffer: string): string;
begin
  if Str.FirstChar(Buffer) = '?' then
    Result := Str.Copy(Buffer, 2) else
    Result := '';
end;

{ TSilUrlProtocol }

function TSilUrlProtocol.GetIsAssigned: Boolean;
begin
  Result := Str.IsAssigned(FProtocol);
end;

function TSilUrlProtocol.GetText: string;
begin
  Result := FProtocol;
end;

procedure TSilUrlProtocol.SetText(const Value: string);
begin
  FProtocol := Value;
end;

{ TSilUrlAddress }

constructor TSilUrlAddress.Create(const Text: string);
begin
  inherited;
  FLogin := TSilUrlLogin.Create();
  FHost := TSilUrlHost.Create();
end;

function TSilUrlAddress.GetLogin: IUrlLogin;
begin
  Result := FLogin;
end;

function TSilUrlAddress.GetHost: IUrlHost;
begin
  Result := FHost;
end;

function TSilUrlAddress.GetIsAssigned: Boolean;
begin
  Result := FLogin.IsAssigned
        or  FHost.IsAssigned;
end;

function TSilUrlAddress.GetText: string;
begin
  Result := '';
  if FLogin.IsAssigned  then  Str.Add(Result, FLogin.Text);
  if FHost.IsAssigned   then  Str.Add(Result, FHost.Text, '@');
end;

procedure TSilUrlAddress.SetText(const Value: string);
var
  Login, Host: string;
begin
  if Str.IsAssigned(Value) then
  begin
    Str.Split(Value, '@', Login, Host, False);
    FLogin.Text := Login;
    FHost.Text := Host; 
  end else
    Clear;
end;

procedure TSilUrlAddress.Clear;
begin
  FLogin.Clear;
  FHost.Clear;
end;

{ TSilUrlHost }

function TSilUrlHost.GetIsAssigned: Boolean;
begin
  Result := Str.IsAssigned(FServer)
        or  Str.IsAssigned(FPort);
end;

function TSilUrlHost.GetPort: string;
begin
  Result := FPort;
end;

function TSilUrlHost.GetServer: string;
begin
  Result := FServer;
end;

procedure TSilUrlHost.SetPort(Value: string);
begin
  FPort := Value;
end;

procedure TSilUrlHost.SetServer(const Value: string);
begin
  FServer := Value;
end;

function TSilUrlHost.GetText: string;
begin
  Result := '';
  if Str.IsAssigned(FServer)  then  Str.Add(Result, FServer);
  if Str.IsAssigned(FPort)    then  Str.Add(Result, FPort, ':');
end;

procedure TSilUrlHost.SetText(const Value: string);
begin
  Clear;
  if Str.IsAssigned(Value) then 
    Str.Split(Value, ':', FServer, FPort, True);
end;

procedure TSilUrlHost.Clear;
begin
  FServer := '';
  FPort := '';
end;

{ TSilUrlLogin }

procedure TSilUrlLogin.Clear;
begin
  FUser := '';
  FPassword := '';
end;

function TSilUrlLogin.GetIsAssigned: Boolean;
begin
  Result := Str.IsAssigned(FUser)
        or  Str.IsAssigned(FPassword);
end;

function TSilUrlLogin.GetPassword: string;
begin
  Result := FPassword;
end;

function TSilUrlLogin.GetUser: string;
begin
  Result := FUser;
end;

procedure TSilUrlLogin.SetPassword(const Value: string);
begin
  FPassword := Value;
end;

procedure TSilUrlLogin.SetUser(const Value: string);
begin
  FUser := Value;
end;

function TSilUrlLogin.GetText: string;
begin
  Result := '';
  if Str.IsAssigned(FUser)      then  Str.Add(Result, FUser);
  if Str.IsAssigned(FPassword)  then  Str.Add(Result, FPassword, ':');
end;

procedure TSilUrlLogin.SetText(const Value: string);
begin
  Clear;
  if Str.IsAssigned(Value) then 
    Str.Split(Value, ':', FUser, FPassword, True);
end;

{ TSilUrlPath }

function TSilUrlPath.Enumerate(var Enum: IEnumerator; out Item: string): Boolean;
begin
  DoCheckItems;
  Result := FItems.Enumerate(Enum, Item);
end;

function TSilUrlPath.GetText: string;
var
  Enum: IEnumerator;
  Item: string;
begin
  Result := '';
  if Assigned(FItems) then
    with FItems do
      while Enumerate(Enum, Item) do
        Str.Add(Result, Item, '/');
  if FAbsolute then Result := '/' + Result;
end;

procedure TSilUrlPath.SetText(const Value: string);
var
  I: Integer;
  Buffer: string;
  Item: string;
begin
  Buffer := Str.Replace(Value, '\', '/');
  FAbsolute := Str.FirstChar(Buffer) = '/';
  if FAbsolute then Str.Delete(Buffer, 1, 1);
  if DoCheckItems then FItems.Clear;
  I := 0;
  while Str.Enumerate(Buffer, '/', Item, I) do
    FItems.Add(Item);
end;

function TSilUrlPath.GetIsAssigned: Boolean;
begin
  Result := Assigned(FItems)
        and (FItems.Count > 0);
end;

function TSilUrlPath.GetIsAbsolute: Boolean;
begin
  Result := FAbsolute;
end;

function TSilUrlPath.GetCount: Integer;
begin
  if Assigned(FItems) then
    Result := FItems.Count else
    Result := 0;
end;

function TSilUrlPath.GetIsRelative: Boolean;
begin
  Result := not FAbsolute;
end;

function TSilUrlPath.GetFirst: string;
begin
  if Assigned(FItems) then
    Result := FItems.First else
    Result := '';
end;

function TSilUrlPath.GetItem(Index: Integer): string;
begin
  if Assigned(FItems) then
    Result := FItems.Items[Index] else
    Result := '';
end;

function TSilUrlPath.GetLast: string;
begin
  if Assigned(FItems) then
    Result := FItems.Last else
    Result := '';
end;

procedure TSilUrlPath.SetItem(Index: Integer; const Value: string);
begin
  DoCheckItems;
  FItems.Items[Index] := Value;
end;

procedure TSilUrlPath.Clear;
begin
  if Assigned(FItems) then
    FItems.Clear;
end;

function TSilUrlPath.Add(const Item: string; Index: Integer): Integer;
begin
  Result := Index;
  if Assigned(FItems) then
    if Index = -1 then
      Result := FItems.Add(Item) else
      FItems.Insert(Index, Item);
end;

procedure TSilUrlPath.Delete(Index: Integer);
begin
  if Assigned(FItems) then
    FItems.Delete(Index);
end;

function TSilUrlPath.Remove(const Item: string): Integer;
begin
  Result := -1;
  if Assigned(FItems) then
    Result := FItems.IndexOf(Item);
  if Result <> -1 then
    FItems.Delete(Result);
end;

function TSilUrlPath.DoCheckItems: Boolean;
begin
  Result := Assigned(FItems);
  if not Result then
    FItems := Sil.List.StringList();
end;

{ TSilUrlItems }

function TSilUrlItems.GetList: IParameterList;
begin
  DoCheckList;
  Result := FList;
end;

procedure TSilUrlItems.SetList(const Value: IParameterList);
begin
  FList := Value;
end;

function TSilUrlItems.GetIsAssigned: Boolean;
begin
  Result := Assigned(FList) and (FList.Count > 0);
end;

procedure TSilUrlItems.Clear;
begin
  if Assigned(FList) then
    FList.Clear;
end;

function TSilUrlItems.DoBuild(const Sep: string): string;
var
  Enum: IEnumerator;
  Item: RParameter;
begin
  Result := '';
  if Assigned(FList) then
    with FList do
      while Enumerate(Enum, Item) do
        Str.Add(Result, Item.Name + '=' + Vart.ToStr(Item.Value), Sep);
end;

procedure TSilUrlItems.DoParse(const Text: string; const Sep: string);
var
  I: Integer;
  Item: string;
  Name: string;
  Value: string;
begin
  if Str.IsAssigned(Text) then
  begin
    DoCheckList;
    I := 0;
    while Str.Enumerate(Text, Sep, Item, I) do
      if Str.IsAssigned(Item) then
      begin
        if not Str.Split(Item, '=', Name, Value) then
          Value := '';
        FList[Name] := Value;
      end;
  end else
    Clear;
end;

function TSilUrlItems.DoCheckList: Boolean;
begin
  Result := Assigned(FList);
  if not Result then
    FList := Sil.List.Parameters();
end;

{ TSilUrlParams }

function TSilUrlParams.GetText: string;
begin
  Result := DoBuild(';');
end;

procedure TSilUrlParams.SetText(const Value: string);
begin
  DoParse(Value, ';');
end;

{ TSilUrlQuery }

function TSilUrlQuery.GetText: string;
begin
  Result := DoBuild('&');
end;

procedure TSilUrlQuery.SetText(const Value: string);
begin
  DoParse(Value, '&');
end;

{ TUrlDb }

function TUrlDb.GetDatabase: string;
begin
  Result := FQuery.List['database'];
end;

function TUrlDb.GetTable: string;
begin
  Result := FQuery.List['table'];
end;

procedure TUrlDb.SetDatabase(const Value: string);
begin
  FQuery.List['Database'] := Value;
end;

procedure TUrlDb.SetTable(const Value: string);
begin
  FQuery.List['Table'] := Value;
end;

{ globales }

function UrlDecode(Value: string): string;
begin
  if Str.Pos('%', Value) > 0 then
  begin
    Str.Replace(Value, '%', '=');
    Result := TISO8859_1Coder.DecodeLine(Value);
  end else
    Result := Value;
end;

function UrlEncode(const Value: string): string;
begin
  Result := TISO8859_1Coder.EncodeLine(Value);
  if Str.Pos('=', Result) > 0 then
    Str.Replace(Result, '=', '%');
end;

end.

