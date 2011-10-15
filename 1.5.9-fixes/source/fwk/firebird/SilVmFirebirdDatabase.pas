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

unit SilVmFirebirdDatabase;

{$I Defines.inc}

interface

uses
  Classes, Db,

  Sil,
  SilSiFirebird,
  SilStFirebird,

  SilVmFirebirdDatabaseParams;

type
  TSilFbDatabase = class(TCustomConnection)
  private
    FParams: TSilFbDatabaseParams;
    FSession: IFbSession;
    FServerName: String;
    FDatabaseName: String;
  private
    procedure SetDatabaseName(const Value: String);
    function GetDatabaseName: String;
    function GetServerName: String;
    procedure SetServerName(const Value: String);
  protected
    procedure DoConnect; override;
    procedure DoDisconnect; override;
    function GetConnected: Boolean; override;
    procedure Loaded; override;
    procedure SetConnected(Value: Boolean); override;
  published
    property ServerName: String read GetServerName write SetServerName;
    property DatabaseName: String read GetDatabaseName write SetDatabaseName;
    property Params: TSilFbDatabaseParams read FParams;
  published
    property Connected;
    property AfterConnect;
    property BeforeConnect;
    property AfterDisconnect;
    property BeforeDisconnect;
  public
    procedure AddObject(Sender: TObject; const Event: TConnectChangeEvent = nil);
    procedure RemoveObject(Sender: TObject);
    property Session: IFbSession read FSession write FSession;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

uses
  SilAfLockedIncrement;

var
  MRefCount: Integer = 0;
  MService: IFbApplication;

procedure ServiceAddRef;
begin
  if LockedInc(MRefCount) = 1 then
    MService := FireBird.Application;
end;

procedure ServiceRelease;
begin
  if LockedDec(MRefCount) = 0 then
    MService := nil;
end;

{ TSilFbDatabase }

procedure TSilFbDatabase.AddObject(Sender: TObject; const Event: TConnectChangeEvent);
begin
  RegisterClient(Sender, Event);
end;

procedure TSilFbDatabase.RemoveObject(Sender: TObject);
begin
  UnRegisterClient(Sender);
end;

constructor TSilFbDatabase.Create(AOwner: TComponent);
begin
  inherited;

  ServiceAddRef;
  FParams := TSilFbDatabaseParams.Create;
end;

destructor TSilFbDatabase.Destroy;
begin
  inherited;

  FParams.Free;
  ServiceRelease;
end;

procedure TSilFbDatabase.DoConnect;
begin
  inherited;
  FSession := MService.Connect(FServerName, FDatabaseName, FParams.Parameters);
end;

procedure TSilFbDatabase.DoDisconnect;
begin
  FSession := nil;
  inherited;
end;

function TSilFbDatabase.GetConnected: Boolean;
begin
  Result := Assigned(FSession);
end;

function TSilFbDatabase.GetDatabaseName: String;
begin
  Result := FDatabaseName;
end;

function TSilFbDatabase.GetServerName: String;
begin
  Result := FServerName;
end;

procedure TSilFbDatabase.Loaded;
begin
  inherited;
end;

procedure TSilFbDatabase.SetConnected(Value: Boolean);
begin
  if Value = GetConnected then Exit;

  if Value then
  begin
    if Assigned(BeforeConnect) then BeforeConnect(Self);
    DoConnect;
    SendConnectEvent(True);
    if Assigned(AfterConnect) then AfterConnect(Self);
  end else
  begin
    if Assigned(BeforeDisconnect) then BeforeDisconnect(Self);
    SendConnectEvent(False);
    DoDisconnect;
    if Assigned(AfterDisconnect) then AfterDisconnect(Self);
  end;
end;

procedure TSilFbDatabase.SetDatabaseName(const Value: String);
begin
  FDatabaseName := Value;
end;

procedure TSilFbDatabase.SetServerName(const Value: String);
begin
  FServerName := Value;
end;

end.
