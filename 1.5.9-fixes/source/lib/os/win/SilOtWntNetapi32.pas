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

unit SilOtWntNetapi32;

{$I Defines.inc}

interface

uses
  SilBkTool,
  SilOsTypes,
  SilOhWntNetapi32,
  SilOiWntNetapi32;

type
  Netapi = class (Tool)
    class function Create: INetapi32;

    class function UserGetInfo(
          const DLL: INetapi32;
          const ServerName: WideString;
          const UserName: WideString;
                Level: LongWord
         ): Pointer;

    class function UserEnum(
          const DLL: INetapi32;
          const ServerName: WideString;
                Level: LongWord;
                Filter: LongWord;
                PrefMaxLen: LongWord;
            out EntriesRead: LongWord;
            out TotalEntries: LongWord;
            out ResumeHandle: LongWord;
            out Info
         ): Boolean;

    class procedure UserChangePassword(
          const DLL: INetapi32;
          const DomainName: WideString;
          const UserName: WideString;
          const OldPassword: WideString;
          const NewPassword: WideString
          );

    class function ServerGetInfo(
          const DLL: INetapi32;
          const ServerName: WideString;
                Level: LongWord
          ): Pointer;

    class function QueryIndex(
          const DLL: INetapi32;
          const ServerName: WideString;
          const Prefix: WideString;
                Level: TNetQueryLevel
          ): Integer;

    class function QueryInfo(
          const DLL: INetapi32;
          const ServerName: WideString;
               Index: Integer;
               Level: TNetQueryLevel;
           out Returned: Integer;
               Requested: Integer = 1;
               MaxLength: Integer = -1
         ): Pointer;

    class function ServerEnum(
          const DLL: INetapi32;
          const ServerName: WideString;
          const DomainName: WideString;
                Level: LongWord;
                ServerType: LongWord;
                PrefMaxLen: LongWord;
            out EntriesRead: LongWord;
            out TotalEntries: LongWord;
            out ResumeHandle: LongWord;
            out Info
          ): Boolean;

    class function GetJoinInformation(
          const DLL: INetapi32;
          const ServerName: WideString;
            out DomainName: WideString
          ): TNetJoinStatus;

    class function GetDomainController(
          const DLL: INetapi32;
          const ServerName: WideString;
          const DomainName: WideString;
                Flags: TNetFindDomainControllerFlags = [NET_DS_RETURN_FLAT_NAME]
          ): PNetDomainControllerInfo;

    class procedure UseAdd(
          const DLL: INetapi32;
          const LocalPath: WideString;
          const RemotePath: WideString;
          const DomainName: WideString;
          const UserName: WideString;
          const Password: WideString;
                Kind: TNetUseKindEnum = NET_USE_DISKDEV;
                ParmError: PNetUseParamError = nil
          );

    class procedure UseDel(
          const DLL: INetapi32;
          const UseName: WideString;
                Force: TNetUseForceKind = NET_USE_LOTS_OF_FORCE);

    class function Netbios(
          const DLL: INetapi32;
          const NCB: TNCB
          ): TNetapiStatus;

    class function NbAdapterStatus(
          const DLL: INetapi32;
                Lana: Byte;
            out Status: TNetAdapterStatus
          ): TNetapiStatus;

    class function NbLanaEnum(
          const DLL: INetapi32;
            out Lana: TNetAdapters
          ): TNetapiStatus;

    class function NbReset(
          const DLL: INetapi32;
                Lana: Byte
          ): TNetapiStatus;

    class procedure Free(
          const DLL: INetapi32;
            var Buffer); reintroduce;

    class function MessageSend(
          const DLL: INetapi32;
          const ServerName: WideString;
          const MsgName: WideString;
          const FromName: WideString;
                Buf: Pointer;
                BufLen: LongWord
          ): TNetapiStatus;

    class function UserModalsGet(
          const DLL: INetapi32;
          const ServerName: WideString;
          Level: LongWord): Pointer;

  end;

implementation

uses
  Windows,
  SilBtTypeinfo,
  SilOsError,
  SilOfWntNetapi32,
  SilOmWntNetapi32;

{ Netapi }

class function Netapi.Create: INetapi32;
begin
  Result := TNetapi32.Create(CNetapi32);
end;

class procedure Netapi.Free(const DLL: INetapi32; var Buffer);
var
  Ptr: Pointer;
begin
  if Pointer(Buffer) <> nil then
  begin
    Ptr := Pointer(Buffer);
    Pointer(Buffer) := nil;
    DLL.NetApiBufferFree(Ptr);
  end;
end;

class function Netapi.GetJoinInformation(
  const DLL: INetapi32;
  const ServerName: WideString;
    out DomainName: WideString): TNetJoinStatus;
var
  Domain, Server: PWideChar;
begin
  if Length(ServerName) <> 0 then
    Server := PWideChar(ServerName) else
    Server := nil;
    
  OsError.Check(
    DLL.NetGetJoinInformation(
      Server,
      Domain,
      Result ),
    'Netapi.GetJoinInformation [netapi32.NetGetJoinInformation]');
  try
    DomainName := Domain;
  finally
    Free(Dll, Domain);
  end;
end;

class function Netapi.ServerGetInfo(const DLL: INetapi32; const ServerName: WideString; Level: LongWord): Pointer;
begin
  OsError.Check(
    DLL.NetServerGetInfo(
      PWideChar(ServerName),
      Level,
      Result),
  'Netapi.ServerGetInfo [netapi32.NetServerGetInfo]');
end;

class function Netapi.ServerEnum(
  const DLL: INetapi32;
  const ServerName, DomainName: WideString;
        Level, ServerType, PrefMaxLen: LongWord;
  out   EntriesRead, TotalEntries, ResumeHandle: LongWord;
  out   Info): Boolean;
var
  State: OsWord;
  Resume: LongWord;
  Domain, Server: PWideChar;
begin
  Resume := 0;
  
  if Length(ServerName) <> 0 then
    Server := PWideChar(ServerName) else
    Server := nil;
  if Length(DomainName) <> 0 then
    Domain := PWideChar(DomainName) else
    Domain := nil;

  State := DLL.NetServerEnum(
               Server,
               Level,
               Pointer(Info),
               PrefMaxLen,
               @EntriesRead,
               @TotalEntries,
               ServerType,
               Domain,
               @Resume);

  Result := (State = ERROR_MORE_DATA);
  
  if not Result then
    raise OsError.Create(State, 'Netapi.ServerEnum [netapi32.NetServerEnum]');
end;

class function Netapi.UserEnum(
  const DLL: INetapi32;
  const ServerName: WideString;
        Level, Filter, PrefMaxLen: LongWord;
  out   EntriesRead, TotalEntries, ResumeHandle: LongWord;
  out   Info): Boolean;
var
  State: OsWord;
  Server: PWideChar;
begin
  if Length(ServerName) <> 0 then
    Server := PWideChar(ServerName) else
    Server := nil;

  State := DLL.NetUserEnum(
               Server,
               Level,
               Filter,
               Pointer(Info),
               PrefMaxLen,
               @EntriesRead,
               @TotalEntries,
               @ResumeHandle);

  Result := (State = ERROR_MORE_DATA);
  if not Result then
    raise OsError.Create(State, 'Netapi.ServerEnum [netapi32.NetServerEnum]');
end;

class procedure Netapi.UserChangePassword(
  const DLL: INetapi32;
  const DomainName, UserName, OldPassword, NewPassword: WideString);
begin
  OsError.Check(
    DLL.NetUserChangePassword(
      PWideChar(DomainName),
      PWideChar(UserName),
      PWideChar(OldPassword),
      PWideChar(NewPassword)),
  'Netapi.UserChangePassword [netapi32.NetUserChangePassword]');
end;

class function Netapi.UserGetInfo(
  const DLL: INetapi32;
  const ServerName, UserName: WideString;
        Level: LongWord): Pointer;
begin
  OsError.Check(
    DLL.NetUserGetInfo(
      PWideChar(ServerName),
      PWideChar(UserName),
      Level,
      Result),
  'Netapi.UserGetInfo [netapi32.NetUserGetInfo]');
end;

class function Netapi.QueryIndex(
  const DLL: INetapi32;
  const ServerName, Prefix: WideString;
  Level: TNetQueryLevel): Integer;
begin
  OsError.Check(
    DLL.NetGetDisplayInformationIndex(
      PWideChar(ServerName),
      Ord(Level) + 1,
      PWideChar(Prefix),
      @Result),
  'Netapi.QueryIndex [netapi32.NetGetDisplayInformationIndex]');
end;

class function Netapi.QueryInfo(
  const DLL: INetapi32;
  const ServerName: WideString;
        Index: Integer;
        Level: TNetQueryLevel;
    out Returned: Integer;
        Requested, MaxLength: Integer): Pointer;
begin
  OsError.Check(
    DLL.NetQueryDisplayInformation(
      PWideChar(ServerName),
      Ord(Level) + 1,
      Index,
      Requested,
      MaxLength,
      @Returned,
       Result),
  'Netapi.QueryInfo [netapi32.NetQueryDisplayInformation]');
end;


class function Netapi.GetDomainController(
  const DLL: INetapi32;
  const ServerName, DomainName: WideString;
        Flags: TNetFindDomainControllerFlags): PNetDomainControllerInfo;
begin                                                             
  OsError.Check(
    DLL.DsGetDCName(
      PWideChar(ServerName),
      PWideChar(DomainName),
      nil,
      nil,
      Flags,
      Result),
  'Netapi.GetDomainController [netapi32.NetGetDCName]');
end;

class procedure Netapi.UseAdd(
  const DLL: INetapi32;
  const LocalPath, RemotePath, DomainName, UserName, Password: WideString;
        Kind: TNetUseKindEnum;
        ParmError: PNetUseParamError);
var
  State: TNetapiStatus;
  Info: TNetUseInfo2;
  Error: TNetUseParamError;
begin
  Info.ui2_local := PWideChar(LocalPath);
  Info.ui2_remote := PWideChar(RemotePath);
  Info.ui2_password := PWideChar(Password);
  Info.ui2_username := PWideChar(UserName);
  Info.ui2_domainname := PWideChar(DomainName);
  Info.ui2_asg_type := Kind;
  Info.ui2_status := NET_USE_OK;
  Info.ui2_refcount := 0;
  Info.ui2_usecount := 1;

  State := DLL.NetUseAdd(nil, 2, @Info, @Error);

  if State <> ERROR_SUCCESS then
    case State of
      ERROR_INVALID_PARAMETER:
        raise OsError.Create(
            'Netapi.UseAdd [netapi32.NetUseAdd] - %s: %s',
            [ OsError.ErrorMessage(State), Typ.Enum.Name(TypeInfo(TNetUseParamError), Ord(Error), 'NET_USE_')]);
      else
        OsError.Check(State, 'Netapi.UseAdd [netapi32.NetUseAdd]');
    end;
end;

class procedure Netapi.UseDel(
  const DLL: INetapi32;
  const UseName: WideString;
        Force: TNetUseForceKind);
begin
  OsError.Check(DLL.NetUseDel(nil, PWideChar(UseName), Force), 'Netapi.UseDel [netapi32.NetUseDel]');
end;

class function Netapi.Netbios(const DLL: INetapi32; const NCB: TNCB): TNetapiStatus;
begin
  Result := DLL.Netbios(NCB);
  if Result <> NRC_GOODRET then Inc(Result, NERR_BASE);
end;

class function Netapi.NbAdapterStatus(const DLL: INetapi32; Lana: Byte; out Status: TNetAdapterStatus): TNetapiStatus;
var
  NCB: TNCB;
begin
  FillChar(NCB, SizeOf(NCB), 0);
  FillChar(Status, SizeOf(Status), 0);
  NCB.ncb_command := NCBASTAT;
  NCB.ncb_buffer := @Status;
  NCB.ncb_length := Sizeof(Status);
  NCB.ncb_callname[0] := '*';
  NCB.ncb_lana_num := Lana;
  Result := Netbios(DLL, NCB);
end;

class function Netapi.NbLanaEnum(const DLL: INetapi32; out Lana: TNetAdapters): TNetapiStatus;
var
  NCB: TNCB;
  Enum: TNetLanaEnum;
  I: Integer;
begin
  FillChar(NCB, SizeOf(NCB), 0);
  FillChar(Enum, SizeOf(Enum), 0);
  NCB.ncb_command := NCBENUM;
  NCB.ncb_buffer := @Enum;
  NCB.ncb_length := Sizeof(Enum);
  Result := Netbios(DLL, NCB);
  if (Result = NRC_GOODRET) then
  begin
    SetLength(Lana, Ord(Enum.length));
    for I := 0 to Ord(Enum.length) - 1 do
      Lana[I] := Ord(Enum.lana[I]);
  end;
end;

class function Netapi.NbReset(const DLL: INetapi32; Lana: Byte): TNetapiStatus;
var
  NCB: TNCB;
begin
  FillChar(NCB, SizeOf(NCB), 0);
  NCB.ncb_command := NCBRESET;
  NCB.ncb_lana_Num := Lana;
  Result := Netbios(DLL, NCB);
end;

class function Netapi.MessageSend(
  const DLL: INetapi32;
  const ServerName, MsgName, FromName: WideString;
        Buf: Pointer;
        BufLen: LongWord): TNetapiStatus;
begin
  Result :=
    DLL.NetMessageBufferSend(
      PWideChar(ServerName),
      PWideChar(MsgName),
      PWideChar(FromName),
      Buf,
      BufLen);

  if Result <> ERROR_SUCCESS then
    case Result of
      ERROR_INVALID_PARAMETER:
        raise OsError.Create(
            'Netapi.MessageSend [netapi32.NetMessageBufferSend] - %s: %s',
            [ OsError.ErrorMessage(Result), Typ.Enum.Name(TypeInfo(TNetUseParamError), Ord(Error), 'NET_USE_')]);
      else
        OsError.Check(Result, 'Netapi.MessageSend [netapi32.NetMessageBufferSend]');
    end;
end;

class function Netapi.UserModalsGet(const DLL: INetapi32;
  const ServerName: WideString; Level: LongWord): Pointer;
begin
  OsError.Check(
    DLL.NetUserModalsGet(
      PWideChar(ServerName),
      Level,
      Result),
  'Netapi.UserModalsGet [netapi32.NetUserModalsGet]');
end;

end.
