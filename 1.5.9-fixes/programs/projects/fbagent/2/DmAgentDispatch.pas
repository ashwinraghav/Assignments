{********************************************************************************
 *                  Standard Interface Library (SIL)                            *
 *                                                                              *
 *       General purpose library whose design is based in STRONG                *
 *   use of interfaces.                                                         *
 *                                                                              *
 *                                                                              *
 *     Copyright (C) 2000 Mariano Podestá    marianop@intercom.com.ar           *
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

unit DmAgentDispatch;

interface

uses
  SysUtils, Classes, IBSQL, IBDatabase, DB, IBServices,

  Sil;

type
  TdaDispatch = class(TDataModule)
    coDispatch: TIBDatabase;
    trDispatchStat: TIBTransaction;
    sqAction: TIBSQL;
    bsBackup: TIBBackupService;
  private
    FFileName: String;
    FFileCount: Integer;
  public
    procedure SetBackupServer(const Params: IParameters);
    procedure DeleteOldFiles;
  end;

var
  daDispatch: TdaDispatch;

implementation

uses SilOtFile;

{$R *.dfm}

{ TdaDispatch }

procedure TdaDispatch.SetBackupServer(const Params: IParameters);
var
  Server, Database: String;
begin
  Str.Split(Vart.ToStr(Params['connection']), ':', Server, Database, false);

  FFileCount := Vart.ToInt(Params['backupfilecount']);
  FFileName := Sil.OS.FileSystem.LogCreate(Vart.ToStr(Params['backupfile']), FFileCount + 1);

  bsBackup.BackupFile.Add(FFileName);
  bsBackup.DatabaseName := Database;

  if Str.NotEmpty(Server) then
  begin
    bsBackup.ServerName := Server;
    bsBackup.Protocol := TCP;
  end;

  if Vart.ToBool(Params['ignorebadchecksums']) then
    bsBackup.Options := bsBackup.Options + [IgnoreChecksums];

  if Vart.ToBool(Params['ignorelimbotransactions']) then
    bsBackup.Options := bsBackup.Options + [IgnoreLimbo];

  if not Vart.ToBool(Params['garbagecollection']) then
    bsBackup.Options := bsBackup.Options + [NoGarbageCollection];

  if not Vart.ToBool(Params['externalfilesastables']) then
    bsBackup.Options := bsBackup.Options + [NonTransportable];

  if Vart.ToBool(Params['transportable']) then
    bsBackup.Options := bsBackup.Options + [ConvertExtTables];

  bsBackup.Params.Values['user_name'] := Vart.ToStr(Params['user']);
  bsBackup.Params.Values['password'] := Vart.ToStr(Params['password']);
end;

procedure TdaDispatch.DeleteOldFiles;
begin
  Sil.Trace.Enter(Self, 'DeleteOldFiles', [FFileName, FFileCount]);
  Sil.OS.FileSystem.LogLimitCount(FFileName, FFileCount);
  Sil.Trace.Leave;
end;

end.
