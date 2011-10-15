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

unit SilSmFirebirdTransaction;

{$INCLUDE Defines.inc}

interface

uses
  Sil,  
  SilSeFirebirdClient,
  SilSiFirebird,
  SilShFirebird,
  SilSkFirebirdHandled;

type
  TSilFirebirdTransaction = class(
    TSilFirebirdHandled,
    IFbTransactionInternal )
  private
    FList: Pointer;
    FDefaults: RFbTransactionDefaults;
    FID: Integer;
    FName: string;
    FFailed: Boolean;
  private
    function DoGetDefaults(const Defaults: IFbTransactionOptions): RFbTransactionDefaults;
    procedure DoStart;
    function DoBuildParams: String;
    procedure DoAdd(var Buffer: String; Value: PChar);
  protected // IFbTransaction
    function GetSession: IFbSession;
    function GetID: Integer;
    function GetName: String;
    function GetIsPending: Boolean;
    function GetIsTerminated: Boolean;
    function GetFailed: Boolean;
    procedure SetFailed(Value: Boolean);
    function GetRetaining: Boolean;
    procedure SetRetaining(Value: Boolean);
    function GetAutoCommit: Boolean;
    procedure SetAutoCommit(Value: Boolean);
    procedure Commit; overload;
    procedure Rollback; overload;
    procedure Commit(Retaining: Boolean); overload;
    procedure Rollback(Retaining: Boolean); overload;
  protected
    function DoGetList: IFbTransactionsInternal;
    function DoGetSession: IFbSessionInternal;
    function DoGetHandle: PISC_TR_HANDLE;
    procedure Detach;
  protected
    procedure DoCloseHandle(const Sender: IFbHandle); override;
  public
    constructor Create(const List: IFbTransactionsInternal; const Name: string; AutoCommit: Boolean);
    destructor Destroy; override;
  public
    property List: IFbTransactionsInternal read DoGetList; 
    property Session: IFbSessionInternal read DoGetSession;
    property Handle: PISC_TR_HANDLE read DoGetHandle;
  end;

implementation

uses
  SilScFirebirdClient,
  SilSfFirebirdClient,
  SilSfFirebird,
  SilSmFirebirdParameters,
  SilSgFirebirdTransaction, SilOtLocked;

var
  MTransactionID: Integer = 0;

{ TSilFirebirdTransaction }

constructor TSilFirebirdTransaction.Create(const List: IFbTransactionsInternal; const Name: string; AutoCommit: Boolean);
begin
  inherited Create;
  FList := Pointer(List);
  List.Add(Self);
  FID := Sil.Os.Locked.Increment(MTransactionID);
  if Sil.Str.IsEmpty(Name) then
    FName := 'TID=' + Sil.Int.ToHex(FID, 8) else
    FName := Name;
  FDefaults := DoGetDefaults(List.Defaults);
  FDefaults.AutoCommit := AutoCommit;
  DoStart();
end;

destructor TSilFirebirdTransaction.Destroy;
begin
  Detach;  
  inherited;
end;

function TSilFirebirdTransaction.GetSession: IFbSession;
begin
  Result := List.Session;
end;

function TSilFirebirdTransaction.GetID: Integer;
begin
  Result := FID;
end;

function TSilFirebirdTransaction.GetName: String;
begin
  Result := FName;
end;

function TSilFirebirdTransaction.GetIsPending: Boolean;
begin
  Result := Assigned(Handle^);
end;

function TSilFirebirdTransaction.GetIsTerminated: Boolean;
begin
  Result := not Assigned(Handle^);
end;

function TSilFirebirdTransaction.GetFailed: Boolean;
begin
  Result := FFailed;
end;

procedure TSilFirebirdTransaction.SetFailed(Value: Boolean);
begin
  FFailed := True;
end;

function TSilFirebirdTransaction.GetRetaining: Boolean;
begin
  Result := FDefaults.Retaining;
end;

procedure TSilFirebirdTransaction.SetRetaining(Value: Boolean);
begin
  FDefaults.Retaining := Value;
end;

function TSilFirebirdTransaction.GetAutoCommit: Boolean;
begin
  Result := FDefaults.AutoCommit;
end;

procedure TSilFirebirdTransaction.SetAutoCommit(Value: Boolean);
begin
  FDefaults.AutoCommit := Value;
end;

procedure TSilFirebirdTransaction.Commit;
begin
  Commit(FDefaults.Retaining);
end;

procedure TSilFirebirdTransaction.Rollback;
begin
  Rollback(FDefaults.Retaining);
end;

procedure TSilFirebirdTransaction.Commit(Retaining: Boolean);
begin
  try
    if not Retaining then
      Check(fb.api.tran.commit(fb.status, Handle)) else
      Check(fb.api.tran.commit_retaining(fb.status, Handle));
  finally
    if not Retaining then Detach;
  end;
end;

procedure TSilFirebirdTransaction.Rollback(Retaining: Boolean);
begin
  try
    if not Retaining then
      Check(fb.api.tran.rollback(fb.status, Handle)) else
      Check(fb.api.tran.rollback_retaining(fb.status, Handle));
  finally
    if not Retaining then Detach;
  end;
end;

procedure TSilFirebirdTransaction.DoCloseHandle(const Sender: IFbHandle);
begin
  if Sender.IsAssigned then
  begin
    if not FFailed then
      Commit(False) else
      Rollback(False);
  end;
end;

procedure TSilFirebirdTransaction.DoStart;
var
  TPB: string;
  TEB: TISC_TEB;
begin
  TPB := DoBuildParams;
  TEB.db_handle := Session.Database.Handle;
  TEB.tpb_length := Length(TPB);
  if TEB.tpb_length > 0 then
    TEB.tpb_address := PChar(TPB) else
    TEB.tpb_address := nil;
  Check(fb.api.tran.start_multiple(fb.status, Handle, 1, @TEB));
end;

function TSilFirebirdTransaction.DoBuildParams: String;
var
  Buffer: string;
begin
  DoAdd(Buffer, GFbTransactionModel[FDefaults.Model]);
  DoAdd(Buffer, GFbTransactionAccess[FDefaults.Access]);
  DoAdd(Buffer, GFbTransactionResolution[FDefaults.Resolution]);
  if Sil.Str.IsAssigned(Buffer) then
    Result := Char(isc_tpb_version3) + Buffer;
end;

procedure TSilFirebirdTransaction.DoAdd(var Buffer: String; Value: PChar);
begin
  if Assigned(Value) then Sil.Str.Add(Buffer, Value);
end;

function TSilFirebirdTransaction.DoGetList: IFbTransactionsInternal;
begin
  Result := IFbTransactionsInternal(FList);
end;

function TSilFirebirdTransaction.DoGetSession: IFbSessionInternal;
begin
  Result := List.Session;
end;

function TSilFirebirdTransaction.DoGetHandle: PISC_TR_HANDLE;
begin
  Result := inherited Handle.Value;
end;

procedure TSilFirebirdTransaction.Detach;
var
  lList: IFbTransactionsInternal;             
begin
  if Assigned(FList) then
  begin
    lList := List;
    FList := nil;
    Close;
    lList.Remove(Self);
  end;
end;

function TSilFirebirdTransaction.DoGetDefaults(const Defaults: IFbTransactionOptions): RFbTransactionDefaults;
begin
  Result.Model := Defaults.Model;
  Result.Access := Defaults.Access;
  Result.Resolution := Defaults.Resolution;
  Result.AutoCommit := Defaults.AutoCommit;
  Result.Retaining := Defaults.Retaining;
end;

end.
