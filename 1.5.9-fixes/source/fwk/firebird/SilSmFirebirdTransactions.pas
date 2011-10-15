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

unit SilSmFirebirdTransactions;

{$INCLUDE Defines.inc}

interface

uses
  Sil, SilContainer,
  SilSeFirebirdClient,
  SilSiFirebird,
  SilShFirebird;

type
  TSilFirebirdTransactionList = class(
    TSilObject,
    IFbTransactions,
    IFbTransactionsInternal,
    IFbTransactionOptions,
    IFbTransactionOptionsDef )
  private
    FSession: Pointer;
    FList: IContainerDynamic;
    FDefaults: RFbTransactionDefaults;
  private
    procedure DoClear;
    procedure DoCheckList;
    function DoCompare(Data1, Data2: HData; Param: Pointer): Integer;
  protected // IFbTransactions
    function GetCount: Integer;
    function GetOptions: IFbTransactionOptionsDef;
    function GetItems(Index: Integer): IFbTransaction;
    function GetByName(const Name: string): IFbTransaction;
    function Find(Field: TFbTransactionField; Data: HData; Item: PInteger = nil): Boolean; overload;
    function Find(Field: TFbTransactionField; Data: HData; out Item: IFbTransaction): Boolean; overload;
    function Find(Field: TFbTransactionField; Data: HData; const IID: TGUID; out Item): Boolean; overload;
    function Start(const Name: string = ''; AutoCommit: Boolean = True): IFbTransaction;
  protected // IFbTransactionsInternal
    function DoGetSession: IFbSessionInternal;
    function Add(const Transaction: IFbTransactionInternal): Integer;
    function Remove(const Transaction: IFbTransactionInternal): Integer;
  protected // IFbTransactionOptions
    function GetModel: TFbTransactionModel;
    function GetAccess: TFbTransactionAccess;
    function GetResolution: TFbTransactionResolution;
    function GetAutoCommit: Boolean;
    function GetRetaining: Boolean;
  protected // IFbTransactionOptionsDef
    procedure SetAccess(const Value: TFbTransactionAccess);
    procedure SetModel(const Value: TFbTransactionModel);
    procedure SetResolution(const Value: TFbTransactionResolution);
    procedure SetAutoCommit(const Value: Boolean);
    procedure SetRetaining(const Value: Boolean);
  protected
    property Session: IFbSessionInternal read DoGetSession;
    property Model: TFbTransactionModel read GetModel write SetModel;
    property Access: TFbTransactionAccess read GetAccess write SetAccess;
    property Resolution: TFbTransactionResolution read GetResolution write SetResolution;
  public
    constructor Create(const Session: IFbSessionInternal);
    destructor Destroy; override;
  end;

implementation

uses
  SilSdFirebird,
  SilSmFirebirdTransaction;

{ TSilFirebirdTransactionList }

constructor TSilFirebirdTransactionList.Create(const Session: IFbSessionInternal);
begin
  inherited Create;
  FDefaults.Model := fbtmUnespecified;
  FDefaults.Access := fbtaUnespecified;
  FDefaults.Resolution := fbtrUnespecified;
  FDefaults.AutoCommit := True;
  FDefaults.Retaining := False;
  FSession := Pointer(Session);
end;

destructor TSilFirebirdTransactionList.Destroy;
begin
  DoClear;
  FList := nil;
  FSession := nil;
  inherited;
end;

function TSilFirebirdTransactionList.GetCount: Integer;
begin
  if Assigned(FList) then
    Result := FList.Items.Count else
    Result := 0;
end;

function TSilFirebirdTransactionList.GetOptions: IFbTransactionOptionsDef;
begin
  Result := Self;
end;

function TSilFirebirdTransactionList.GetItems(Index: Integer): IFbTransaction;
begin
  if Assigned(FList) then
    Result := IFbTransactionInternal(FList[Index]^) else
    Result := nil;
end;

function TSilFirebirdTransactionList.GetByName(const Name: string): IFbTransaction;
begin
  if not Find(fbtfName, @Name, Result) then
    raise Sil.Error.Create(SErrTransactionNameNotFound, [Name]);
end;

function TSilFirebirdTransactionList.Find(Field: TFbTransactionField; Data: HData; Item: PInteger): Boolean;
begin
  Result := Assigned(FList) and FList.Find(Data, PItem(Item), nil, Pointer(Field));
end;

function TSilFirebirdTransactionList.Find(Field: TFbTransactionField; Data: HData; out Item: IFbTransaction): Boolean;
begin
  Result := Find(Field, Data, IFbTransaction, Item);
end;

function TSilFirebirdTransactionList.Find(Field: TFbTransactionField; Data: HData; const IID: TGUID; out Item): Boolean;
var
  Index: Integer;
begin
  Result := Find(Field, Data, @Index) and Sil.Ref.Get(GetItems(Index), IID, Item);
end;

function TSilFirebirdTransactionList.Start(const Name: string; AutoCommit: Boolean): IFbTransaction;
begin
  Result := IFbTransactionInternal(TSilFirebirdTransaction.Create(Self, Name, AutoCommit));
end;

function TSilFirebirdTransactionList.Add(const Transaction: IFbTransactionInternal): Integer;
begin
  DoCheckList;
  Result := FList.Add(@Transaction);
end;

function TSilFirebirdTransactionList.Remove(const Transaction: IFbTransactionInternal): Integer;
begin
  if Find(fbtfInstance, @Transaction, @Result) then
    FList.Items.Delete(Result);
end;

function TSilFirebirdTransactionList.GetModel: TFbTransactionModel;
begin
  Result := FDefaults.Model;
end;

function TSilFirebirdTransactionList.GetAccess: TFbTransactionAccess;
begin
  Result := FDefaults.Access;   
end;

function TSilFirebirdTransactionList.GetResolution: TFbTransactionResolution;
begin
  Result := FDefaults.Resolution;   
end;

function TSilFirebirdTransactionList.GetAutoCommit: Boolean;
begin
  Result := FDefaults.AutoCommit;   
end;

function TSilFirebirdTransactionList.GetRetaining: Boolean;
begin
  Result := FDefaults.Retaining;
end;

procedure TSilFirebirdTransactionList.SetAccess(const Value: TFbTransactionAccess);
begin
  FDefaults.Access := Value;
end;

procedure TSilFirebirdTransactionList.SetModel(const Value: TFbTransactionModel);
begin
  FDefaults.Model := Value;
end;

procedure TSilFirebirdTransactionList.SetResolution(const Value: TFbTransactionResolution);
begin
  FDefaults.Resolution := Value;
end;

procedure TSilFirebirdTransactionList.SetAutoCommit(const Value: Boolean);
begin
  FDefaults.AutoCommit := Value;
end;

procedure TSilFirebirdTransactionList.SetRetaining(const Value: Boolean);
begin
  FDefaults.Retaining := Value;
end;

function TSilFirebirdTransactionList.DoGetSession: IFbSessionInternal;
begin
  Result := IFbSessionInternal(FSession);
end;

procedure TSilFirebirdTransactionList.DoClear;
var
  Item: IFbTransactionInternal;
begin
  if Assigned(FList) then
    while FList.Items.Count > 0 do
    begin
      Item := IFbTransactionInternal(FList.Cursors.Last.Data^);
      Item.Detach;
    end;
end;

procedure TSilFirebirdTransactionList.DoCheckList;
begin
  if not Assigned(FList) then
    FList := Vector.Create(SizeOf(IFbTransactionInternal), DoCompare);
end;

function TSilFirebirdTransactionList.DoCompare(Data1, Data2: HData; Param: Pointer): Integer;
begin
  case TFbTransactionField(Param) of
    fbtfID:         Result := Integer(Data1^) - IFbTransaction(Data2^).ID;
    fbtfInstance:   Result := Sil.Ref.Compare(IFbTransaction(Data1^), IFbTransaction(Data2^));
    fbtfName:       Result := Sil.Str.TextCompare(string(Data1^), IFbTransaction(Data2^).Name);
    else            Result := -1;
  end;
end;

end.
