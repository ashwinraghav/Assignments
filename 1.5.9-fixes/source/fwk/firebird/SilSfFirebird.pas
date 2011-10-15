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

unit SilSfFirebird;

{$INCLUDE Defines.inc}

interface

uses
  Sil, 
  SilSiFirebirdClient,
  SilSeFirebirdClient,
  SilSiFirebird,
  SilShFirebird;

function Check(ErrCode: ISC_STATUS; RaiseError: Boolean = True): ISC_STATUS;
procedure Throw(vector: PStatusVector);
function Error(vector: PStatusVector): string;
function SqlType(BaseType: TFbType; AllowNulls: Boolean = True): Short; overload;
function SqlType(const Domain: IFbDomain): Short; overload;
function BlobType(Value: TFbBlobType): Short; overload;
function BlobType(const Domain: IFbDomain): Short; overload;

function Param(const Name: string; const Value: Variant): RParameter;
function Values(const Data: array of Variant): IParameterList; overload; 
function Params(const Data: array of RParameter): IParameterList; overload;  
function CheckTransaction(const Session: IFbSessionInternal; Transaction: IFbTransaction = nil; AutoCommit: Boolean = True): IFbTransactionInternal; overload; 
function CheckTransaction(const Transaction: IFbTransaction; const Default: IFbTransactionInternal): IFbTransactionInternal; overload;

function Bindings(const Variables: IFbVariablesInternal): IFbBindingsInternal; overload;
function Bindings(const Session: IFbSessionInternal): IFbBindingsInternal; overload;
function Buffer(const Command: IFbCommandInternal; const Bindings: IFbBindingsInternal; HoldCommand: Boolean = True): IFbBufferInternal; overload;
function Buffer(const Command: IFbCommandInternal; const Bindings: IFbBindingsInternal; const Values: IParameterList; HoldCommand: Boolean = True): IFbBufferInternal; overload;
function SetValues(const Buffer: IFbBufferInternal; const Values: IParameterList): IFbBufferInternal; overload;  


implementation

uses
  SilSeFirebird,
  SilScFirebirdClient,
  SilSfFirebirdClient,
  SilSmFirebirdBindings,
  SilSmFirebirdBuffer;

function Check(ErrCode: ISC_STATUS; RaiseError: Boolean = True): ISC_STATUS;
begin
  result := ErrCode;
  if RaiseError and (ErrCode > 0) then
    Throw(fb.vector);
end;

procedure Throw(vector: PStatusVector);
var
  SQLCode: Long;
  ErrorCode: Long;
  Mesg: string;
begin
  SQLCode := fb.api.sql.code(@vector[0]);
  ErrorCode := vector[1];
  Mesg := Error(vector);

  if SQLCode <> SQLERROR_NOPERMISSION then
    raise EFirebirdServerError.Create(SQLCode, ErrorCode, Mesg) else
    raise EFirebirdAccessDenied.Create(SQLCode, ErrorCode, Mesg)
end;
                                
function Failed(Status: ISC_STATUS; out Code: ISC_STATUS): Boolean;
begin
  Result := Status > 0;
  Code := Status;
end;

function Error(vector: PStatusVector): string;
var
  SQLCode: Long;
  ErrorCode: Long;
  Code: ISC_STATUS;
  StatusPtr: PISC_STATUS;
  Buffer: array[0 .. 32766] of Char;
begin
  Result := 'Firebird Error';
  ErrorCode := vector[1];
  SQLCode := fb.api.sql.code(@vector[0]);

  Sil.Str.Add(Result,   '  SQL CODE : %d', [SQLCode], sLineBreak);
  fb.api.sql.interprete(SQLCode, Buffer, SizeOf(Buffer) - 1);
  Sil.Str.Add(Result,   '  SQL ERROR: %dL [%s]', [ErrorCode, string(Buffer)], sLineBreak);

  Sil.Str.Add(Result, sLineBreak);
  StatusPtr := @vector[0];
  while Failed(fb.api.utils.interprete(Buffer, @StatusPtr), Code) do
    Sil.Str.Add(Result, '  CODE     : %dL [%s]', [Code, string(Buffer)], sLineBreak);
end;

function SqlType(BaseType: TFbType; AllowNulls: Boolean = True): Short;
begin
  Result := Integer(BaseType) * 2 + Ord(AllowNulls);
end;

function SqlType(const Domain: IFbDomain): Short; 
begin
  Result := SqlType(Domain.BaseType, Domain.AllowNulls);
end;                                              

function BlobType(Value: TFbBlobType): Short;
begin
  Result := Short(Ord(Value));
end;

function BlobType(const Domain: IFbDomain): Short; 
begin
  Result := BlobType(Domain.BlobType);
end;

function Values(const Data: array of Variant): IParameterList;
var
  Index: Integer;
begin
  Result := Sil.List.Parameters();
  for Index := Low(Data) to High(Data) do
    Result['Param ' + Sil.Int.ToStr(Succ(Index))] := Data[Index];
end;

function Params(const Data: array of RParameter): IParameterList;
var
  Index: Integer;
begin
  Result := Sil.List.Parameters();
  for Index := Low(Data) to High(Data) do
    with Data[Index] do
      Result[Name] := Value;
end;

function Param(const Name: string; const Value: Variant): RParameter;
begin
  Result.Name := Name;
  Result.Value := Value;
end;

function CheckTransaction(const Session: IFbSessionInternal; Transaction: IFbTransaction; AutoCommit: Boolean): IFbTransactionInternal;
begin
  ASSERT(Assigned(Session));
  if not Assigned(Transaction) then
    Transaction := Session.StartTransaction('', AutoCommit);
  Result := Transaction as IFbTransactionInternal;
end;

function CheckTransaction(const Transaction: IFbTransaction; const Default: IFbTransactionInternal): IFbTransactionInternal;
begin
  if Assigned(Transaction) then
    Result := Transaction as IFbTransactionInternal else
    Result := Default;
end;

function Bindings(const Variables: IFbVariablesInternal): IFbBindingsInternal;
var
  Enum: IEnumerator;
  Item: IFbVariableInternal;
begin
  if Assigned(Variables) and (Variables.Count > 0) then
  begin
    Result := Bindings(Variables.Session);
    with Variables do
      if Count > 0 then
        while Enumerate(Enum, IFbVariableInternal, Item) do
          Result.Add(Item);
  end else
    Result := nil;
end;

function Bindings(const Session: IFbSessionInternal): IFbBindingsInternal;
begin
  ASSERT(Assigned(Session));
  Result := TSilFirebirdBindingList.Create(Session);
end;

function Buffer(const Command: IFbCommandInternal; const Bindings: IFbBindingsInternal; HoldCommand: Boolean): IFbBufferInternal;
begin
  if Assigned(Bindings) then
    Result := TSilFirebirdBuffer.Create(Command, Bindings, HoldCommand) else
    Result := nil;
end;

function Buffer(const Command: IFbCommandInternal; const Bindings: IFbBindingsInternal; const Values: IParameterList; HoldCommand: Boolean): IFbBufferInternal;
begin
  Result := SetValues(Buffer(Command, Bindings, HoldCommand), Values);
end;

function SetValues(const Buffer: IFbBufferInternal; const Values: IParameterList): IFbBufferInternal;
var
  Enum: IEnumerator;
  Item: RParameter;
begin
  Result := Buffer;
  if Assigned(Result) and Assigned(Values) then
    with Values do
      while Enumerate(Enum, Item) do
        Result.ItemByName[Item.Name].Value := Item.Value;
end;

end.
