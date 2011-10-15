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

unit SilSkFirebirdData;

{$INCLUDE Defines.inc}

interface

uses
  Sil,
  SilBeDataType,
  SilLiDataType,
  SilLiValue,
  SilSeFirebirdClient,
  SilSiFirebird,
  SilShFirebird;

type
  ESilFirebirdAssignError = class(ESilFirebird);

type
  TSilFirebirdData = class(
    TSilFirebirdDataType,
    IDataBuffer,
    IFbDataInternal )
  private
    FList: Pointer;
    FHandler: IDataHandler;
    FBinding: IFbBindingInternal;
    FBuffer: PFbBufferEntry;
  private
    function DoGetDomain: IFbDomainInternal;
    procedure DoCheckHandler;
  protected // IDataBuffer
    function IDataBuffer.GetHandler = GetDataHandler;
    function IDataBuffer.GetSize = GetDataSize;
    function IDataBuffer.GetMemory = GetDataMemory;
    function GetDataHandler: IDataHandler; virtual;
    function GetDataSize: LongWord; virtual;
    function GetDataMemory: PChar; virtual;
  protected // IFbData
    function GetBuffer: IFbBuffer;
    function GetBinding: IFbBinding;
    function GetIsAssigned: Boolean;
    function GetValue: Variant;
    procedure SetValue(const Value: Variant);
    function GetIsNull: Boolean;
    procedure SetIsNull(Value: Boolean);
    procedure Clear; overload; virtual;
  protected // IFbDataInternal
    function DoGetBuffer: IFbBufferInternal;
    function DoGetBinding: IFbBindingInternal;
    function DoGetTotalSize: LongWord;
    function DoGetMemory: PChar;
    function DoGetStatus: PShort;
  protected
    property Handler: IDataHandler read FHandler;
    property Buffer: IFbBufferInternal read DoGetBuffer;
    property Binding: IFbBindingInternal read DoGetBinding;
    property Domain: IFbDomainInternal read DoGetDomain;
    property Data: PChar read DoGetMemory;
    property Status: PShort read DoGetStatus;
    property Size: LongWord read DoGetTotalSize;
    property Value: Variant read GetValue write SetValue;
    property IsNull: Boolean read GetIsNull write SetIsNull;
  public
    constructor Create(const List: IFbBufferInternal; const Binding: IFbBindingInternal; const Buffer: PFbBufferEntry); override;
    destructor Destroy; override;
  end;

implementation

{ TSilFirebirdData }

constructor TSilFirebirdData.Create(const List: IFbBufferInternal; const Binding: IFbBindingInternal; const Buffer: PFbBufferEntry);
begin
  inherited Create(IDataBuffer(Self));
  FList := Pointer(List);
  FBinding := Binding;
  FBuffer := Buffer;
end;

destructor TSilFirebirdData.Destroy;
begin
  Clear(nil);
  FBuffer := nil;
  FBinding := nil;
  FList := nil;
  inherited;
end;

function TSilFirebirdData.GetDataHandler: IDataHandler;
begin
  DoCheckHandler;
  Result := FHandler;
end;

function TSilFirebirdData.GetDataSize: LongWord;
begin
  Result := Size;
end;

function TSilFirebirdData.GetDataMemory: PChar;
begin
  Result := Pointer(FBuffer);
end;

function TSilFirebirdData.GetBuffer: IFbBuffer;
begin
  Result := Buffer;
end;

function TSilFirebirdData.GetBinding: IFbBinding;
begin
  Result := FBinding;
end;

function TSilFirebirdData.GetIsAssigned: Boolean;
begin
  Result := True;    
end;

function TSilFirebirdData.GetIsNull: Boolean;
begin
  Result := Domain.AllowNulls and (Status^ < 0);
end;

procedure TSilFirebirdData.SetIsNull(Value: Boolean);
begin
  if Domain.AllowNulls then
  begin
    inherited Clear;
    case Value of
      True:   Status^ := -1;
      False:  Status^ :=  0;
    end
  end
  else if Value = True then
    raise Sil.Error.Create('Este valor no permite NULL');
end;

function TSilFirebirdData.GetValue: Variant;
begin
  Result := AsVariant.Value;
end;

procedure TSilFirebirdData.SetValue(const Value: Variant);
begin
  AsVariant.Value := Value;
end;

procedure TSilFirebirdData.Clear;
begin
  IsNull := True;
end;

function TSilFirebirdData.DoGetBuffer: IFbBufferInternal;
begin
  Result := IFbBufferInternal(FList);
end;

function TSilFirebirdData.DoGetBinding: IFbBindingInternal;
begin
  Result := FBinding;
end;

function TSilFirebirdData.DoGetTotalSize: LongWord;
begin
  Result := Binding.Size;
end;

function TSilFirebirdData.DoGetMemory: PChar;
begin
  Result := @FBuffer.Data;
end;

function TSilFirebirdData.DoGetStatus: PShort;
begin
  if Domain.AllowNulls then
    Result := @FBuffer.Status else
    Result := nil;
end;

function TSilFirebirdData.DoGetDomain: IFbDomainInternal;
begin
  Result := FBinding.Domain;
end;

procedure TSilFirebirdData.DoCheckHandler;
begin
  if FHandler = nil then
    FHandler := Domain.GetHandler(Buffer.Command);
end;

end.
