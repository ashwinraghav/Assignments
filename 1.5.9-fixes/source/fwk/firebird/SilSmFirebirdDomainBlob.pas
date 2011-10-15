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

unit SilSmFirebirdDomainBlob;

{$INCLUDE Defines.inc}

interface

uses
  Sil,
  SilBeDataType,
  SilLiDataType,
  SilSeFirebirdClient,
  SilSiFirebird,
  SilShFirebird,
  SilSkFirebirdDomain;

type
  TSilFirebirdDomainBlob = class(TSilFirebirdDomain)
  protected
    function GetBlobType: TFbBlobType; override;
  protected
    function DoGetHandler(const Command: IFbCommandInternal; const Data: RFbDomainData): IDataHandler; override;
  end;

implementation

uses
  SilLhDataType,
  SilLmDataTypeAnsiString,
  SilScFirebirdClient,
  SilSfFirebirdClient,
  SilSfFirebird;

{ TSilFirebirdDomainBlob }

type
  TSilFirebirdHandlerBlob = class(TSilDataHandlerAnsiString)
  private
    FCommand: IFbCommandInternal;
  private
    function DoGetInfo(Handle: PISC_BLOB_HANDLE; Item: Byte): string;
    function DoReadBlob(const Command: IFbCommandInternal; ID: PISC_QUAD): String;
    procedure DoWriteBlob(const Command: IFbCommandInternal; ID: PISC_QUAD; const Value: String);
    function DoGetSegmented(Handle: PISC_BLOB_HANDLE): string;
    procedure DoSetSegmented(Handle: PISC_BLOB_HANDLE; const Value: String);
    function DoGetStream(Handle: PISC_BLOB_HANDLE): string;
    procedure DoSetStream(Handle: PISC_BLOB_HANDLE; const Value: String);
  protected
    function ToAnsiString(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus; override;
    function FromAnsiString(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus; override;
  public
    constructor CreateNew(const List: IDataTypeList; Def: PDataTypeDef; Parameter: Pointer); override;
    destructor Destroy; override; 
  public
    class function Create(const Command: IFbCommandInternal): IDataHandler;
  end;

function TSilFirebirdDomainBlob.GetBlobType: TFbBlobType;
begin
  Result := inherited BlobType;
end;

function TSilFirebirdDomainBlob.DoGetHandler(const Command: IFbCommandInternal; const Data: RFbDomainData): IDataHandler;
begin
  Result := TSilFirebirdHandlerBlob.Create(Command);
end;

{ TSilFirebirdHandlerBlob }

class function TSilFirebirdHandlerBlob.Create(const Command: IFbCommandInternal): IDataHandler; 
const
  Definition: RDataTypeDef = (Name: 'TIMESTAMP' ; DataType: ( Value: dtAnsiString; ); Handler: TSilFirebirdHandlerBlob; );
begin
  Result := inherited Create(@Definition, Pointer(Command));
end;

constructor TSilFirebirdHandlerBlob.CreateNew(const List: IDataTypeList; Def: PDataTypeDef; Parameter: Pointer);
begin
  inherited;
  FCommand := IFbCommandInternal(Parameter);
end;

destructor TSilFirebirdHandlerBlob.Destroy;
begin
  FCommand := nil;
  inherited;
end;

function TSilFirebirdHandlerBlob.ToAnsiString(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
var
  Origin: PFbBufferEntry absolute Source;
  Target: PAnsiString absolute Destination;
  ID: PISC_QUAD;
begin
  Result := DoCheck(Source, Destination, SourceSize, DestinationSize, SizeOf(Target^), tcSOk);
  if Result in tcSucceeded then
  begin
    ID := @Origin.Data;
    Target^ := DoReadBlob(FCommand, ID);
  end;
end;

function TSilFirebirdHandlerBlob.FromAnsiString(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
var
  Origin: PAnsiString absolute Source;
  Target: PFbBufferEntry absolute Destination;
  ID: PISC_QUAD;
begin
  Result := DoCheck(Source, Destination, SourceSize, DestinationSize, SizeOf(Target^), tcSOk);
  if Result in tcSucceeded then            
  begin
    ID := @Target.Data;
    DoWriteBlob(FCommand, ID, Origin^);
  end;
end;

function TSilFirebirdHandlerBlob.DoGetInfo(Handle: PISC_BLOB_HANDLE; Item: Byte): string;
var
  Buffer: array[0..1023] of Byte;
  Len: PSmallint;
begin
  FillChar(Buffer, SizeOf(Buffer), 0);
  Check(fb.api.blob.info(fb.status, Handle, SizeOf(Item), @Item, SizeOf(Buffer), @Buffer[0]));
  if Item = Buffer[0] then
  begin
    Len := @Buffer[1];
    SetString(Result, PChar(@Buffer[3]), Len^);
  end;
end;

function TSilFirebirdHandlerBlob.DoReadBlob(const Command: IFbCommandInternal; ID: PISC_QUAD): String;
var
  Handle: TISC_BLOB_HANDLE;
  Info: string;
begin
  Check(fb.api.blob.open(fb.status, Command.Session.Database.Handle, Command.Transaction.Handle, @Handle, ID, 0, nil));
  try
    Info := DoGetInfo(@Handle, isc_info_blob_type);
    if PChar(Info)^ <> #0 then
      Result := DoGetStream(@Handle) else
      Result := DoGetSegmented(@Handle);
  finally
    fb.api.blob.close(fb.status, @Handle);
  end;
end;

procedure TSilFirebirdHandlerBlob.DoWriteBlob(const Command: IFbCommandInternal; ID: PISC_QUAD; const Value: String);
var
  Handle: TISC_BLOB_HANDLE;
  Info: string;
  OK: Boolean;
begin
  if LargeInt(ID^) <> 0 then
    OK := fb.api.blob.open(fb.status, Command.Session.Database.Handle, Command.Transaction.Handle, @Handle, ID, 0, nil) = 0 else
    OK := False;    

  if not OK then Check(fb.api.blob.create(fb.status, Command.Session.Database.Handle, Command.Transaction.Handle, @Handle, ID, 0, nil));
  try
    Info := DoGetInfo(@Handle, isc_info_blob_type);
    
    if PChar(Info)^ <> #0 then
      DoSetStream(@Handle, Value) else
      DoSetSegmented(@Handle, Value);
      
  finally
    fb.api.blob.close(fb.status, @Handle);
  end;
end;

function TSilFirebirdHandlerBlob.DoGetSegmented(Handle: PISC_BLOB_HANDLE): string;
var
  Info: string;
  Data: string;
  Status: ISC_STATUS;
  Len: UShort;
begin
  Result := '';
  Info := DoGetInfo(Handle, isc_info_blob_max_segment);
  SetLength(Data, fb.api.utils.vax_integer(PChar(Info), SizeOf(Integer)));
  repeat
    Status := fb.api.blob.get_segment(fb.status, Handle, @Len, Length(Data), PChar(Data));
    Sil.Str.Append(Result, PChar(Data)^, Len);
  until (Status <> 0);
  if Status <> isc_segstr_eof then Check(Status);
end;

procedure TSilFirebirdHandlerBlob.DoSetSegmented(Handle: PISC_BLOB_HANDLE; const Value: String);
var
  Info: string;
  Buffer, Data: string;
  Status: ISC_STATUS;
  Size: Integer;
begin
  Buffer := Value;
  Info := DoGetInfo(Handle, isc_info_blob_max_segment);
  Size := fb.api.utils.vax_integer(PChar(Info), SizeOf(Integer));
  if Size = 0 then Size := 128;
  repeat
    Sil.Str.Split(Buffer, Size, Data, Buffer);
    Status := fb.api.blob.put_segment(fb.status, Handle, Length(Data), PChar(Data));
    if Status <> 0 then Break;
  until Length(Buffer) = 0;
  if Status <> 0 then Check(Status);
end;

function TSilFirebirdHandlerBlob.DoGetStream(Handle: PISC_BLOB_HANDLE): string;
begin
end;

procedure TSilFirebirdHandlerBlob.DoSetStream(Handle: PISC_BLOB_HANDLE; const Value: String);
begin
end;

end.
