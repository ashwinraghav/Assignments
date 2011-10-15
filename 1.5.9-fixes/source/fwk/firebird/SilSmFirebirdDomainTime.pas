{********************************************************************************
 *                  Standard Interface Library (SIL)                            *
 *                                                                              *
 *       General purpose library whose design is based in STRONG                *
 *   use of interfaces.                                                         *
 *                                                                              *
 *                                                                              *
 *     Copyright (C) 2000 Mariano Podest�    antiriad@gmail.com                 *
 *     Copyright (C) 2000 Leandro Conde      lconde@str.com.ar                  *
 *     Copyright (C) 2000 Lisandro Podest�   lisandrop@movi.com.ar              *
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

unit SilSmFirebirdDomainTime;

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
  TSilFirebirdDomainTime = class(TSilFirebirdDomain)
  protected
    function DoGetHandler(const Command: IFbCommandInternal; const Data: RFbDomainData): IDataHandler; override;
  end;

implementation

uses
  SilLhDataType,
  SilLmDataTypeDateTime,
  SilScFirebirdClient,
  SilSfFirebirdClient,
  SilSfFirebird;

{ TSilFirebirdDomainTime }

type
  TSilFirebirdHandlerTypeTime = class(TSilDataHandlerDateTime)
  protected
    function ToDateTime(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus; override;
    function FromDateTime(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus; override;
  public
    class function Create: IDataHandler;
  end;

function TSilFirebirdDomainTime.DoGetHandler(const Command: IFbCommandInternal; const Data: RFbDomainData): IDataHandler;
begin
  Result := TSilFirebirdHandlerTypeTime.Create;
end;

{ TSilFirebirdHandlerTypeTime }

class function TSilFirebirdHandlerTypeTime.Create: IDataHandler;
const
  Definition: RDataTypeDef = (Name: 'TIME' ; DataType: ( Value: dtDate; ); Handler: TSilFirebirdHandlerTypeTime; );
begin
  Result := inherited Create(@Definition);
end;

function TSilFirebirdHandlerTypeTime.ToDateTime(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
var
  Origin: PFbBufferEntry absolute Source;
  Target: PDateTime absolute Destination;
  Data: PISC_TIME;
  Rec: TCTimeStructure;
begin
  Result := DoCheck(Source, Destination, SourceSize, DestinationSize, SizeOf(Target^), tcSOk);
  if Result in tcSucceeded then
  begin
    Data := @Origin.Data;
    fb.api.utils.decode_sql_time(Data, @Rec);
    Target^ := Sil.Time.Encode(Sil.Time.Parts(Rec.tm_hour, Rec.tm_min, Rec.tm_sec, 0));
  end;
end;

function TSilFirebirdHandlerTypeTime.FromDateTime(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
var
  Origin: PDateTime absolute Source;
  Target: PFbBufferEntry absolute Destination;
  Data: PISC_TIME;
  Rec: TCTimeStructure;
begin
  Result := DoCheck(Source, Destination, SourceSize, DestinationSize, SizeOf(Data^), tcSOk);

  if Result in tcSucceeded then
  begin

    Data := @Target.Data;

    with Sil.Time.Decode(Origin^) do
    begin
      Rec.tm_sec := Seconds;
      Rec.tm_min := Minutes;
      Rec.tm_hour := Hour;
      Rec.tm_mday := 0;
      Rec.tm_mon := 0;
      Rec.tm_year := 0;
      Rec.tm_isdst := 0;
      Rec.tm_yday := 0;
      Rec.tm_wday := 0;
    end;

    fb.api.utils.encode_sql_time(@Rec, Data);
  end;
end;

end.
