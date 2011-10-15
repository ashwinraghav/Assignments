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

unit SilSmFirebirdDomainTimestamp;

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
  TSilFirebirdDomainTimestamp = class(TSilFirebirdDomain)
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
{ TSilFirebirdDomainTimestamp }

type
  TSilFirebirdHandlerTimestamp = class(TSilDataHandlerDateTime)
  protected
    function ToDateTime(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus; override;
    function FromDateTime(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus; override;
  public
    class function Create: IDataHandler;
  end;

function TSilFirebirdDomainTimestamp.DoGetHandler(const Command: IFbCommandInternal; const Data: RFbDomainData): IDataHandler;
begin
  Result := TSilFirebirdHandlerTimestamp.Create;
end;

{ TSilFirebirdHandlerTimestamp }

class function TSilFirebirdHandlerTimestamp.Create: IDataHandler;
const
  Definition: RDataTypeDef = (Name: 'TIMESTAMP' ; DataType: ( Value: dtDate; ); Handler: TSilFirebirdHandlerTimestamp; );
begin
  Result := inherited Create(@Definition);
end;

function TSilFirebirdHandlerTimestamp.ToDateTime(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
var
  Origin: PFbBufferEntry absolute Source;
  Target: PDateTime absolute Destination;
  Data: PISC_TIMESTAMP;
  Rec: TCTimeStructure;
begin
  Result := DoCheck(Source, Destination, SourceSize, DestinationSize, SizeOf(Target^), tcSOk);
  if Result in tcSucceeded then
  begin
    Data := @Origin.Data;
    fb.api.utils.decode_timestamp(Data, @Rec);
    Target^ := Sil.DateTime.Encode(Sil.DateTime.Parts(Sil.Date.Parts(Rec.tm_year + cYearOffset, Rec.tm_mon + 1, Rec.tm_mday), Sil.Time.Parts(Rec.tm_hour, Rec.tm_min, Rec.tm_sec, 0)))
  end;
end;

function TSilFirebirdHandlerTimestamp.FromDateTime(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
var
  Origin: PDateTime absolute Source;
  Target: PFbBufferEntry absolute Destination;
  Data: PISC_TIMESTAMP;
  Rec: TCTimeStructure;
begin
  Result := DoCheck(Source, Destination, SourceSize, DestinationSize, SizeOf(Data^), tcSOk);

  if Result in tcSucceeded then
  begin

    Data := @Target.Data;

    with Sil.DateTime.Decode(Origin^) do
    begin
      Rec.tm_sec := Time.Seconds;
      Rec.tm_min := Time.Minutes;
      Rec.tm_hour := Time.Hour;
      Rec.tm_mday := Date.Day;
      Rec.tm_mon := Date.Month - 1;
      Rec.tm_year := Date.Year - cYearOffset;
      Rec.tm_isdst := 0;
      Rec.tm_yday := 0;
      Rec.tm_wday := 0;
    end;

    fb.api.utils.encode_timestamp(@Rec, Data);
  end;
end;

end.
 