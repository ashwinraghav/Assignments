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

unit SilSmFirebirdDomainText;

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
  TSilFirebirdDomainText = class(TSilFirebirdDomain)
  protected
    function GetSize: LongWord; override;
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
  
{ TSilFirebirdDomainText }

type
  TSilFirebirdHandlerText = class(TSilDataHandlerAnsiString)
  protected
    function ToAnsiString(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus; override;
    function FromAnsiString(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus; override;
  public
    class function Create: IDataHandler;
  end;

function TSilFirebirdDomainText.DoGetHandler(const Command: IFbCommandInternal; const Data: RFbDomainData): IDataHandler;
begin
  Result := TSilFirebirdHandlerText.Create;
end;

function TSilFirebirdDomainText.GetSize: LongWord;
begin
  Result := Sil.Int.Align(inherited GetSize + SizeOf(AnsiChar), SizeOf(RFbBufferEntry));
end;

{ TSilFirebirdHandlerText }

class function TSilFirebirdHandlerText.Create: IDataHandler;
const
  Definition: RDataTypeDef = (Name: 'CHAR' ; DataType: ( Value: dtAnsiString; ); Handler: TSilFirebirdHandlerText; );
begin
  Result := inherited Create(@Definition);
end;

function TSilFirebirdHandlerText.ToAnsiString(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
var
  Origin: PFbBufferEntry absolute Source;
  Target: PAnsiString absolute Destination;
  Data: PChar;
  Value: AnsiString;
begin
  Result := DoCheck(Source, Destination, SourceSize, DestinationSize, SizeOf(Target^), tcSOk);
  if Result in tcSucceeded then
  begin
    Data := @Origin.Data;
    SetString(Value, Data, SourceSize);
    Target^ := Sil.Str.Trim(Value);
  end;
end;

function TSilFirebirdHandlerText.FromAnsiString(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
var
  Origin: PAnsiString absolute Source;
  Target: PFbBufferEntry absolute Destination;
  Len: LongWord;
  Data: PChar;
begin
  if Assigned(Origin) then
    Len := Sil.Str.Len(Origin^) else
    Len := 0;  
  Result := DoCheck(Source, Destination, SourceSize, DestinationSize, Len, tcSOk);
  if Result in tcSucceeded then
  begin
    Dec(DestinationSize, SizeOf(Target^));
    if Len > DestinationSize then Len := DestinationSize;
    Data := @Target.Data;
    Sil.Mem.Clear(Data^, DestinationSize, Byte(ccSPC));
    Sil.Mem.Move(Origin^[1], Data^, Len);
    Result := tcSOk;
  end;
end;

end.
