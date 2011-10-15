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

unit SilSkFirebirdDomain;

{$INCLUDE Defines.inc}

interface

uses
  Sil, SilLiDataType, SilLiValue,
  SilSeFirebirdClient,
  SilSiFirebird,
  SilShFirebird;

type
  TSilFirebirdDomain = class(
    TSilFirebirdDomainType,
    IFbDomainInternal )
  private
    FData: RFbDomainData;
  protected // IFbDomain
    function GetName: string; virtual;
    function GetBaseType: TFbType;
    function GetDataType: TFbDataType; virtual;
    function GetBlobType: TFbBlobType; virtual;
    function GetLength: LongWord; virtual;
    function GetScale: Integer; virtual;
    function GetAllowNulls: Boolean;
  protected // IFbDomainInternal
    function GetSize: LongWord; virtual;
    function GetHandler(const Command: IFbCommandInternal): IDataHandler;
  protected
    function DoGetHandler(const Command: IFbCommandInternal; const Data: RFbDomainData): IDataHandler; virtual; abstract;
  protected
    property BaseType: TFbType read FData.BaseType;
    property BlobType: TFbBlobType read FData.BlobType;
    property Length: LongWord read FData.Length;
    property Scale: Integer read FData.Scale;
    property AllowNulls: Boolean read FData.AllowNulls;
  public
    constructor Create(const Data: RFbDomainData); override;
    destructor Destroy; override;
  end;

implementation

uses
  SilSfFirebirdDomain,
  SilSfFirebird;

{ TSilFirebirdDomain }

constructor TSilFirebirdDomain.Create(const Data: RFbDomainData);
begin
  inherited Create;
  FData := Data;
end;

destructor TSilFirebirdDomain.Destroy;
begin
  inherited;
end;

function TSilFirebirdDomain.GetDataType: TFbDataType;
begin
  Result := FbType(FData.BaseType);
end;

function TSilFirebirdDomain.GetBaseType: TFbType;
begin
  Result := FData.BaseType;
end;

function TSilFirebirdDomain.GetBlobType: TFbBlobType;
begin
  Result := fbbtBlobUntyped;
end;

function TSilFirebirdDomain.GetName: string;
begin
  Result := Sil.Enum.Name(System.TypeInfo(TFbDataType), Ord(GetDataType()), 'fbdt');
end;

function TSilFirebirdDomain.GetLength: LongWord;
begin
  Result := FData.Length;
end;

function TSilFirebirdDomain.GetSize: LongWord;
begin
  Result := GetLength;
end;

function TSilFirebirdDomain.GetHandler(const Command: IFbCommandInternal): IDataHandler;
begin
  Result := DoGetHandler(Command, FData);
end;

function TSilFirebirdDomain.GetScale: Integer;
begin
  Result := FData.Scale;
end;

function TSilFirebirdDomain.GetAllowNulls: Boolean;
begin
  Result := FData.AllowNulls;
end;

end.
