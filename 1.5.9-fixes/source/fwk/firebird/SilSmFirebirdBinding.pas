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

unit SilSmFirebirdBinding;

{$INCLUDE Defines.inc}

interface

uses
  Sil, SilContainer,
  SilSeFirebirdClient,
  SilSiFirebird,
  SilShFirebird;

type
  TSilFirebirdBinding = class(
    TSilObject,
    IFbBindingInternal )
  private
    FList: Pointer;
    FName: string;
    FDomain: IFbDomainInternal;
    FIndex: Integer;
    FOffset: LongWord;
  protected // IFbBinding
    function GetList: IFbBindings;
    function GetIndex: Integer;
    function GetPosition: Integer;
    function GetName: string;
    function GetDomain: IFbDomain;
  protected // IFbBindingInternal
    function DoGetSession: IFbSessionInternal;
    function DoGetList: IFbBindingsInternal;
    function DoGetDomain: IFbDomainInternal;
    function DoGetSize: LongWord;
    function DoGetOffset: LongWord;
  protected
    property List: IFbBindingsInternal read DoGetList;
  public
    constructor Create(const List: IFbBindingsInternal; const Name: string; const Domain: RFbDomainData; Offset: LongWord);
    destructor Destroy; override; 
  end;

implementation

uses
  SilSfFirebirdDomain;

{ TSilFirebirdBinding }

constructor TSilFirebirdBinding.Create(const List: IFbBindingsInternal; const Name: string; const Domain: RFbDomainData; Offset: LongWord);
begin
  inherited Create;
  FList := Pointer(List);
  FName := Name;
  DoGetClass(Domain.BaseType).Create(Domain).GetInterface(IFbDomainInternal, FDomain);
  FIndex := List.Add(IFbBindingInternal(Self));
  FOffset := Offset;
end;

destructor TSilFirebirdBinding.Destroy;
begin
  FDomain := nil;
  FList := nil;
  inherited;
end;

function TSilFirebirdBinding.GetList: IFbBindings;
begin
  Result := List;
end;

function TSilFirebirdBinding.GetIndex: Integer;
begin
  Result := FIndex;
end;

function TSilFirebirdBinding.GetPosition: Integer;
begin
  Result := Succ(FIndex);
end;

function TSilFirebirdBinding.GetName: string;
begin
  Result := FName;
end;

function TSilFirebirdBinding.GetDomain: IFbDomain;
begin
  Result := FDomain;
end;

function TSilFirebirdBinding.DoGetSession: IFbSessionInternal;
begin
  Result := List.Session;
end;

function TSilFirebirdBinding.DoGetList: IFbBindingsInternal;
begin
  Result := IFbBindingsInternal(FList);
end;

function TSilFirebirdBinding.DoGetDomain: IFbDomainInternal;
begin
  Result := FDomain;
end;

function TSilFirebirdBinding.DoGetSize: LongWord;
begin
  Result :=  Sil.Int.Align(FDomain.Size + SizeOf(RFbBufferEntry));
end;

function TSilFirebirdBinding.DoGetOffset: LongWord;
begin
  Result := FOffset;
end;

end.
