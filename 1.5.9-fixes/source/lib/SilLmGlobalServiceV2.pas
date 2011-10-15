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

unit SilLmGlobalServiceV2;

{$I Defines.inc}

interface

uses
  SilBeTypes,
  SilLkInterfaced,
  
  SilLiGlobalServicesV2;

type
  TSilGlobalServiceV2 = class(
    TSilInterfacedObject,
    IGlobalServiceV2 )
  private
    FList: Pointer; 
    FService: PGlobalService;
    FInstance: IUnknown;
    FReferences: IGlobalReferencesV2;
    FLinks: IGlobalLinksV2;
    FModule: LongWord;
    FKind: TGlobalServiceKind;
  protected // IGlobalServiceV2
    function GetList: IGlobalListV2;
    function GetService: PGlobalService;
    function GetKind: TGlobalServiceKind;
    function GetIsCreated: Boolean;
    function GetInstance: IUnknown;
    procedure SetInstance(const Value: IUnknown);
    function GetReferences: IGlobalReferencesV2;
    function GetLinks: IGlobalLinksV2;
    function AddRef(const IID: TGuid; Obj: PUnknown; Module: LongWord): Boolean; 
    function DropRef(Obj: PUnknown; Module: LongWord): Boolean; reintroduce;
  public
    constructor Create(const List: IGlobalListV2; Service: PGlobalService; Module: LongWord; Kind: TGlobalServiceKind);
    destructor Destroy; override;
  public
    property List: IGlobalListV2 read GetList;
    property Instance: IUnknown read GetInstance write SetInstance;
  end;

implementation

uses
  SilLmGlobalLinksV2,
  SilLmGlobalReferencesV2,
  SilLfGlobalServiceV2;

{ TSilGlobalServiceV2 }

constructor TSilGlobalServiceV2.Create(const List: IGlobalListV2; Service: PGlobalService; Module: LongWord; Kind: TGlobalServiceKind);
begin
  inherited Create;
  FKind := Kind;
  FModule := Module;
  MakeRef(List, @FList);
  FService := Service;
  FLinks := TSilGlobalLinksV2.Create(Self);
  FReferences := TSilGlobalReferencesV2.Create(Self);
  DoInitialize(Self);
end;

destructor TSilGlobalServiceV2.Destroy;
begin
  FLinks.Clear;
  FReferences.Clear; 
  Self.Instance := nil;
  DoFinalize(Self);
  FReferences := nil;
  FLinks := nil;
  FService := nil;
  inherited DropRef(@FList);
  inherited;
end;

function TSilGlobalServiceV2.GetService: PGlobalService;
begin
   Result := FService;
end;

function TSilGlobalServiceV2.GetKind: TGlobalServiceKind;
begin
  Result := FKind;
end;

function TSilGlobalServiceV2.GetIsCreated: Boolean;
begin
  Result := Assigned(FInstance);
end;

function TSilGlobalServiceV2.GetInstance: IUnknown;
begin
  if not Assigned(FInstance) then FInstance := DoCreate(Self);
  Result := FInstance;
end;

procedure TSilGlobalServiceV2.SetInstance(const Value: IInterface);
begin
  if Assigned(FInstance) then DoDestroy(Self);
  FInstance := Value;
end;

function TSilGlobalServiceV2.GetLinks: IGlobalLinksV2;
begin
  Result := FLinks;
end;

function TSilGlobalServiceV2.GetReferences: IGlobalReferencesV2;
begin
  Result := FReferences;
end;

function TSilGlobalServiceV2.AddRef(const IID: TGuid; Obj: PUnknown; Module: LongWord): Boolean;
begin
  Result := Self.Instance.QueryInterface(IID, Obj^) = 0;
  if Result then
    FReferences.Add(Obj, Module);
end;

function TSilGlobalServiceV2.DropRef(Obj: PUnknown; Module: LongWord): Boolean;
begin
  Result := FReferences.Remove(Obj, Module);
end;

function TSilGlobalServiceV2.GetList: IGlobalListV2;
begin
  Result := IGlobalListV2(FList);
end;

end.
 