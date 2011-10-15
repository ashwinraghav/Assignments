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

unit SilSmSharedID;

{$INCLUDE Defines.inc}

interface

uses
  Sil,
  SilSiSharedObject;

type
  TSilSharedClass = class of TSilSharedID;

  TSilSharedID = class(
    TSilObject,
    ISharedID )
  private
    FKind: TSharedID;
  protected // ISharedID
    function GetKind: TSharedID;
    function GetValue: string; virtual; abstract;     
    function GetAsName: ISharedProgID;
    function GetAsClass: ISharedClassID;
    function GetAsModule: ISharedModuleID;
  public
    constructor CreateNew(Kind: TSharedID; const Value); reintroduce; overload; virtual;  
    class function Create(const ID: ISharedClassID): ISharedID; overload;
    class function Create(const ID: ISharedProgID): ISharedID; overload;
    class function Create(const ID: ISharedModuleID): ISharedID; overload;
    class function Create(const ID: IUnknown): ISharedID; overload;
    class function Create(const ProgID: string): ISharedID; overload;
    class function Create(const ClassID: TGUID): ISharedID; overload;
    class function Create(Kind: TSharedID; const Value: string): ISharedID; overload;
    class function Create(Kind: TSharedID; const Value: TGUID): ISharedID; overload;
    class function IsEqual(const ID1, ID2: ISharedID): Boolean;
    destructor Destroy; override;
  end;

  TSilSharedProgID = class(
    TSilSharedID,
    ISharedProgID )
  private
    FProgID: string;
  protected // ISharedProgID
    function GetProgID: string;
    function GetValue: string; override;      
  public
    constructor CreateNew(Kind: TSharedID; const Value); override;  
    destructor Destroy; override;
  end;

  TSilSharedClassID = class(
    TSilSharedID,
    ISharedClassID )
  private
    FClassID: TGUID;
  protected // ISharedClassID
    function GetClassID: TGUID;
    function GetValue: string; override;      
  public
    constructor CreateNew(Kind: TSharedID; const Value); override;  
    destructor Destroy; override;
  end;

  TSilSharedModuleID = class(
    TSilSharedID,
    ISharedModuleID )
  private
    FModuleID: string;
  protected // ISharedClassID
    function GetModuleID: string;
    function GetValue: string; override;
  public
    constructor CreateNew(Kind: TSharedID; const Value); override;  
    destructor Destroy; override;
  end;

implementation

const
  MKind: array[TSharedID] of TSilSharedClass = (TSilSharedProgID, TSilSharedClassID, TSilSharedModuleID);   

{ TSilSharedID }

constructor TSilSharedID.CreateNew(Kind: TSharedID; const Value);
begin
  inherited Create;
  FKind := Kind;
end;

class function TSilSharedID.Create(const ID: ISharedClassID): ISharedID;
begin
  Result := Create(idClass, ID.ClassID);
end;

class function TSilSharedID.Create(const ID: ISharedProgID): ISharedID;
begin
  Result := Create(ID.ProgID);
end;

class function TSilSharedID.Create(const ID: ISharedModuleID): ISharedID;
begin
  Result := Create(ID.ModuleID);
end;

class function TSilSharedID.Create(Kind: TSharedID; const Value: string): ISharedID;
begin
  Result := MKind[Kind].CreateNew(Kind, Value);
end;

class function TSilSharedID.Create(Kind: TSharedID; const Value: TGUID): ISharedID;
begin
  Result := MKind[Kind].CreateNew(Kind, Value);
end;

class function TSilSharedID.Create(const ID: IInterface): ISharedID;
var
  Unk: IUnknown;
begin
  if Ref.GetInterface(ID, ISharedClassID, Unk) then
    Result := Create(ISharedClassID(Unk))
  else if Ref.GetInterface(ID, ISharedProgID, Unk) then
    Result := Create(ISharedProgID(Unk))
  else if Ref.GetInterface(ID, ISharedModuleID, Unk) then
    Result := Create(ISharedModuleID(Unk))
  else
    Result := nil;
end;

class function TSilSharedID.Create(const ClassID: TGUID): ISharedID;
begin
  Result := Create(idClass, ClassID);
end;

class function TSilSharedID.Create(const ProgID: string): ISharedID;
begin
  Result := Create(idName, ProgID);
end;

destructor TSilSharedID.Destroy;
begin
  inherited;
end;

function TSilSharedID.GetAsClass: ISharedClassID;
begin
  Result := Self as ISharedClassID;
end;

function TSilSharedID.GetAsModule: ISharedModuleID;
begin
  Result := Self as ISharedModuleID;
end;

function TSilSharedID.GetAsName: ISharedProgID;
begin
  Result := Self as ISharedProgID;
end;

function TSilSharedID.GetKind: TSharedID;
begin
  Result := FKind;
end;

class function TSilSharedID.IsEqual(const ID1, ID2: ISharedID): Boolean;
begin
  Result := (ID1.Kind = ID2.Kind) and (Sil.Text.Compare(ID1.Value, ID2.Value) = 0);
end;

{ TSilSharedProgID }

constructor TSilSharedProgID.CreateNew(Kind: TSharedID; const Value); 
begin
  inherited;
  FProgID := string(Value); 
end;

destructor TSilSharedProgID.Destroy;
begin
  inherited;
end;

function TSilSharedProgID.GetProgID: string;
begin
  Result := FProgID;
end;

function TSilSharedProgID.GetValue: string;
begin
  Result := FProgID; 
end;

{ TSilSharedClassID }

constructor TSilSharedClassID.CreateNew(Kind: TSharedID; const Value); 
begin
  inherited;
  FClassID := TGUID(Value); 
end;

destructor TSilSharedClassID.Destroy;
begin
  inherited;
end;

function TSilSharedClassID.GetClassID: TGUID;
begin
  Result := FClassID;
end;

function TSilSharedClassID.GetValue: string;
begin
  Result := Sil.GUID.ToStr(FClassID);
end;

{ TSilSharedModuleID }

constructor TSilSharedModuleID.CreateNew(Kind: TSharedID; const Value); 
begin
  inherited;
  FModuleID := string(Value);
end;

destructor TSilSharedModuleID.Destroy;
begin
  inherited;
end;

function TSilSharedModuleID.GetModuleID: string;
begin
  Result := FModuleID;
end;

function TSilSharedModuleID.GetValue: string;
begin
  Result := FModuleID;
end;

end.
