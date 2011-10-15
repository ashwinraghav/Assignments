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

unit SilVmProfileDocument;

interface

uses
  Sil,
  SilViProfileDocument;

type
  TProfileList = class;
     
  TProfileDocument = class (
  //- extends
    TInterfacedObject,
  //- implements
    IProfileName,
    IProfileDocument)
  private
    FValues: TProfileList;
    FOwners: TProfileList;
  protected //- IProfileName
    function GetName: string;
  protected //- IProfileDocument
    function GetValues: IProfileList;
    function GetOwners: IProfileList;
  public
    constructor Create(const AName: string); reintroduce;
    destructor Destroy; override;
    property Values: TProfileList read FValues;
    property Owners: TProfileList read FOwners;
  end;

  CProfileItem = class of TProfileItem;
  TProfileItem = class;

  TProfileList = class (
  //- extends
    TInterfacedObject,
  //- implements,
    IEnumerable,
    IProfileList)
  private
    FList: IPointerList;
    FClass: CProfileItem;
    FIID: PGUID;
  protected //- IProfileList
    function GetCount: Integer;
    function GetItem(Index: Integer): IProfileItem;
  public
    constructor Create(ItemKind: TProfileKindType); reintroduce;
    destructor Destroy; override;
    function AddNew: TProfileItem;
    procedure Add(Item: TProfileItem);
  public //- IEnumerable
		function Enumerate(var Enum: IEnumerator; out Item): Boolean;
    function GetEnumerator(out Enum: IEnumerator; Locked: Boolean): Boolean;
  end;

  TProfileName = class;
  TProfileObject = class;
  TProfileOwner = class;
  TProfileValue = class;
  TProfileProperty = class;

  TProfileItem = class (
  //- extends
    TInterfacedObject,
  //- implements
    IProfileItem,
    IProfileKind,
    IProfileElement)
  private
    FKind: TProfileKindType;
  protected //- IProfileItem
    function GetKind: IProfileKind;
    function GetElem: IProfileElement;
  protected //- IProfileKind
    function GetValue: TProfileKindType;
    function IsA(Kind: TProfileKindType): Boolean;
    function GetIsName: Boolean;
    function GetIsOwner: Boolean;
    function GetIsObject: Boolean;
    function GetIsValue: Boolean;
    function GetIsProperty: Boolean;
  protected //- IProfileElement
    function GetAsName:  IProfileName;
    function GetAsOwner: IProfileOwner;
    function GetAsObject: IProfileObject;
    function GetAsProperty: IProfileProperty;
    function GetAsValue: IProfileValue;
  public
    constructor Create(AKind: TProfileKindType); reintroduce; overload; 
    constructor Create; reintroduce; overload; virtual;  
    destructor Destroy; override;
    function AsName: TProfileName;
    function AsObject: TProfileObject;
    function AsOwner: TProfileOwner;
    function AsProperty: TProfileProperty;
    function AsValue: TProfileValue;
  end;

  TProfileName = class(
  //- extends
    TProfileItem,
  //- implements
    IProfileName,
    IProfileAttrs )
  private
    FName: string;
    FAttrs: TProfileList;
  protected //- IProfileName
    function GetName: string;
  protected //- IProfileAttrs
    function GetAttrs: IProfileList;
  public
    constructor Create; override;
    constructor Create(AKind: TProfileKindType; const AName: string = ''); overload;
    constructor Create(const AName: string); reintroduce; overload;
    destructor Destroy; override;
    property Name: string read FName write FName;
    property Attrs: TProfileList read FAttrs;
  end;

  TProfileObject = class (
  //- extends
    TProfileItem,
  //- implements
    IProfileObject)
  private
    FNames: TProfileList;
    FProps: TProfileList;
  protected //- IProfileObject
    function GetNames: IProfileList;
    function GetProps: IProfileList;
  public
    constructor Create; override; 
    constructor Create(AKind: TProfileKindType); overload; 
    destructor Destroy; override;
    property Names: TProfileList read FNames;
    property Props: TProfileList read FProps;
  end;

  TProfileOwner = class(
  //- extends
    TProfileObject,
  //- implements
    IProfileOwner)
  private
    FObjects: TProfileList;
  protected //- IProfileOwner
    function GetObjects: IProfileList;
  public
    constructor Create; override;
    destructor Destroy; override;
    property Objects: TProfileList read FObjects;
  end;
  
  TProfileValue = class(
  //- extends
    TProfileName,
  //- implements
    IProfileValue)
  private
    FText: string;
  protected //- IProfileValue
    function GetText: string;
    function GetAsVariant(OfType: Word): Variant;
    function GetAsInteger: Integer;
    function GetAsString: string;
    function GetAsFloat: Double;
  public
    constructor Create; override;
    constructor Create(const AName: string; const AText: string = ''); overload;
    property Text: string read FText write FText;
  end;
  
  TProfileProperty = class(
  //- extends
    TProfileName,
  //- implements
    IProfileProperty)
  private
    FValue: TProfileValue;
    procedure SetValue(Value: TProfileValue);
  protected //- IProfileProperty
    function GetValue: IProfileValue;
  public
    constructor Create; override;
    constructor Create(const AName: string); overload; 
    property Value: TProfileValue read FValue write SetValue;
  end;

implementation

{ TProfileDocument }

constructor TProfileDocument.Create(const AName: string);
begin
  inherited Create;
  FValues := TProfileList.Create(pkValue);
  FOwners := TProfileList.Create(pkOwner);
end;

destructor TProfileDocument.Destroy;
begin
  FOwners.Free;
  FValues.Free;
  inherited;
end;

function TProfileDocument.GetValues: IProfileList;
begin
  Result := FValues;
end;

function TProfileDocument.GetOwners: IProfileList;
begin
  Result := FOwners;
end;

function TProfileDocument.GetName: string;
begin
  Result := 'Document';
end;

{ TProfileList }

const
  KindClasses: array[TProfileKindType] of CProfileItem =
  (  TProfileOwner,
     TProfileObject,
     TProfileProperty,
     TProfileValue,
     TProfileName );
    
  KindIIDs: array[TProfileKindType] of PGUID =
  (  @IID_IProfileOwner,
     @IID_IProfileObject,
     @IID_IProfileProperty,
     @IID_IProfileValue,
     @IID_IProfileName );

constructor TProfileList.Create(ItemKind: TProfileKindType);
begin
  inherited Create;
  _AddRef;
  FClass := KindClasses[ItemKind];
  FIID := KindIIDs[ItemKind];
  FList := SIL.Tk.PointerList(False, ObjectHandler);
end;

destructor TProfileList.Destroy;
begin
  FList := nil;
  inherited;
end;

function TProfileList.GetCount: Integer;
begin
  Result := FList.GetCount;
end;

function TProfileList.GetItem(Index: Integer): IProfileItem;
begin
  Result := TProfileItem(FList[Index]);
end;

function TProfileList.Enumerate(var Enum: IEnumerator; out Item): Boolean;
  var Obj: TProfileItem;
begin
  Result := FList.Enumerate(Enum, Obj);
  if Result then
    Obj.GetInterface(FIID^, Item);
end;

function TProfileList.GetEnumerator(out Enum: IEnumerator; Locked: Boolean): Boolean;
begin
  Result := Sil.Tk.Enumerator(FList, Enum, Locked);
end;

function TProfileList.AddNew: TProfileItem;
begin
  Result := FClass.Create;
  Add(Result);
end;

procedure TProfileList.Add(Item: TProfileItem);
begin
  Assert(Item.InheritsFrom(FClass), 'Solo se pueden agregar items que deriven de ' + FClass.ClassName);
  FList.Add(Item);
end;

{ TProfileItem }

constructor TProfileItem.Create(AKind: TProfileKindType);
begin
  inherited Create;
  _AddRef;
  FKind := AKind;
end;

constructor TProfileItem.Create;
begin
end;

destructor TProfileItem.Destroy;
begin
  inherited;
end;

function TProfileItem.AsName: TProfileName;
begin
  Result := Self as TProfileName;
end;

function TProfileItem.AsObject: TProfileObject;
begin
  Result := Self as TProfileObject;
end;

function TProfileItem.AsOwner: TProfileOwner;
begin
  Result := Self as TProfileOwner;
end;

function TProfileItem.AsProperty: TProfileProperty;
begin
  Result := Self as TProfileProperty;
end;

function TProfileItem.AsValue: TProfileValue;
begin
  Result := Self as TProfileValue;
end;

function TProfileItem.GetKind: IProfileKind;
begin
  Result := Self;  
end;

function TProfileItem.GetElem: IProfileElement;
begin
  Result := Self;
end;

function TProfileItem.IsA(Kind: TProfileKindType): Boolean;
begin
  Result := (Kind = FKind);
end;

function TProfileItem.GetIsObject: Boolean;
begin
  Result := IsA(pkObject) or IsA(pkOwner);
end;

function TProfileItem.GetIsOwner: Boolean;
begin
  Result := IsA(pkOwner);
end;

function TProfileItem.GetIsValue: Boolean;
begin
  Result := IsA(pkValue);
end;

function TProfileItem.GetValue: TProfileKindType;
begin
  Result := FKind;
end;

function TProfileItem.GetAsName: IProfileName;
begin
  Result := AsName;
end;

function TProfileItem.GetAsObject: IProfileObject;
begin
  Result := AsObject
end;

function TProfileItem.GetAsOwner: IProfileOwner;
begin
  Result := AsOwner
end;

function TProfileItem.GetAsProperty: IProfileProperty;
begin
  Result := AsProperty
end;

function TProfileItem.GetAsValue: IProfileValue;
begin
  Result := AsValue
end;

function TProfileItem.GetIsName: Boolean;
begin
  Result := IsA(pkValue) or IsA(pkProperty);
end;

function TProfileItem.GetIsProperty: Boolean;
begin
  Result := IsA(pkProperty);
end;

{ TProfileName }

constructor TProfileName.Create;
begin
  Create('');
end;

constructor TProfileName.Create(AKind: TProfileKindType; const AName: string);
begin
  inherited Create(AKind);
  FName := AName;
  FAttrs := TProfileList.Create(pkValue);
end;

constructor TProfileName.Create(const AName: string);
begin
  Create(pkName, AName);
end;

destructor TProfileName.Destroy;
begin
  FAttrs.Free;
  inherited;
end;

function TProfileName.GetName: string;
begin
  Result := FName;
end;

function TProfileName.GetAttrs: IProfileList;
begin
  Result := FAttrs;
end;

{ TProfileObject }

constructor TProfileObject.Create;
begin
  Create(pkObject);
end;

constructor TProfileObject.Create(AKind: TProfileKindType);
begin
  inherited Create(AKind);
  FNames := TProfileList.Create(pkName);
  FProps := TProfileList.Create(pkProperty);
end;

destructor TProfileObject.Destroy;
begin
  FProps.Free;
  FNames.Free;
  inherited;
end;

function TProfileObject.GetNames: IProfileList;
begin
  Result := FNames;
end;

function TProfileObject.GetProps: IProfileList;
begin
  Result := FProps;
end;

{ TProfileOwner }

constructor TProfileOwner.Create;
begin
  inherited Create(pkOwner);
  FObjects := TProfileList.Create(pkObject);
end;

destructor TProfileOwner.Destroy;
begin
  FObjects.Free;
  inherited;
end;

function TProfileOwner.GetObjects: IProfileList;
begin
  Result := FObjects;
end;

{ TProfileValue }

constructor TProfileValue.Create(const AName, AText: string);
begin
  inherited Create(pkValue, AName);
  FText := AText;
end;

constructor TProfileValue.Create;
begin
  Create('');
end;

function TProfileValue.GetText: string;
begin
  Result := FText;
end;

function TProfileValue.GetAsFloat: Double;
begin
  Result := SIL.Str.ToFloat(FText);
end;

function TProfileValue.GetAsInteger: Integer;
begin
  Result := SIL.Str.ToInt(FText);
end;

function TProfileValue.GetAsString: string;
begin
  Result := FText;
end;

function TProfileValue.GetAsVariant(OfType: Word): Variant;
begin
  Result := Vart.ToType(FText, Sil.Vart.Empty, OfType);
end;

{ TProfileProperty }

constructor TProfileProperty.Create(const AName: string);
begin
  inherited Create(pkProperty, AName);
end;

constructor TProfileProperty.Create;
begin
  Create('');
end;

function TProfileProperty.GetValue: IProfileValue;
begin
  Result := FValue;
end;

procedure TProfileProperty.SetValue(Value: TProfileValue);
begin
  if Assigned(FValue) then FValue._Release;
  FValue := Value;
  if Assigned(FValue) then FValue._AddRef;
end;

end.
