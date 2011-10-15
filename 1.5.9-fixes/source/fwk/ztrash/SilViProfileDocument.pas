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

unit SilViProfileDocument;

interface

uses
  Sil;

const
  SIID_IProfileDocument = '{82BE6704-55D4-11D4-9D4E-00C0DFE46337}';
  SIID_IProfileList     = '{82BE6703-55D4-11D4-9D4E-00C0DFE46337}';
  SIID_IProfileItem     = '{82BE6702-55D4-11D4-9D4E-00C0DFE46337}';
  SIID_IProfileKind     = '{82BE670A-55D4-11D4-9D4E-00C0DFE46337}';
  SIID_IProfileElement  = '{82BE6705-55D4-11D4-9D4E-00C0DFE46337}';
  SIID_IProfileName     = '{82BE6701-55D4-11D4-9D4E-00C0DFE46337}';
  SIID_IProfileAttrs    = '{D5BFF201-6007-11D4-9D5E-00C0DFE46337}';
  SIID_IProfileObject   = '{82BE6706-55D4-11D4-9D4E-00C0DFE46337}';
  SIID_IProfileOwner    = '{82BE6707-55D4-11D4-9D4E-00C0DFE46337}';
  SIID_IProfileProperty = '{82BE6709-55D4-11D4-9D4E-00C0DFE46337}';
  SIID_IProfileValue    = '{82BE6708-55D4-11D4-9D4E-00C0DFE46337}';

type

{ forward declarations }

  IProfileDocument   = interface;
  IProfileList       = interface;
  IProfileKind       = interface;
  IProfileElement    = interface;
  IProfileObject     = interface;
  IProfileOwner      = interface;
  IProfileProperty   = interface;
  IProfileValue      = interface;


{ IProfileName: intf para elementos con nombre }

  IProfileName = interface
    [SIID_IProfileName]
    function GetName: string;
  //----------------------------------------------
    property Name: string read GetName;
  end;

  IProfileAttrs = interface(IProfileName)
    [SIID_IProfileAttrs]
    function GetAttrs: IProfileList;
  //----------------------------------------------
    property Attrs: IProfileList read GetAttrs;
  end;

{ TProfileKindType: posible tipos de items }

  TProfileKindType = (pkOwner, pkObject, pkProperty, pkValue, pkName);

{ IProfileItem: items dentro del profile }

  IProfileItem = interface
    [SIID_IProfileItem]
    function GetKind: IProfileKind;
    function GetElem: IProfileElement;
  //----------------------------------------------
    property Kind: IProfileKind read GetKind;
    property Elem: IProfileElement read GetElem;
  end;

{ IProfileKind: facilita el acceso al enumerado de tipos }

  IProfileKind = interface
    [SIID_IProfileKind]
    function GetValue: TProfileKindType;
    function GetIsName: Boolean;
    function GetIsOwner: Boolean;
    function GetIsObject: Boolean;
    function GetIsValue: Boolean;
    function GetIsProperty: Boolean;
  //----------------------------------------------
    function IsA(Kind: TProfileKindType): Boolean;
    property Value: TProfileKindType read GetValue;
    property IsName: Boolean read GetIsName;
    property IsOwner: Boolean read GetIsOwner;
    property IsObject: Boolean read GetIsObject;
    property IsProperty: Boolean read GetIsProperty;
    property IsValue: Boolean read GetIsValue;
  end;

{ IProfileElement: contiene las distintas posibilidades de tipos de items }

  IProfileElement = interface(IProfileItem)
    [SIID_IProfileElement]
    function GetAsName:  IProfileName;
    function GetAsOwner: IProfileOwner;
    function GetAsObject: IProfileObject;
    function GetAsProperty: IProfileProperty;
    function GetAsValue: IProfileValue;
  //----------------------------------------------
    property AsName: IProfileName read GetAsName;
    property AsOwner: IProfileOwner read GetAsOwner;
    property AsObject: IProfileObject read GetAsObject;
    property AsProperty: IProfileProperty read GetAsProperty;
    property AsValue: IProfileValue read GetAsValue;
  end;

{ IProfileObject: contiene la info sobre un objeto }

  IProfileObject = interface(IProfileItem)
    [SIID_IProfileObject]
    function GetNames: IProfileList;
    function GetProps: IProfileList;
  //----------------------------------------------
    property Names: IProfileList read GetNames;
    property Props: IProfileList read GetProps;
  end;

{ IProfileOwner: especializacion de un objeto que contiene a otros }

  IProfileOwner = interface(IProfileObject)
    [SIID_IProfileOwner]
    function GetObjects: IProfileList;
  //----------------------------------------------
    property Objects: IProfileList read GetObjects;
  end;
  
{ IProfileElement: contiene las distintas posibilidades de tipos de items }

  IProfileProperty = interface(IProfileName)
    [SIID_IProfileProperty]
    function GetValue: IProfileValue;
  //----------------------------------------------
    property Value: IProfileValue read GetValue;
  end;

{ IProfileValue: difinicion de un valor en el profile }

  IProfileValue = interface(IProfileName)
    [SIID_IProfileValue]
    function GetText: string;
    function GetAsVariant(OfType: Word): Variant;
    function GetAsInteger: Integer;
    function GetAsString: string;
    function GetAsFloat: Double;
  //----------------------------------------------
    property Text: string read GetText;
    property AsVariant[OfType: Word]: Variant read GetAsVariant;
    property AsInteger: Integer read GetAsInteger;
    property AsString: string read GetAsString;
    property AsFloat: Double read GetAsFloat;
  end;

{ IProfileList: lista simple de items }

  IProfileList = interface(IItemization)
    [SIID_IProfileList]
    function GetCount: Integer;
    function GetItem(Index: Integer): IProfileItem;
  //----------------------------------------------
    property Count: Integer read GetCount;
    property _Item[Index: Integer]: IProfileItem read GetItem; default;
  end;

{ IProfileDocument: documento para contener la info de un profile }

  IProfileDocument = interface(IProfileName)
    [SIID_IProfileDocument]
    function GetValues: IProfileList;
    function GetOwners: IProfileList; 
  //----------------------------------------------
    property Values: IProfileList read GetValues;  
    property Owners: IProfileList read GetOwners; 
  end;


const
  IID_IProfileDocument: TGUID = SIID_IProfileDocument;
  IID_IProfileList    : TGUID = SIID_IProfileList;
  IID_IProfileItem    : TGUID = SIID_IProfileItem;
  IID_IProfileKind    : TGUID = SIID_IProfileKind;
  IID_IProfileElement : TGUID = SIID_IProfileElement;
  IID_IProfileName    : TGUID = SIID_IProfileName;
  IID_IProfileAttrs   : TGUID = SIID_IProfileAttrs;
  IID_IProfileObject  : TGUID = SIID_IProfileObject;
  IID_IProfileOwner   : TGUID = SIID_IProfileOwner;
  IID_IProfileProperty: TGUID = SIID_IProfileProperty;
  IID_IProfileValue   : TGUID = SIID_IProfileValue;


implementation
end.
