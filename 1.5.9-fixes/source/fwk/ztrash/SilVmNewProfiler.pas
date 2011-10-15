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

unit SilVmNewProfiler;

interface

{$i Defines.inc}

uses
	Classes,
	Forms,
	Controls,

  Sil,
  SilClasses,
  SilVCL,
  SilViProfileDocument,
	SilViNewProfiler;

type
	TProfileValue = class;
	TProfileValueList = class;
	TOwner = class;
	TOwnerList = class;

{ TProfiler }
                     
	TProfiler = class(TInterfacedObject, IProfiler)
	private
		FSource: IProfileSource;
		FValues: TProfileValueList;
    FVCLMain: Boolean;
		procedure DoSetProp(Obj: TPersistent; const PropName, Value: String);
		procedure DoCreateForm(InstanceClass: TComponentClass; var Reference; Parent: TWinControl);
		function DoUpdate(const Document: IProfileDocument; InitRead: Boolean): Boolean;
    procedure DoReadOwners(const Owners: IProfileOwner);
    procedure DoReadOwnerNames(List: TOwnerList; const Names: IProfileList);
    procedure DoReadObjectNames(Owner: TOwner; const Names: IProfileList);
    procedure DoReadObjects(List: TOwnerList; const Owners: IProfileOwner);
    procedure DoReadOwnerObjects(Owner: TOwner; const Owners: IProfileOwner);
    procedure DoReadObjectProps(Owner: TOwner; const Props: IProfileList);
    procedure DoReadObjectProp(Owner: TOwner; const Prop: IProfileProperty);
	protected //- IProfiler
		function GetSource: IProfileSource;
		procedure SetSource(const Value: IProfileSource);
		function Update(const Name: String = ''): Boolean; overload;
		function Update(const Document: IProfileDocument; const Owner: String = ''): Boolean; overload;
		function Value(const Name: String): String;
		function OrdValue(Obj: TPersistent; const PropName, ValueName: String): Boolean;
		procedure CreateForm(InstanceClass: TComponentClass; var Reference; Parent: TWinControl); overload;
		procedure CreateForm(InstanceClass: TComponentClass; var Reference; const Name: String; Parent: TWinControl); overload;
		procedure CreateForm(InstanceClass: TComponentClass; var Reference; const Name: String); overload;
		procedure CreateForm(InstanceClass: TComponentClass; var Reference); overload;
		procedure CreateForm(InstanceClass: TComponentClass); overload;
	public
    class function App: IProfiler;
		constructor Create(const Source: IProfileSource);
		destructor Destroy; override;
	end;

{ TProfileValue }

	TProfileValue = class
	private
		FName: String;
		FValue: String;
	public
		property Name: String read FName;
		property Value: String read FValue;
	end;

{ TProfileValueList }

	TProfileValueList = class
  private
    FList: IPointerList; 
	private
		function InternalGet(Index: Integer): TProfileValue;
	public
		function AddNew: TProfileValue;
		function GetValue(const Name: String; const Create: Boolean): TProfileValue;
		property Items[Index: Integer]: TProfileValue read InternalGet; default;
  public
    constructor Create;
    destructor Destroy; override;
	end;

	TOwner = class
		Obj: TComponent;
		List: IPointerList;
		constructor Create;
		destructor Destroy; override;
	end;

{ TOwnerList }

	TOwnerList = class
  private
    FList: IPointerList;
	private
		function InternalGet(Index: Integer): TOwner;
	public
		function AddNew(AComp: TComponent): TOwner;
		function Enumerate(var Enum: IEnumerator; out Item): Boolean;
		property Items[Index: Integer]: TOwner read InternalGet; default;
  public
    constructor Create;
    destructor Destroy; override;
	end;

implementation                          

uses
	TypInfo {$IFNDEF D50}, Graphics {$ENDIF};

var
	AppProf: IProfiler = nil;

{ TProfiler }

constructor TProfiler.Create(const Source: IProfileSource);
begin
	inherited Create;
	FValues := TProfileValueList.Create;
	FSource := Source;
end;

destructor TProfiler.Destroy;
begin
	FValues.Free;
	FSource := nil;
	inherited;
end;

class function TProfiler.App: IProfiler;
begin
  if AppProf = nil then AppProf := TProfiler.Create(nil);
  Result := AppProf;
end;

function TProfiler.DoUpdate(const Document: IProfileDocument; InitRead: Boolean): Boolean;
var
  Enum: IEnumerator;
  Item: IProfileOwner;
begin
  Result := Document <> nil;
  if Result then with Document.Owners do
    while Enumerate(Enum, Item) do
      DoReadOwners(Item);
end;

procedure TProfiler.DoReadOwners(const Owners: IProfileOwner);
var
  List: TOwnerList;
begin
  List := TOwnerList.Create;
  try
    DoReadOwnerNames(List, Owners.Names);
    DoReadObjects(List, Owners);
  finally
    List.Free;
  end;
end;

procedure TProfiler.DoReadOwnerNames(List: TOwnerList; const Names: IProfileList);
var
  Enum: IEnumerator;
  Item: IProfileName;
  Comp: TComponent;
begin
  while Names.Enumerate(Enum, Item) do
  begin
    if Vcl.Comp.FindGlobal(nil, Item.Name, Comp) then
      List.AddNew(Comp);
  end;
end;

procedure TProfiler.DoReadObjectNames(Owner: TOwner; const Names: IProfileList);
var
  Enum: IEnumerator;
  Item: IProfileName;
  Comp: TComponent;
begin
  Owner.List.Clear;
  while Names.Enumerate(Enum, Item) do
  begin
    if  Vcl.Comp.FindChild(Owner.Obj, nil, Item.Name, Comp) then
      Owner.List.Add(Comp);
  end;
end;

procedure TProfiler.DoReadObjects(List: TOwnerList; const Owners: IProfileOwner);
var
  Enum: IEnumerator;
  Item: TOwner;
begin
  while List.Enumerate(Enum, Item) do
    DoReadOwnerObjects(Item, Owners);
end;

procedure TProfiler.DoReadOwnerObjects(Owner: TOwner; const Owners: IProfileOwner);
var
  Enum: IEnumerator;
  Item: IProfileObject;
begin
  while Owners.Objects.Enumerate(Enum, Item) do
  begin
    DoReadObjectNames(Owner, Item.Names);
    DoReadObjectProps(Owner, Item.Props);
  end;
end;

procedure TProfiler.DoReadObjectProps(Owner: TOwner; const Props: IProfileList);
var
  Enum: IEnumerator;
  Item: IProfileProperty;
begin
  while Props.Enumerate(Enum, Item) do
    DoReadObjectProp(Owner, Item);
end;

procedure TProfiler.DoReadObjectProp(Owner: TOwner; const Prop: IProfileProperty);
var
  Enum: IEnumerator;
  Item: TComponent;
begin
  while Owner.List.Enumerate(Enum, Item) do
    DoSetProp(Item, Prop.Name, Prop.Value.Text);
end;

function TProfiler.Update(const Name: String): Boolean;
begin
	Result := (FSource <> nil) and DoUpdate(FSource.ReadDocument(Name), True);
end;

function TProfiler.Update(const Document: IProfileDocument; const Owner: String = ''): Boolean; 
begin
  Result := DoUpdate(Document, False);
end;

{$IFDEF D50}

procedure SetIntIdent(Obj: TPersistent; PropInfo: Pointer; const Ident: String);
var
	V: Longint;
	IdentToInt: TIdentToInt;
begin
	IdentToInt := FindIdentToInt(PPropInfo(PropInfo)^.PropType^);
	if Assigned(IdentToInt) and IdentToInt(Ident, V) then SetOrdProp(Obj, PropInfo, V);
end;

{$ELSE}

function GetPropInfo(Obj: TPersistent; const PropName: String): PPropInfo;
begin
	Result := TypInfo.GetPropInfo(Obj.ClassInfo, PropName);
end;

procedure SetIntIdent(Obj: TPersistent; PropInfo: Pointer; const Ident: String);
var
	V: Longint;
begin
	try
		V := StringToColor(Ident);
		SetOrdProp(Obj, PropInfo, V);
	except end;
end;

{$ENDIF}

procedure TProfiler.DoSetProp(Obj: TPersistent; const PropName, Value: String);

{$IFNDEF D50}

type
	TIntegerSet = set of 0..SizeOf(Integer) * 8 - 1;

	function GetOrdProp(Obj: TPersistent; const PropName: String): Integer;
	begin
		Result := TypInfo.GetOrdProp(Obj, GetPropInfo(Obj, PropName));
	end;

{$ENDIF}

	function ReadSet(SetType: Pointer): Integer;
	var
		EnumType: PTypeInfo;
		EnumName, sVal: String;
		i: Integer;
	begin
		Result := 0;
		sVal := SIL.Str.RemoveDelimiters(Value, '[', ']');
		EnumType := GetTypeData(SetType)^.CompType^;
		i := 1;
		repeat
			EnumName := SIL.Str.Trim(SIL.Str.Token(sVal, ',', i));
			Include(TIntegerSet(Result), GetEnumValue(EnumType, EnumName));
		until i = 0;
	end;

var
	iPos, iValue, e: Integer;
	PropInfo: PPropInfo;
	PropType: PTypeInfo;
begin
	iPos := Pos('.', PropName);

	if iPos = 0 then
	begin
		PropInfo := GetPropInfo(Obj, PropName);

		if PropInfo <> nil then
		begin
			PropType := PropInfo^.PropType^;

			try
				case PropType^.Kind of
					tkInteger:
					begin
						Val(Value, iValue, e);
						if e <> 0 then
							SetIntIdent(Obj, PropInfo, Value) else
							SetOrdProp(Obj, PropInfo, iValue);
					end;
					tkChar:
						SetOrdProp(Obj, PropInfo, Ord(SIL.Str.ToChr(Value, #32)));
					tkEnumeration:
						SetOrdProp(Obj, PropInfo, GetEnumValue(PropType, Value));
					tkFloat:
						SetFloatProp(Obj, PropInfo, SIL.Str.ToFloat(value));
					tkString, tkLString, tkWString:
						SetStrProp(Obj, PropInfo, SIL.Str.RemoveDelimiters(Value, #39, #39));
					tkSet:
						SetOrdProp(Obj, PropInfo, ReadSet(PropType));
					tkClass:
						if (GetTypeData(PropInfo^.PropType^)^.ClassType = TStrings) then
							TStrings(TypInfo.GetOrdProp(Obj, PropInfo)).SetText(PChar(Value));
				end;
			except end;
		end;
	end else
	begin
		Obj := TPersistent(GetOrdProp(Obj, SIL.Str.Copy(PropName, 1, iPos - 1)));
		DoSetProp(Obj, SIL.Str.Copy(PropName, iPos + 1), Value);
	end;
end;

procedure TProfiler.SetSource(const Value: IProfileSource);
begin
	FSource := Value;
end;

function TProfiler.GetSource: IProfileSource;
begin
	Result := FSource;
end;

function TProfiler.OrdValue(Obj: TPersistent; const PropName, ValueName: String): Boolean;
var
	sVal: String;
	iValue, e: Integer;
	PropInfo: PPropInfo;
begin
	sVal := Value(ValueName);
	Val(sVal, iValue, e);
	PropInfo := GetPropInfo(Obj, PropName);
	Result := e = 0;

	if PropInfo <> nil then
	begin
		if not Result then
			SetIntIdent(Obj, PropInfo, sVal) else
			SetOrdProp(Obj, PropInfo, iValue);
	end else
		Result := false;
end;

function TProfiler.Value(const Name: String): String;
var
	pVal: TProfileValue;
begin
	if (Length(Name) > 0) and (Name[1] <> '$') then
		pVal := FValues.GetValue('$' + Name, false) else
		pVal := FValues.GetValue(Name, false);

	if pVal <> nil then
		Result := pVal.Value else
		Result := '';
end;

procedure TProfiler.CreateForm(InstanceClass: TComponentClass); 
  var Form: TForm;
begin
	CreateForm(InstanceClass, Form);
end;

procedure TProfiler.CreateForm(InstanceClass: TComponentClass; var Reference);
begin
	CreateForm(InstanceClass, Reference, nil);
end;

procedure TProfiler.CreateForm(InstanceClass: TComponentClass; var Reference; Parent: TWinControl);
begin
	DoCreateForm(InstanceClass, Reference, Parent);
	Update(TComponent(Reference).Name);
end;

procedure TProfiler.CreateForm(InstanceClass: TComponentClass; var Reference; const Name: String);
begin
	CreateForm(InstanceClass, Reference, Name, nil);
end;

procedure TProfiler.CreateForm(InstanceClass: TComponentClass; var Reference; const Name: String; Parent: TWinControl);
  var Document: IProfileDocument;
begin
  if FSource <> nil then Document := FSource.ReadDocument(Name);
	if (Document <> nil) then
	begin
		DoCreateForm(InstanceClass, Reference, Parent);
    TComponent(Reference).Name := Name;
		DoUpdate(Document, false);
	end;
end;

procedure TProfiler.DoCreateForm(InstanceClass: TComponentClass; var Reference; Parent: TWinControl);
begin
	try
		if (Application.MainForm <> nil) or FVCLMain then
    begin
			TComponent(Reference) := InstanceClass.Create(Parent);
    end else
    begin
      FVCLMain := true; // soluciona un bug de la vcl
			Application.CreateForm(InstanceClass, Reference);
    end;
	except
		TComponent(Reference) := nil;
		raise;
	end;
end;

{ TProfileValueList }

constructor TProfileValueList.Create;
begin
  inherited;
  FList := Sil.Tk.PointerList(True, ObjectHandler);
end;

destructor TProfileValueList.Destroy;
begin
  FList := nil;
  inherited;
end;

function TProfileValueList.AddNew: TProfileValue;
begin
	Result := TProfileValue.Create;
	FList.Add(Result);
end;

function TProfileValueList.InternalGet(Index: Integer): TProfileValue;
begin
	Result := FList[Index];
end;

function TProfileValueList.GetValue(const Name: String; const Create: Boolean): TProfileValue;
var
	i: Integer;
begin
	for i := 0 to FList.Count - 1 do
	begin
		Result := Items[i];
		if SIL.Str.CompareText(Result.Name, Name) = 0 then Exit;
	end;

	if Create then
	begin
		Result := AddNew;
		Result.FName := Name;
	end else
	begin
		i := SIL.Str.LastDelimiter('.', Name);
		if i > 0 then
			Result := GetValue(SIL.Str.Copy(Name, i + 1), false) else
			Result := nil;
	end;
end;

{ TOwner }

constructor TOwner.Create;
begin
	List := Sil.Tk.PointerList;
end;

destructor TOwner.Destroy;
begin
	List := nil;
end;

{ TOwnerList }

function TOwnerList.AddNew(AComp: TComponent): TOwner;
begin
	Result := TOwner.Create;
  Result.Obj := AComp;
	FList.Add(Result);
end;

constructor TOwnerList.Create;
begin
  inherited;
  FList := Sil.Tk.PointerList(True, ObjectHandler);
end;

destructor TOwnerList.Destroy;
begin
  FList := nil;
  inherited;
end;

function TOwnerList.Enumerate(var Enum: IEnumerator; out Item): Boolean;
begin
  Result := FList.Enumerate(Enum, Item);
end;

function TOwnerList.InternalGet(Index: Integer): TOwner;
begin
	Result := FList[Index];
end;

(*function TProfiler.DoUpdate(const Name: String; InitRead: Boolean): Boolean;
var
	Owners, Comps: TOwnerList;

	procedure DoReadOwnedObjects(Owner: TOwner);
	var
		sItem, sObj: String;
		iPos: Integer;
		Obj: TObject;
	begin
		Owner.List.Clear;

		sItem := FSource.ItemName;
		iPos := 1;

		repeat
			sObj := SIL.Str.Trim(SIL.Str.Token(sItem, ',', iPos));
			Obj := Owner.Obj.FindComponent(sObj);
			if Obj <> nil then Owner.List.Add(Obj);
		until iPos = 0;
	end;

	procedure DoSetValues;
	var
		i: Integer;
		PVal: TProfileValue;
		sItem: String;
	begin
		if Owners.Count > 0 then
		begin
			for i := 0 to Owners.Count - 1 do
			begin
				sItem := Owners[i].Obj.Name + '.' + FSource.PropName;
				PVal := FValues.GetValue(sItem, true);
				PVal.FValue := FSource.PropValue;
			end;
		end else
		begin
			sItem := FSource.PropName;
			PVal := FValues.GetValue(sItem, true);
			PVal.FValue := FSource.PropValue;
		end;
	end;

	procedure DoReadOwnedProperty(Owner, Obj: TComponent; sProp, sValue: String);
	var
		PVal: TProfileValue;
		sItem: String;
	begin
		if Length(sProp) > 0 then
		begin
			if sValue[1] = '$' then
			begin
				sItem := Owner.Name + '.' + sValue;
				PVal := FValues.GetValue(sItem, false);
				if PVal <> nil then sValue := PVal.Value;
			end;
			DoSetProp(Obj, sProp, sValue);
		end;
	end;

	procedure DoReadSetProperty;
	var
		i, j: Integer;
		sProp, sValue: String;
	begin
		sProp := SIL.Str.Trim(FSource.PropName);
		sValue := SIL.Str.Trim(FSource.PropValue);

		for i := 0 to Owners.Count - 1 do
			if Owners[i].List.Count > 0 then
			begin
				for j := 0 to Owners[i].List.Count - 1 do
					DoReadOwnedProperty(Owners[i].Obj, Owners[i].List[j], sProp, sValue);
			end else
				DoReadOwnedProperty(Owners[i].Obj, Owners[i].Obj, sProp, sValue);
	end;

begin
	Result := false;
	if FSource = nil then Exit;

	Owners := TOwnerList.Create(fmFreeObject);
	Comps := TOwnerList.Create(fmFreeObject);

	try
		Result := not InitRead or FSource.BeginRead(Name);
		if not Result then Exit;

		while FSource.HasMore do
		begin
			FSource.ReadNext;
			if FSource.ItemIsOwner then
        DoReadOwners
      else if FSource.ItemIsObject then
        DoReadObjects
      else if FSource.ItemIsValue then
        DoSetValues
      else
        DoReadSetProperty;
		end;

		FSource.EndRead(Name);
	finally
		Owners.Free;
		Comps.Free;
	end;
end;*)


end.

