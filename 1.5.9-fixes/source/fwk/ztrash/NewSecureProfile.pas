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

unit NewSecureProfile;

interface

uses
	Classes,
	SysUtils,

	Sil,

  SilViProfileDocument,
  SilViNewProfiler,
  SilVmProfileDocument;

type
	TSecureProfileSource = class;

{ TSecureProfileSource }

	TCheckModule = function(const Module: String; out Level: String): Boolean of object;
 
  TSecureProfileLoader = class(TInterfacedObject, IProfileLoader)
  private
    FSource: TSecureProfileSource;
    function DoLoadDocument(const AOwner: string; const Lines: IStringList): IProfileDocument;
    procedure DoStoreDocument(const AOwner: string; const Lines: IStringList; const Document: IProfileDocument);
    function CheckValid(var Line: string): Boolean;
    function CheckOwner(const AOwner: string; var Current: TProfileOwner; var Line: string): Boolean;
    function CheckObject(var Line: string): Boolean;
    function CheckValue(var Line: string): Boolean;
    function LoadOwner(Document: TProfileDocument; Instance: TProfileOwner; const Line: string): TProfileOwner;
    function LoadObject(Document: TProfileDocument; Instance: TProfileObject; Current: TProfileOwner; const Line: string): TProfileObject;
    function LoadValue(Document: TProfileDocument; Instance: TProfileValue; const Line: string): TProfileValue;
    function LoadProperty(Document: TProfileDocument; Instance: TProfileProperty; Current: TProfileObject; const Line: string): TProfileProperty;
    function LoadNames(Document: TProfileDocument; Instance: TProfileList; const Line: string): TProfileList;
    function LoadName(Document: TProfileDocument; Instance:  TProfileName; const sText: string): TProfileName;
    function LoadValueData(Instance: TProfileValue; const Line: string): TProfileValue;
    function ParsePropertyData(const Line: string; var Name, Value: string): Boolean;
    procedure StoreOwner(const Lines: IStringList; const Item: IProfileOwner);
    procedure StoreObjects(const Lines: IStringList;  const List: IProfileList);
    procedure StoreObject(const Lines: IStringList; const Item: IProfileObject);
    procedure StoreProperties(const Lines: IStringList;  const List: IProfileList);
    procedure StoreProperty(const Lines: IStringList; const Item: IProfileProperty);
  protected //- IProfileReader
    function LoadDocument(const Stream: IStream; const Owner: string = ''): IProfileDocument;
    procedure StoreDocument(const Stream: IStream; const Document: IProfileDocument; const Owner: string  = '');
  public
    constructor Create(Source: TSecureProfileSource);
    destructor Destroy; override;
    property Source: TSecureProfileSource read FSource;
  end;

	TSecureProfileSource = class(TInterfacedObject, IProfileSource)
	private
    FLoader: IProfileLoader;
		FReader: IProfileReader;
		FLevel: string;
		FCheckModule: TCheckModule;
	protected
    function GetReader: IProfileReader;
    procedure SetReader(const Value: IProfileReader);
    function GetLoader: IProfileLoader;
    function ReadDocument(const Owner: string): IProfileDocument;
    procedure WriteDocument(const Document: IProfileDocument; const Owner: string);
	public
		constructor Create(const Reader: IProfileReader; CheckModule: TCheckModule); overload;
		constructor Create(const Reader: IProfileReader; CheckModule: Pointer); overload;
		constructor Create(CheckModule: Pointer = nil); overload;
		destructor Destroy; override;
    property Level: string read FLevel;
	end;

implementation

uses
  SilVtProfileDocument;
  
{ TSecureProfileSource }


constructor TSecureProfileSource.Create(const Reader: IProfileReader; CheckModule: TCheckModule);
begin
	inherited Create;
  FLoader := TSecureProfileLoader.Create(Self);
	FReader := Reader;
	FCheckModule := CheckModule;
end;

constructor TSecureProfileSource.Create(const Reader: IProfileReader; CheckModule: Pointer);
var
	Method: TMethod;
begin
	Method.Code := CheckModule;
	Method.Data := Self;
	Create(Reader, TCheckModule(Method));
end;

constructor TSecureProfileSource.Create(CheckModule: Pointer);
var
  Reader: Pointer;
begin
  Reader := nil;
  Create(IProfileReader(Reader), CheckModule);
end;

destructor TSecureProfileSource.Destroy;
begin
  FReader := nil;
  FLoader := nil;
	inherited;
end;

function TSecureProfileSource.ReadDocument(const Owner: string): IProfileDocument;
begin
  if Assigned(FReader) then 
    Result := FLoader.LoadDocument(FReader.Stream) else
    Result := nil;
end;

procedure TSecureProfileSource.WriteDocument(const Document: IProfileDocument; const Owner: string);
begin
  if Assigned(FReader) then
    FLoader.StoreDocument(FReader.Stream, Document, Owner);
end;

function TSecureProfileSource.GetReader: IProfileReader;
begin
  Result := FReader;
end;

procedure TSecureProfileSource.SetReader(const Value: IProfileReader);
begin
  FReader := Value;
end;

function TSecureProfileSource.GetLoader: IProfileLoader;
begin
  Result := FLoader;
end;

{ TSecureProfileLoader }

constructor TSecureProfileLoader.Create(Source: TSecureProfileSource);
begin
  inherited Create;
  FSource := Source;
end;

destructor TSecureProfileLoader.Destroy;
begin
  FSource := nil;
  inherited;
end;

function TSecureProfileLoader.LoadDocument(const Stream: IStream; const Owner: string): IProfileDocument;
var
  Lines: IStringList;
begin
  Lines := SIL.Tk.StringList();
  try
    SIL.Serializer.LoadFromStream(Lines, Stream);
    Result := DoLoadDocument(Owner, Lines);
  finally
    Lines := nil;
  end;
end;

procedure TSecureProfileLoader.StoreDocument(const Stream: IStream; const Document: IProfileDocument; const Owner: string);
var
  Lines: IStringList;
begin
  Lines := SIL.Tk.StringList();
  try
    DoStoreDocument(Owner, Lines, Document);
    SIL.Serializer.SaveToStream(Lines, Stream);
  finally
    Lines := nil;
  end;
end;

function TSecureProfileLoader.DoLoadDocument(const AOwner: string; const Lines: IStringList): IProfileDocument;
var
  Document: TProfileDocument;
  Enum: IEnumerator;
  Line: string;
  Owner: TProfileOwner;
  Current: TProfileObject;
begin
  Document := TProfileDocument.Create('');
  Result := Document;
  try
    Current := nil;
    Owner := nil;
    while Lines.Enumerate(Enum, Line) do
      if CheckValid(Line) then
      begin
        if CheckOwner(AOwner, Owner, Line) then
          begin
            Owner := LoadOwner(Document, nil, Line);
            Current := Owner;
          end
        else if CheckValue(Line) then
          LoadValue(Document, nil, Line)
        else if Owner = nil then
          Continue
        else if CheckObject(Line) then
          Current := LoadObject(Document, nil, Owner, Line)
        else if Current = nil then
          Continue
        else
          LoadProperty(Document, nil, Current, Line)
      end;
    if Current = nil then
      Result := nil;
  except
    Result := nil;
    raise;
  end;
end;

procedure TSecureProfileLoader.DoStoreDocument(const AOwner: string; const Lines: IStringList; const Document: IProfileDocument);
var
  Enum: IEnumerator;
  Item: IProfileOwner;
begin
  while Document.Owners.Enumerate(Enum, Item) do
    StoreOwner(Lines, Item);
end;

function TSecureProfileLoader.CheckValid(var Line: string): Boolean;
begin
  Line := SIL.Str.Trim(Line);
  Result := (Length(Line) > 0) and (Line[1] <> '#');
end;

function TSecureProfileLoader.CheckOwner(const AOwner: string; var Current: TProfileOwner; var Line: string): Boolean;
begin
	Result := StrLIComp(PChar(Line), 'owner ', 6) = 0;
  if Result then
  begin
    Current := nil;
    Line := SIL.Str.Trim(SIL.Str.Copy(Line, 7));
    if not SIL.Str.IsEmpty(AOwner) then
      Result := SIL.Str.Pos(AOwner, Line) <> 0;
  end;
end;

function TSecureProfileLoader.CheckObject(var Line: string): Boolean;
begin
	Result := StrLIComp(PChar(Line), 'object ', 7) = 0;
	if Result then Line := SIL.Str.Trim(SIL.Str.Copy(Line, 8));
end;

function TSecureProfileLoader.CheckValue(var Line: string): Boolean;
begin
	Result := not SIL.Str.IsEmpty(Line) and (Line[1] = '$');
end;

function TSecureProfileLoader.LoadOwner(Document: TProfileDocument; Instance: TProfileOwner; const Line: string): TProfileOwner;
begin
  Assert(Document <> nil, 'El Documento debe estar creado');
  if Instance = nil then Instance := TProfileOwner.Create;
  Document.Owners.Add(Instance);
  LoadObject(Document, Instance, nil, Line);
  Result := Instance;
end;

function TSecureProfileLoader.LoadObject(Document: TProfileDocument; Instance: TProfileObject; Current: TProfileOwner; const Line: string): TProfileObject;
begin
  if Instance = nil then Instance := TProfileObject.Create;
  if Current <> nil then Current.Objects.Add(Instance);
  LoadNames(Document, Instance.Names, Line);
  Result := Instance;
end;

function TSecureProfileLoader.LoadProperty(Document: TProfileDocument; Instance: TProfileProperty; Current: TProfileObject; const Line: string): TProfileProperty;
var
  sName, sValue: string;
begin
  Assert(Current <> nil, 'Debe existir un objeto en donde cargar la property');
  if ParsePropertyData(Line, sName, sValue) then
  begin
    if Instance = nil then Instance := TProfileProperty.Create;
    Instance.Name := sName;
    if CheckValue(sValue) then
      Instance.Value := LoadValue(Document, nil, sValue) else
      Instance.Value := TProfileValue.Create(sName, sValue);
    Current.Props.Add(Instance);
  end;
  Result := Instance;
end;

function TSecureProfileLoader.LoadValue(Document: TProfileDocument; Instance: TProfileValue; const Line: string): TProfileValue;
begin
  Instance := LoadValueData(Instance, Line);
  Document.Values.Add(Instance);
  Result := Instance;
end;

function TSecureProfileLoader.LoadNames(Document: TProfileDocument; Instance: TProfileList; const Line: string): TProfileList;
var
  sText: string;
  iPos: Integer;
begin
  Assert(Instance <> nil);
  iPos := 0;
  repeat
    sText := SIL.Str.Trim(SIL.Str.Token(Line, ',', iPos));
    Instance.Add(LoadName(Document, TProfileName.Create(), sText));
  until iPos = 0;
  Result := Instance;
end;

function TSecureProfileLoader.LoadName(Document: TProfileDocument; Instance: TProfileName; const sText: string): TProfileName;
var
  sAttr: string;
  iPos: Integer;
begin
  if Instance = nil then Instance := TProfileName.Create();
  iPos := 1;
  Instance.Name := SIL.Str.Trim(SIL.Str.Token(sText, ':', iPos));
  while iPos <> 0 do
  begin
    sAttr := SIL.Str.Trim(SIL.Str.Token(sText, '&', iPos));
    if not SIL.Str.IsEmpty(sAttr) then
      Instance.Attrs.Add(LoadValueData(TProfileValue.Create, sAttr));
  end;
  Result := Instance;
end;

function TSecureProfileLoader.LoadValueData(Instance: TProfileValue; const Line: string): TProfileValue;
var
  sName, sValue: string;
begin
  if ParsePropertyData(Line, sName, sValue) then
  begin
    if Instance = nil then Instance := TProfileValue.Create;
    Instance.Name := sName;
    Instance.Text := sValue;
  end;
  Result := Instance;
end;

function TSecureProfileLoader.ParsePropertyData(const Line: string; var Name, Value: string): Boolean;
var
  iPos: Integer;
begin
  iPos := 1;
	Name := SIL.Str.Trim(SIL.Str.Token(Line, '=', iPos));
  Result := not SIL.Str.IsEmpty(Name);
	if Result then
    Value:= SIL.Str.Trim(SIL.Str.Copy(Line, iPos));
end;

procedure TSecureProfileLoader.StoreOwner(const Lines: IStringList; const Item: IProfileOwner);
begin
  Lines.Add('owner ' + Names.ToStr(Item.Names));
  StoreObjects(Lines, Item.Objects);
end;

procedure TSecureProfileLoader.StoreObjects(const Lines: IStringList; const List: IProfileList);
var
  Enum: IEnumerator;
  Item: IProfileObject;
begin
  while List.Enumerate(Enum, Item) do
    StoreObject(Lines, Item);
end;

procedure TSecureProfileLoader.StoreObject(const Lines: IStringList; const Item: IProfileObject);
begin
  Lines.Add('  object ' + Names.ToStr(Item.Names));
  StoreProperties(Lines, Item.Props);
end;

procedure TSecureProfileLoader.StoreProperties(const Lines: IStringList; const List: IProfileList);
var
  Enum: IEnumerator;
  Item: IProfileProperty;
begin
  while List.Enumerate(Enum, Item) do
    StoreProperty(Lines, Item);
end;

procedure TSecureProfileLoader.StoreProperty(const Lines: IStringList; const Item: IProfileProperty);
begin
  Lines.Add('    ' + Prop.ToStr(Item));
end;

end.
