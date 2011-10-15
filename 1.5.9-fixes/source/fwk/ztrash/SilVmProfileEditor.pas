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

unit SilVmProfileEditor;

interface

uses
  Sil,

  SilViTreeView,
  SilViProfileDocument,
  SilViProfileEditor;

type
  TProfileEditorLevel = class;
  TProfileEditorBind = class;
     
  TProfileEditor = class(
  //- Extends
    TInterfacedObject,
  //- Implements
    IProfileEditor,
    IProfileEditorLevel )
  private
    FView: ITreeView;
    FBinds: IPointerList;
    function AddBind(const Node: ITreeNode; const OwnerName, ObjectName, PropertyName: string): TProfileEditorBind;
    procedure DoBuildBindings(const ADocument: IProfileDocument);
    procedure DoBuildBinding(const ADocument: IProfileDocument; Bind: TProfileEditorBind);
    function DoCreateOwner(const ADocument: IProfileDocument; const Name: string): IProfileOwner;
    function DoCreateObject(const ADocument: IProfileDocument; const Parent: IProfileOwner; const Name: string): IProfileObject;
    function DoCreateProperty(const ADocument: IProfileDocument; const Parent: IProfileObject; const Name: string): IProfileProperty;
    function DoCreateValue(const ADocument: IProfileDocument; const Parent: IProfileProperty; const Value: Boolean): IProfileValue; 
  protected //- IProfileEditor
    function GetRoot: IProfileEditorLevel;
    function BuildDocument: IProfileDocument;
    procedure ApplyDocument(const ADocument: IProfileDocument);
  protected //- IProfileEditorItem
    function GetNode: ITreeNode;
  protected //- IProfileEditorLevel
    function Add(const Text: string): IProfileEditorLevel;
    function Bind(const Text: string; const OwnerName, ObjectName, PropertyName: string): IProfileEditorBind;
  public
    constructor Create(const View: ITreeView);
    destructor Destroy; override;
  end;

  TProfileEditorItem = class(
  //- Extends
    TNonCountedObject,
  //- Implements
    IProfileEditorItem )
  private // IProfileEditorItem
    FEditor: TProfileEditor;
    FNode: ITreeNode;
    FText: string;
    FData: Pointer;
  protected
    function GetNode: ITreeNode;
    function GetText: string;
    procedure SetText(const Value: string);
    function GetData: Pointer;
    procedure SetData(const Value: Pointer);
  public
    constructor Create(Editor: TProfileEditor; const Node: ITreeNode);
    destructor Destroy; override;
    property Text: string read FText write FText;
    property Data: Pointer read FData write FData;
    property Node: ITreeNode read FNode;
  end;

  TProfileEditorLevel = class(
  //- Extends
    TProfileEditorItem,
  //- Implements
    IProfileEditorLevel )
  private
  protected //- IProfileEditorLevel
    function Add(const Text: string): IProfileEditorLevel;
    function Bind(const Text: string; const OwnerName, ObjectName, PropertyName: string): IProfileEditorBind;
  public
    constructor Create(Editor: TProfileEditor; const Node: ITreeNode);
    destructor Destroy; override;
  end;

  TProfileEditorBind = class(
  //- Extends
    TProfileEditorItem,
  //- Implements
    IProfileEditorBind )
  private
    FOwner: string;
    FObject: string;
    FProperty: string;
  protected
    function GetValue: Variant;
    procedure SetValue(const Value: Variant);
  public
    constructor Create(Editor: TProfileEditor; const Node: ITreeNode; const OwnerName, ObjectName, PropertyName: string);
    destructor Destroy; override;
  public
    property OwnerName: string read FOwner;
    property ObjectName: string read FObject;
    property PropertyName: string read FProperty;
  end;

implementation

uses
  TypInfo,
  SilVtProfileDocument;

{ TProfileEditor }

constructor TProfileEditor.Create(const View: ITreeView);
begin
  inherited Create;
  FBinds := SIL.Tk.PointerList();
  FView := View;
end;

destructor TProfileEditor.Destroy;
begin
  FView := nil;
  FBinds := nil;
  inherited;
end;

function TProfileEditor.GetNode: ITreeNode;
begin
  Result := nil;
end;

function TProfileEditor.Add(const Text: string): IProfileEditorLevel;
begin
  Result := TProfileEditorLevel.Create(Self, FView.Add(nil, Text));
end;

function TProfileEditor.Bind(const Text: string; const OwnerName, ObjectName, PropertyName: string): IProfileEditorBind;
begin
  Result := AddBind(FView.Add(nil, Text), OwnerName, ObjectName, PropertyName);
end;

function TProfileEditor.GetRoot: IProfileEditorLevel;
begin
  Result := Self;
end;

procedure TProfileEditor.ApplyDocument(const ADocument: IProfileDocument);
var
  Enum: IEnumerator;
  Bind: TProfileEditorBind;
  OwnerItem, ObjectItem, PropItem: IProfileItem;
  Found: Boolean;
begin
  { TODO -ojorge -cadvice : Agregué el chequeo de nil porque cuando creaba un nuevo
  registro en la tabla de profiles me venia el Document en nil por estar el blob vacío }
  if ADocument = nil then Exit;

  while FBinds.Enumerate(Enum, Bind) do
  begin
    Found := List.FindsByName(ADocument.Owners,           Bind.OwnerName, OwnerItem);
    if not Found then Continue;
    Found := List.FindsByName(OwnerItem.Elem.AsOwner.Objects, Bind.ObjectName, ObjectItem);
    if not Found then Continue;
    Found := List.FindsByName(ObjectItem.Elem.AsObject.Props,  Bind.PropertyName, PropItem);
    if not Found then Continue;
    Bind.Node.Options.Checked := PropItem.Elem.AsProperty.Value.AsVariant[varBoolean];
  end;
end;

function TProfileEditor.BuildDocument: IProfileDocument;
begin
  Result := Document.NewDocument();
  DoBuildBindings(Result);
end;

function TProfileEditor.AddBind(const Node: ITreeNode; const OwnerName, ObjectName, PropertyName: string): TProfileEditorBind;
begin
  Result := TProfileEditorBind.Create(Self, Node, OwnerName, ObjectName, PropertyName);
end;

procedure TProfileEditor.DoBuildBindings(const ADocument: IProfileDocument);
var
  Enum: IEnumerator;
  Bind: TProfileEditorBind;
begin
  while FBinds.Enumerate(Enum, Bind) do
    DoBuildBinding(ADocument, Bind);
end;

procedure TProfileEditor.DoBuildBinding(const ADocument: IProfileDocument; Bind: TProfileEditorBind);
var
  OwnerItem: IProfileOwner;
  ObjectItem: IProfileObject;
  PropItem: IProfileProperty;
begin
  OwnerItem := DoCreateOwner(ADocument, Bind.OwnerName);
  ObjectItem := DoCreateObject(ADocument, OwnerItem, Bind.ObjectName);
  PropItem := DoCreateProperty(ADocument, ObjectItem, Bind.PropertyName);
  DoCreateValue(ADocument, PropItem, Bind.Node.Options.Checked);
end;

function TProfileEditor.DoCreateOwner(const ADocument: IProfileDocument; const Name: string): IProfileOwner;
var
  Item: IProfileItem;
begin
  if not List.FindsByName(ADocument.Owners, Name, Item) then
  begin
    Item := Document.NewOwner(ADocument);
    Document.NewName(Item.Elem.AsOwner.Names, Name);
  end;
  Result := Item.Elem.AsOwner;
end;

function TProfileEditor.DoCreateObject(const ADocument: IProfileDocument; const Parent: IProfileOwner; const Name: string): IProfileObject;
var
  Item: IProfileItem;
begin
  if not List.FindsByName(Parent.Objects, Name, Item) then
  begin
    Item := Document.NewObject(Parent);
    Document.NewName(Item.Elem.AsObject.Names, Name);
  end;
  Result := Item.Elem.AsObject;
end;

function TProfileEditor.DoCreateProperty(const ADocument: IProfileDocument; const Parent: IProfileObject; const Name: string): IProfileProperty;
var
  Item: IProfileItem;
begin
  if not List.FindsByName(Parent.Props, Name, Item) then
    Item := Document.NewProperty(Parent, Name);
  Result := Item.Elem.AsProperty;
end;

function TProfileEditor.DoCreateValue(const ADocument: IProfileDocument; const Parent: IProfileProperty; const Value: Boolean): IProfileValue;
var
  Item: IProfileItem;
begin
  Item := Document.NewValue(nil, GetEnumName(TypeInfo(Boolean), Ord(Value)));
  Prop.SetValue(Parent, Item.Elem.AsValue);
end;

{ TProfileEditorItem }

constructor TProfileEditorItem.Create(Editor: TProfileEditor; const Node: ITreeNode);
begin
  inherited Create;
  FEditor := Editor;
  FNode := Node;
end;

destructor TProfileEditorItem.Destroy;
begin
  FNode := nil;
  FEditor := nil;
  inherited;
end;

function TProfileEditorItem.GetNode: ITreeNode;
begin
  Result := FNode;
end;

function TProfileEditorItem.GetText: string;
begin
  Result := FText;
end;

procedure TProfileEditorItem.SetText(const Value: string);
begin
  FText := Value;
end;

function TProfileEditorItem.GetData: Pointer;
begin
  Result := FData;
end;

procedure TProfileEditorItem.SetData(const Value: Pointer);
begin
  FData := Value;
end;

{ TProfileEditorLevel }

constructor TProfileEditorLevel.Create(Editor: TProfileEditor; const Node: ITreeNode);
begin
  inherited Create(Editor, Node);
end;

destructor TProfileEditorLevel.Destroy;
begin
  inherited;
end;

function TProfileEditorLevel.Add(const Text: string): IProfileEditorLevel;
begin
  Result := TProfileEditorLevel.Create(FEditor, FNode.Add(Text, nil));  
end;

function TProfileEditorLevel.Bind(const Text: string; const OwnerName, ObjectName, PropertyName: string): IProfileEditorBind;
begin
  Result := FEditor.AddBind(FNode.Add(Text), OwnerName, ObjectName, PropertyName);
end;

{ TProfileEditorBind }

constructor TProfileEditorBind.Create(Editor: TProfileEditor; const Node: ITreeNode; const OwnerName, ObjectName, PropertyName: string);
begin
  inherited Create(Editor, Node);
  FOwner := OwnerName;
  FObject := ObjectName;
  FProperty := PropertyName;
  FEditor.FBinds.Add(Self);
end;

destructor TProfileEditorBind.Destroy;
begin
  FEditor.FBinds.Remove(Self);
  inherited;
end;

function TProfileEditorBind.GetValue: Variant;
begin
end;

procedure TProfileEditorBind.SetValue(const Value: Variant);
begin
end;

end.
