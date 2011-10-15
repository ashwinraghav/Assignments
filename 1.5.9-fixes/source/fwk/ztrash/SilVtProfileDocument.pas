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

unit SilVtProfileDocument;

interface

uses
  Sil,
  SilViProfileDocument;

type
  List = class
    class function FindsByName(const This: IProfileList; const Name: string; out ItemFound: IProfileItem): Boolean;
    class function AddItem(const This: IProfileList; const Item: IProfileItem): IProfileItem; 
  end;

  Names = class
    class function ToStr(const This: IProfileList; const Default: string = ''; const Separator: string = ','): string;
  end;

  Name = class
    class function ToStr(const This: IProfileName; const Default: string = ''): string;
  end;

  Prop = class
    class function ToStr(const This: IProfileProperty; const Default: string = ''; const ReturnValue: Boolean = True; const ValueSeparator: string = '='): string;
    class procedure SetValue(const This: IProfileProperty; const Value: IProfileValue);
  end;

  Value = class
    class function ToStr(const This: IProfileValue; const Default: string = ''): string;
  end;

  Document = class
    class function NewDocument(const Name: string = ''): IProfileDocument;
    class function NewOwner(const This: IProfileDocument): IProfileItem;
    class function NewName(const This: IProfileList; const Name: string): IProfileItem;
    class function NewObject(const This: IProfileOwner): IProfileItem;
    class function NewProperty(const This: IProfileObject; const Name: string): IProfileItem;
    class function NewValue(const This: IProfileDocument; const Text: string; const Name: string = ''): IProfileItem;
  end;

implementation

uses
  SilVmProfileDocument;

{ ProfileList }

class function List.FindsByName(const This: IProfileList; const Name: string; out ItemFound: IProfileItem): Boolean;
var
  Enum: IEnumerator;
  Obj: IUnknown;
  Temp, Item: IProfileItem;
begin
  Result := False;
  ItemFound := nil;
  while This.Enumerate(Enum, Obj) do
  begin
    Item := Obj as IProfileItem;
    case Item.Kind.Value of
      pkOwner, pkObject:
          Result := FindsByName(Item.Elem.AsObject.Names, Name, Temp);
      pkProperty, pkValue, pkName:
          Result := SIL.Str.IsEqual(Item.Elem.AsName.Name, Name);
    else
          Result := False;
    end;
    if Result then
      begin
        ItemFound := Item;
        Break;
      end;
  end;
end;

class function List.AddItem(const This: IProfileList; const Item: IProfileItem): IProfileItem;
var
  List: TProfileList;
  Instance: TProfileItem;
begin
  Instance := Reference.GetInstance(Item);
  if Assigned(This) and Assigned(Instance) then
    begin
      List := Reference.GetInstance(This);
      Assert(List <> nil);
      List.Add(Instance);
    end;
  Result := Instance;
end;

{ ProfileNames }

class function Names.ToStr(const This: IProfileList; const Default, Separator: string): string;
var
  Enum: IEnumerator;
  Item: IProfileName;
begin
  if Assigned(This) then
    begin
      Result := SIL.Str.Null;
      while This.Enumerate(Enum, Item) do
      begin
        if SIL.Str.NotEmpty(Result) then SIL.Str.Add(Result, Separator);
        SIL.Str.Add(Result, Name.ToStr(Item));
      end;
    end
  else
    Result := Default;
end;

{ Name }

class function Name.ToStr(const This: IProfileName; const Default: string): string;
begin
  if Assigned(This) then
    Result := This.Name else
    Result := Default;
end;

{ Prop }

class procedure Prop.SetValue(const This: IProfileProperty; const Value: IProfileValue);
var
  Instance: TProfileProperty;

begin
  Instance := Reference.GetInstance(This);
  if Assigned(Instance) then
    Instance.Value := Reference.GetInstance(Value);
end;

class function Prop.ToStr(const This: IProfileProperty; const Default: string; const ReturnValue: Boolean; const ValueSeparator: string): string;
begin
  if Assigned(This) then
    begin
      Result := This.Name;
      if ReturnValue then
      begin
        SIL.Str.Add(Result, ValueSeparator);
        SIL.Str.Add(Result, Value.ToStr(This.Value));
      end;
    end
  else
    Result := Default;
end;

{ Value }

class function Value.ToStr(const This: IProfileValue; const Default: string): string;
begin
  if Assigned(This) then
    Result := This.Text else
    Result := Default;
end;

{ Document }

class function Document.NewDocument(const Name: string): IProfileDocument;
begin
  Result := TProfileDocument.Create(Name);
end;

class function Document.NewName(const This: IProfileList; const Name: string): IProfileItem;
begin
  Result := List.AddItem(This, TProfileName.Create(Name));
end;

class function Document.NewObject(const This: IProfileOwner): IProfileItem;
var
  Container: IProfileList;
begin
  if Assigned(This) then
    Container := This.Objects else
    Container := nil;
  Result := List.AddItem(Container, TProfileObject.Create());
end;

class function Document.NewOwner(const This: IProfileDocument): IProfileItem;
var
  Container: IProfileList;
begin
  if Assigned(This) then
    Container := This.Owners else
    Container := nil;
  Result := List.AddItem(Container, TProfileOwner.Create());
end;

class function Document.NewProperty(const This: IProfileObject; const Name: string): IProfileItem;
var
  Container: IProfileList;
begin
  if Assigned(This) then
    Container := This.Props else
    Container := nil;
  Result := List.AddItem(Container, TProfileProperty.Create(Name));
end;

class function Document.NewValue(const This: IProfileDocument; const Text, Name: string): IProfileItem;
var
  Container: IProfileList;
begin
  if Assigned(This) then
    Container := This.Values else
    Container := nil;
  Result := List.AddItem(Container, TProfileValue.Create(Name, Text));
end;

end.
