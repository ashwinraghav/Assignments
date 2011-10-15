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

unit SilVtComponents;

{$I Defines.inc}

interface

uses
  Classes, Sil;

type
  ComponentClass = class of ComponentTool;
  ComponentTool = class(Tool)
  public
    class function FindGlobal(ClassType: TComponentClass; const Name: string; out Component): Boolean;
    class function FindChild(Instance: TComponent; ClassType: TComponentClass; const Name: string; out Component): Boolean;
    class function FindChildByIntf(Instance: TComponent; const IID: TGUID; const Value: IUnknown): TComponent;
    class function FindForm(ClassType: TComponentClass; const Name: string; out Form): Boolean;
    class function FindModule(ClassType: TComponentClass; const Name: string; out Module): Boolean;
    class function ID(const Prefix: string; ID: Integer): string;
    class function Name(Instance: TComponent; const Default: string = ''): string;
    class function SetReference(Instance, Value: TObject; const Name: string; Overwrite: Boolean = True): Pointer;
    class function GetReference(Instance: TObject; const Name: string): Pointer;
    class procedure Enable(OnOff: Boolean; const Components: array of TComponent; Recursive: Boolean = False); overload;
    class procedure Enable(OnOff: Boolean; Component: TComponent; Recursive: Boolean = True); overload;
    class procedure SetProp(const PropName: string; OnOff: Boolean; const Components: array of TComponent; Recursive: Boolean = False); overload;
    class procedure SetProp(const PropName: string; OnOff: Boolean; Component: TComponent; Recursive: Boolean = True); overload;
    class procedure Value(const PropName: string; Value1, Value2: Variant; const Components: array of TComponent; Recursive: Boolean = False); overload;
    class procedure Value(const PropName: string; const Value: Variant; const Components: array of TComponent; Recursive: Boolean = False); overload;
    class procedure Value(const PropName: string; const Value: Variant; Component: TComponent; Recursive: Boolean = True); overload;
    class procedure SetProp(const PropName: string; const Value: Variant; const Components: array of TComponent; Recursive: Boolean = False); overload; 
    class function ChildCount(Instance: TComponent): integer;
    class function GetChild(Instance: TComponent; Index: integer; out Component): boolean;
  end;

implementation

uses
  Controls, Forms, TypInfo;

{ ComponentTool }

class function ComponentTool.FindGlobal(ClassType: TComponentClass; const Name: string; out Component): Boolean;
var
  IsModule: Boolean;
  IsForm: Boolean;
begin
  Result := False;

  IsModule := (ClassType = nil) or ClassType.InheritsFrom(TDataModule);
  IsForm   := (ClassType = nil) or ClassType.InheritsFrom(TForm);

  if not Result and IsForm then
    Result := Self.FindForm  (ClassType, Name, Component);

  if not Result and IsModule then
    Result := Self.FindModule(ClassType, Name, Component);

end;

class function ComponentTool.FindChild(Instance: TComponent; ClassType: TComponentClass; const Name: string; out Component): Boolean;
var
  I: Integer;
  C: TComponent;
begin
  Result := False;
  for i := 0 to Instance.ComponentCount - 1 do
  begin
    C := Instance.Components[i];
    if ( (ClassType = nil) or C.InheritsFrom(ClassType) )  and
       ( Str.IsEmpty(Name) or Str.IsEqual(C.Name, Name) )  then
      begin
        TComponent(Component) := C;
        Result := True;
        Break;
      end;
  end;
end;

class function ComponentTool.FindChildByIntf(Instance: TComponent; const IID: TGUID; const Value: IInterface): TComponent;
var
  Index: Integer;
  Comp: TComponent;
  Intf: IUnknown;
begin
  Result := nil;
  for Index := 0 to Instance.ComponentCount - 1 do
  begin
    Comp := Instance.Components[Index];
    if Comp.GetInterface(IID, Intf) and Sil.Ref.SameObject(Intf, Value, IID) then
      begin
        Result := Comp;
        Break;
      end;
  end;
end;

class function ComponentTool.FindForm(ClassType: TComponentClass; const Name: string; out Form): Boolean;
var
  I: Integer;
  F: TForm;
begin
  Result := False;
  for I := 0 to Screen.FormCount - 1 do
  begin
    F := Screen.Forms[I];
    if ( (ClassType = nil) or F.InheritsFrom(ClassType) )  and
       ( Str.IsEmpty(Name) or Str.IsEqual(F.Name, Name) )  then
      begin
        TForm(Form) := F;
        Result := True;
        Exit;
      end;
  end;
end;

class function ComponentTool.FindModule(ClassType: TComponentClass; const Name: string; out Module): Boolean;
var
  I: Integer;
  M: TComponent;
begin
  Result := False;
  for i := 0 to Screen.DataModuleCount - 1 do
  begin
    M := Screen.DataModules[i];
    if ( (ClassType = nil) or M.InheritsFrom(ClassType) )  and
       ( Str.IsEmpty(Name) or Str.IsEqual(M.Name, Name) )  then
      begin
        TComponent(Module) := M;
        Result := True;
        Break;
      end;
  end;
end;

class function ComponentTool.ID(const Prefix: string; ID: Integer): string;
begin
  Result := Prefix + Int.ToHex(ID, 8);
end;

class function ComponentTool.Name(Instance: TComponent; const Default: string): string;
begin
  if Assigned(Instance) then
    Result := Instance.Name else
    Result := Default;
end;

class function ComponentTool.SetReference(Instance, Value: TObject; const Name: string; Overwrite: Boolean): Pointer;
var
  Field: ^TObject;
begin
  Field := Instance.FieldAddress(Name);
  if (Field <> nil) and (Overwrite or not Assigned(Field^)) then
  begin
    Result := Field^;
    Field^ := Value;
  end else
    Result := nil;
end;

class function ComponentTool.GetReference(Instance: TObject; const Name: string): Pointer;
var
  Field: ^TObject;
begin
  Field := Instance.FieldAddress(Name);
  if Field <> nil then
    Result := Field^ else
    Result := nil;
end;

class procedure ComponentTool.Enable(OnOff: Boolean; const Components: array of TComponent; Recursive: Boolean);
begin
  Self.Value('Enabled', OnOff, not OnOff, Components, Recursive);
end;

class procedure ComponentTool.Enable(OnOff: Boolean; Component: TComponent; Recursive: Boolean);
begin
  Self.Value('Enabled', OnOff, Component, Recursive);
end;

class procedure ComponentTool.SetProp(const PropName: string; OnOff: Boolean; const Components: array of TComponent; Recursive: Boolean);
begin
  Self.Value(PropName, OnOff, not OnOff, Components, Recursive);
end;

class procedure ComponentTool.SetProp(const PropName: string; OnOff: Boolean; Component: TComponent; Recursive: Boolean);
begin
  Self.Value(PropName, OnOff, Component, Recursive);
end;

class procedure ComponentTool.Value(const PropName: string; Value1, Value2: Variant; const Components: array of TComponent; Recursive: Boolean);
var
  I: Integer;
  Comp: TComponent;
begin
  for I := Low(Components) to High(Components) do
  begin
    Comp := Components[I];
    if Comp <> nil then
      Self.Value(PropName, Value1, Comp, Recursive) else
      Sil.Vart.Swap(Value1, Value2);
  end;
end;

class procedure ComponentTool.Value(const PropName: string; const Value: Variant; const Components: array of TComponent; Recursive: Boolean);
begin
  Self.Value(PropName, Value, Value, Components, Recursive);
end;

class procedure ComponentTool.Value(const PropName: string; const Value: Variant; Component: TComponent; Recursive: Boolean);
var
  I: Integer;
  Parent: TWinControl;
  Child: TControl;
begin
  if Recursive and Component.InheritsFrom(TWinControl) then
  begin
    Parent := TWinControl(Component);
    for I := 0 to Parent.ControlCount - 1 do
    begin
      Child := Parent.Controls[I];
      Self.Value(PropName, Value, Child, Recursive);
    end;
  end;
  if Typ.Prop.Exists(Component, PropName) then
    Typ.Prop.Value(Component, PropName, Value);
end;

class procedure ComponentTool.SetProp(const PropName: string; const Value: Variant; const Components: array of TComponent; Recursive: Boolean);
begin
  Self.Value(PropName, Value, Components, Recursive);
end;

class function ComponentTool.ChildCount(Instance: TComponent): integer;
begin
  result := Instance.ComponentCount;
end;

class function ComponentTool.GetChild(Instance: TComponent; Index: integer;
  out Component): boolean;
begin
  result := ( 0 <= Index ) and ( Index < Instance.ComponentCount );
  if result then
    TComponent(Component) := Instance.Components[ Index ];
end;

end.

