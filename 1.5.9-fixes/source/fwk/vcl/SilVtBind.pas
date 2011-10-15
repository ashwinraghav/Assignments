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

unit SilVtBind;

interface

uses
  Classes, ComCtrls,

  Sil;

type
  TPropertyAccessModes = ( pamDefaultClear, pamHideZeros );
  TPropertyAccessMode = set of TPropertyAccessModes;

  PropertyBindClass = class of PropertyBindTool;
  PropertyBindTool = class
    class function GetProperties(
                      Container: TComponent;
                const ObjPrefix: string;
                const Prop: string ): IValueList;

    class function SetProperties(
                      Container: TComponent;
                const ObjPrefix: string;
                const Prop: string;
                      Values: IValueList;
                      AMode: TPropertyAccessMode = [] ): integer;

    class function PropertyAccess(
                      Instance: TComponent;
                const Prop: string;
                const Name: string;
                      AValue: IFieldAccess = nil;
                      AMode: TPropertyAccessMode = [] ): IFieldAccess;
  end;

implementation

uses
  TypInfo,
  SilClasses,
  SilVCL;

{ Bind }

class function PropertyBindTool.GetProperties(
        Container: TComponent;
  const ObjPrefix: string;
  const Prop: string ): IValueList;
var
  i1, i2, cnt: integer;
  s1: string;
  comp: TComponent;
  pa: IFieldAccess;
begin
  result := TValueList.Create(true);
  cnt := Vcl.Comp.ChildCount( Container );
  i2 := Length( ObjPrefix );
  for i1 := 0 to cnt - 1 do
  begin
    if Vcl.Comp.GetChild( Container, i1, comp ) and
      ( ( i2 = 0 ) or ( Length( comp.Name ) >= i2 ) and
        ( Text.Compare( comp.Name, ObjPrefix, i2 ) = 0 ) ) then
    begin
      s1 := Str.Right( comp.Name, Length( comp.Name ) - i2 );
      pa := PropertyAccess( comp, Prop, s1 );
      if ( pa <> nil ) then
        result.Add( pa );
    end;
  end;
end;

class function PropertyBindTool.SetProperties(
        Container: TComponent;
  const ObjPrefix: string;
  const Prop: string;
        Values: IValueList;
        AMode: TPropertyAccessMode ): integer;
var
  i1, i2, cnt: integer;
  s1: string;
  comp: TComponent;
  psrc: IFieldAccess;
begin
  result := 0;
  cnt := Vcl.Comp.ChildCount( Container );
  i2 := Length( ObjPrefix );
  for i1 := 0 to cnt - 1 do
  begin
    if Vcl.Comp.GetChild( Container, i1, comp ) and
      ( ( i2 = 0 ) or ( Length( comp.Name ) >= i2 ) and
        ( Text.Compare( comp.Name, ObjPrefix, i2 ) = 0 ) ) then
    begin
      s1 := Str.Right( comp.Name, Length( comp.Name ) - i2 );
      if ( Values <> nil ) then
        psrc := Values.Items[ s1 ] else psrc := nil;
      if ( psrc <> nil ) or ( pamDefaultClear in AMode ) then
        PropertyAccess( comp, Prop, s1, psrc, AMode );
    end;
  end;
end;

class function PropertyBindTool.PropertyAccess(
        Instance: TComponent;
  const Prop: string;
  const Name: string;
        AValue: IFieldAccess;
        AMode: TPropertyAccessMode ): IFieldAccess;
var
  info: PPropInfo;
  musthide: boolean;
begin
  if ( Instance <> nil ) then
  begin
    info := GetPropInfo( Instance.ClassType, Prop );

    case info^.PropType^.Kind of
    tkInteger:
      begin
        result := TFieldAccess.CreateTyped( Name, ftInteger );
        result.AsInteger := GetPropValue( Instance, Prop, false );
      end;
    tkFloat:
      begin
        result := TFieldAccess.CreateTyped( Name, ftFloat );
        result.AsFloat := GetPropValue( Instance, Prop, false );
      end;
    tkLString, tkString:
      begin
        result := TFieldAccess.CreateTyped( Name, ftString );
        result.AsString := GetPropValue( Instance, Prop );
      end;
    else result := nil;
    end;

    if ( AValue <> nil ) then
      case info^.PropType^.Kind of
      tkInteger:
        SetPropValue( Instance, Prop, AValue.AsInteger );
      tkFloat:
        SetPropValue( Instance, Prop, AValue.AsFloat );
      tkLString, tkString:
        begin
          // Determina si debe ocultar el valor
          musthide := false;
          if ( pamHideZeros in AMode ) and ( AValue <> nil ) then
            case AValue.DataType of
            ftSmallInt, ftInteger, ftByte, ftWord:
              musthide := ( AValue.AsInteger = 0 );
            ftLargeInt, ftLongWord:
              musthide := ( AValue.AsLongWord = 0 );
            ftFloat, ftCurrency, ftDate, ftDateTime:
              musthide := ( AValue.AsFloat = 0 );
            end;

          if musthide then
            SetPropValue( Instance, Prop, '' ) else
            SetPropValue( Instance, Prop, AValue.AsString );
        end;
      end
    else if ( pamDefaultClear in AMode ) then
      case info^.PropType^.Kind of
      tkInteger: SetPropValue( Instance, Prop, 0 );
      tkFloat: SetPropValue( Instance, Prop, 0 );
      tkLString, tkString: SetPropValue( Instance, Prop, '' );
      end;
  end
  else
    result := nil;
end;

end.

