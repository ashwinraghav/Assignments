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

unit InterfaceProperty;

{$INCLUDE Defines.inc}

interface

uses
  Classes,
{$IFDEF D60}
  DesignIntf,
  DesignEditors,
{$ELSE}
  DsgnIntf,
{$ENDIF}
  TypInfo, Dialogs, SysUtils,
  Sil;

type
  //
  // Editor de propiedades para la interface
  //
  TSInterfaceEditor = class( TPropertyEditor )
  protected
    function GUID: TGUID; virtual; abstract;

  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
    function GetValue: string; override;
    procedure SetValue(const Value: string); override;
  end;

  //
  // Componente que tiene propiedades de tipo interface
  //
  TSIntfPropComp = class
    class function FindPropComp(Parent: TComponent; IID: TGUID;
      PropValue: Integer): TComponent;
  end;


implementation


{ TSInterfaceEditor }

function TSInterfaceEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [ paValueList{, paMultiSelect}, paRevertable ];
end;

function TSInterfaceEditor.GetValue: string;
var
  comp: TComponent;
begin
  comp := TComponent( GetOrdValue );
  if ( comp <> nil ) then
    result := comp.Name
  else
    result := '';
end;

procedure TSInterfaceEditor.GetValues(Proc: TGetStrProc);
{$IFNDEF D60}
var
  i1: Integer;
  comp: TComponent;
  intf: IUnknown;
{$ENDIF}
begin
{$IFNDEF D60}
  for i1 := 0 to Designer.ContainerWindow.ComponentCount - 1 do
  begin
    comp := Designer.ContainerWindow.Components[ i1 ];
    comp.GetInterface( GUID, intf );
    if ( intf <> nil ) then
    begin
      Proc( comp.Name );
    end;
  end;
{$ENDIF}
end;

procedure TSInterfaceEditor.SetValue(const Value: string);
begin
{$IFNDEF D60}
  SetOrdValue( Integer( Designer.ContainerWindow.FindComponent( Value ) ) );
{$ENDIF}
end;

{ TSIntfPropComp }

class function TSIntfPropComp.FindPropComp( Parent: TComponent; IID: TGUID;
  PropValue: LongInt ): TComponent;
var
  i1: Integer;
  comp: TComponent;
  intf: IUnknown;
begin
  result := nil;
  for i1 := 0 to Parent.ComponentCount - 1 do
  begin
    comp := Parent.Components[ i1 ];
    comp.GetInterface( IID, intf );
    if Reference.SameObject( intf, IUnknown( PropValue ), IID ) then
    begin
      result := comp;
      break;
    end;
  end;
end;


//
// Código utilizado en la versión donde la property es una interface en lugar
// de un TComponent
//
(*constructor TSIntfPropComp.Create( const Comp: TComponent; const IID: TGUID;
  Filer: TFiler; PropName: string; HasData: Boolean );
begin
  fComp := Comp;
  fIID := IID;
  fPropInfo := GetPropInfo( Comp, PropName );
  Filer.DefineProperty( fPropInfo^.Name + 'Value', ReadData, WriteData, HasData );
end;

procedure TSIntfPropComp.ReadData(Reader: TReader);
var
  i1: Integer;
  s1: string;
  comp: TComponent;
  intf: IUnknown;
begin
  s1 := Reader.ReadString;

  if ( fComp.Owner <> nil ) then
  begin
    comp := fComp.Owner.FindComponent( s1 );
    if ( comp <> nil ) then
    begin
      comp.GetInterface( fIID, intf );
      SetOrdProp( fComp, fPropInfo, Integer( intf ) );
    end;
  end;

  if ( fComp.Owner <> nil ) then
  begin
    s1 := '';
    for i1 := 0 to fComp.Owner.ComponentCount - 1 do
    begin
      if ( i1 > 0 ) then s1 := s1 + '; ';
      s1 := s1 + fComp.Owner.Components[ i1 ].Name;
    end;
    if ( comp <> nil ) then
      ShowMessage( 'Componente encontrado en: ' + s1 )
    else
      ShowMessage( 'Componente no encontrado en: ' + s1 );
  end;
end;

procedure TSIntfPropComp.WriteData( Writer: TWriter );
var
  comp: TComponent;
begin
  comp := FindPropComp( fComp.Owner, fIID, GetOrdProp( fComp, fPropInfo ) );
  if ( comp <> nil ) then
    Writer.WriteString( comp.Name )
  else
    Writer.WriteString( '' );
end; *)



end.

