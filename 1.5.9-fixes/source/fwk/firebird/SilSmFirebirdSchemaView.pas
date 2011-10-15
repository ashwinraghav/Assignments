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

unit SilSmFirebirdSchemaView;

{$INCLUDE Defines.inc}

interface

uses
  Sil,
  SilSiFirebird,
  SilShFirebird,
  SilSmFirebirdSchemaRelation,
  SilSkFirebirdSchemaObject,
  SilSkFirebirdSchemaFields;
  
type
  TSilFirebirdSchemaViews = class(
    TSilFirebirdSchemaRelations,
    IFbSchemaViews )
  private
    FFields: IUnknown;
  protected
    function DoGetFields: IUnknown; override;
  protected // IFbSchemaViews
    function GetViewFields: IFbSchemaView;
    function Enumerate(var Enum: IEnumerator; out Fields: IFbSchemaView): Boolean; overload;
  public
    destructor Destroy; override;
  end;

type
  TSilFirebirdSchemaView = class(
    TSilFirebirdSchemaRelation,
    IFbSchemaView )
  protected // IFbSchemaView
    function GetViewBlr: IFbData;              // VIEW_BLR
    function GetViewSource: IFbData;           // VIEW_SOURCE
  public
    destructor Destroy; override;
  end;

implementation

uses
  SilSgFirebirdSchemaQueries;

const
    CSysIndexViewViewBlr              =  13;
    CSysIndexViewViewSource           =  14;

{ TSilFirebirdSchemaViews }

destructor TSilFirebirdSchemaViews.Destroy;
begin
  FFields := nil;
  inherited;
end;

function TSilFirebirdSchemaViews.GetViewFields: IFbSchemaView;
begin
  if not Assigned(FFields) then FFields := TSilFirebirdSchemaView.Create(Self);
  Result := FFields as IFbSchemaView;
end;

function TSilFirebirdSchemaViews.DoGetFields: IUnknown;
begin
  Result := GetViewFields();
end;

function TSilFirebirdSchemaViews.Enumerate(var Enum: IEnumerator; out Fields: IFbSchemaView): Boolean;
begin
  Result := inherited Enumerate(Enum, Fields);
end;

{ TSilFirebirdSchemaView }

destructor TSilFirebirdSchemaView.Destroy;
begin
  inherited;
end;

function TSilFirebirdSchemaView.GetViewBlr: IFbData;
begin
  Result := DoCheck(CSysIndexViewViewBlr, CSysFieldRelationViewBlr);
end;

function TSilFirebirdSchemaView.GetViewSource: IFbData;
begin
  Result := DoCheck(CSysIndexViewViewSource, CSysFieldRelationViewSource);
end;

end.
 