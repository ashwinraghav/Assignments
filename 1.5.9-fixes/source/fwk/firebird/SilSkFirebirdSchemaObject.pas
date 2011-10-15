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

unit SilSkFirebirdSchemaObject;

{$INCLUDE Defines.inc}

interface

uses
  Sil,
  SilSiFirebird,
  SilShFirebird,
  SilSmFirebirdCursor;

type
  TSilFirebirdSchemaObject = class(
    TSilObject,
    IFbSchemaObjectInternal )
  private
    FCursor: IFbCursorInternal;
  protected
  protected // IFbSchemaObject
    function GetCursor: IFbCursor;
    function Enumerate(var Enum: IEnumerator; out Fields): Boolean; overload;
  protected // IFbSchemaObjectInternal
    function DoGetCursor: IFbCursorInternal;
    function DoGetFields: IUnknown; virtual; abstract;
  protected
    property Cursor: IFbCursorInternal read FCursor;
  public
    constructor Create(const Cursor: IFbCursor);
    destructor Destroy; override;
  end;

implementation

{ TSilFirebirdSchemaObject }

constructor TSilFirebirdSchemaObject.Create(const Cursor: IFbCursor);
begin
  inherited Create;
  FCursor := IFbCursorInternal(Cursor);
end;

destructor TSilFirebirdSchemaObject.Destroy;
begin
  FCursor := nil;
  inherited;
end;

function TSilFirebirdSchemaObject.GetCursor: IFbCursor;
begin
  Result := FCursor;
end;

function TSilFirebirdSchemaObject.Enumerate(var Enum: IEnumerator; out Fields): Boolean;
var
  Buffer: IFbBuffer;
begin
  Result := Cursor.Enumerate(Enum, Buffer);
  if Result then IUnknown(Fields) := DoGetFields();
end;

function TSilFirebirdSchemaObject.DoGetCursor: IFbCursorInternal;
begin
  Result := FCursor;
end;

end.
