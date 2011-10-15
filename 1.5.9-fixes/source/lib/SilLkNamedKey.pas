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

unit SilLkNamedKey;

{$I Defines.inc}

interface

uses
  SilBeTypes,
  SilLiKey,
  SilLkNamedItem;

type
  TSilNamedKey = class (
    TSilNamedItem,
    INamedKey )
  private
    FParent: INamedKey;
    FKeys: INamedKeys;
    FValues: INamedValues;
    FIsNew: Boolean;
  protected
    function GetParentPath: string; virtual;
  protected // INamedKey
    function GetIsNew: Boolean; virtual;
    function GetParent: INamedKey; virtual;
    function GetPath: string; virtual;
    function GetKeys: INamedKeys; virtual;
    function GetValues: INamedValues; virtual;
    procedure Close; virtual;
    function Notify(const Sink: IUnknown; const Filters: TNamedKeyNotificationFilters = knChangeAny; const Subtree: Boolean = True; KeepRef: Boolean = True): INamedKey; virtual;  
  protected //- primitives
    procedure DoClose; virtual; abstract;
    function DoGetKeys: INamedKeys; virtual; abstract;
    function DoGetValues: INamedValues; virtual; abstract;
    procedure DoNotify(const Sink: IUnknown; const Filters: TNamedKeyNotificationFilters; const Subtree: Boolean; KeepRef: Boolean); virtual; abstract;   
  protected //- utils
    procedure FireChanged;
  public
    constructor Create; overload;
    constructor Create(const Parent: INamedKey; const Name: String; IsNew: Boolean); overload;
    destructor Destroy; override;
  end;

implementation

{ TSilNamedKey }

constructor TSilNamedKey.Create;
begin
  inherited Create('');
end;

constructor TSilNamedKey.Create(const Parent: INamedKey; const Name: String; IsNew: Boolean);
begin
  inherited Create(Name);
  FParent := Parent;
  FIsNew := IsNew;
end;

destructor TSilNamedKey.Destroy;
begin
  Close;
  inherited;
end;

function TSilNamedKey.GetParentPath: string;
begin
  if Assigned(FParent) then
    Result := FParent.Path else
    Result := '';
end;

function TSilNamedKey.GetIsNew: Boolean;
begin
  Result := FIsNew;
end;

function TSilNamedKey.GetParent: INamedKey;
begin
  Result := FParent;
end;

function TSilNamedKey.GetPath: string;
begin
  Result := GetParentPath + GetName;
end;

function TSilNamedKey.GetKeys: INamedKeys;
begin
  if FKeys = nil then FKeys := DoGetKeys();
  Result := FKeys;
end;

function TSilNamedKey.GetValues: INamedValues;
begin
  if FValues = nil then FValues := DoGetValues();
  Result := FValues;
end;

procedure TSilNamedKey.Close;
begin
  DoClose;
  FParent := nil;
end;

function TSilNamedKey.Notify(const Sink: IInterface; const Filters: TNamedKeyNotificationFilters; const Subtree: Boolean; KeepRef: Boolean): INamedKey;
begin
  DoNotify(Sink, Filters, Subtree, KeepRef);
  Result := Self;
end;

procedure TSilNamedKey.FireChanged;
begin
end;

end.
