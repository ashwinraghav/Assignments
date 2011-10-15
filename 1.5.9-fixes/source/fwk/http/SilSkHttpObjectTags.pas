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

unit SilSkHttpObjectTags;

interface

uses
  Sil,
  SilSiHttpTypes,
  SilSiHttpTags;

type
  TSilHttpObjectTags = class(
    TSilObject,
    IHttpObjectTags)
  private
    FList: IHttpTagList;
  protected
    procedure DoCheck(Field, Result: PUnknown; const Tag: THttpTag; const IID: TGUID);
  protected //  IHttpObjectTags
    function GetList: IHttpTagList;
  public
    constructor Create(const List: IHttpTagList);
    destructor Destroy; override;
    property List: IHttpTagList read FList;   
  end;

implementation

{ TSilHttpObjectTags }

constructor TSilHttpObjectTags.Create(const List: IHttpTagList);
begin
  inherited Create;
  FList := List;  
end;

destructor TSilHttpObjectTags.Destroy;
begin
  FList := nil;
  inherited;
end;

procedure TSilHttpObjectTags.DoCheck(Field, Result: PUnknown; const Tag: THttpTag; const IID: TGUID);
begin
  if not Assigned(Field^) and not FList.Find(IID, Field^) then
    if not (  (FList.Add(Tag).QueryInterface(IID, Field^) = 0)
          or  (FList.Add(Tag).QueryInterface(IHttpTag, Field^) = 0)) then
      raise Sil.Error.Create('El tag solicitado no soporta la interface pedida.');
  Result^ := Field^;
end;

function TSilHttpObjectTags.GetList: IHttpTagList;
begin
  Result := FList;
end;

end.
 