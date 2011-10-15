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

unit SilSmHttpTag;

interface

uses
  Sil,
  SilSiHttpTypes,
  SilSiHttpTags,
  SilSkHttpObject,
  SilSmHttpTags;

type
  TSilHttpTagClass = class of TSilHttpTag;
  TSilHttpTag = class(
    TSilHttpObject,
    IHttpTag )
  private
    FOwner: TSilHttpTagList;
    FId: THttpTag;
    FName: string;
  protected // IHttpTag
    function GetID: THttpTag;
    function GetKind: THttpTagKind;
    function GetName: string;
    function GetValue: Variant; virtual; abstract; 
    procedure SetValue(const Value: Variant); virtual; abstract;     
  protected
    procedure DoParseText(const Text: string); virtual; abstract;
  public
    constructor CreateNew(Owner: TSilHttpTagList; const Name, Value: string); reintroduce; virtual;
    class function Create(Owner: TSilHttpTagList; const Name, Value: string): IHttpTag; overload;
    class function Create(Owner: TSilHttpTagList; ID: THttpTag; const Value: string): IHttpTag; overload;
    destructor Destroy; override;
  end;

  TSilHttpTagVariant = class(TSilHttpTag)
  private
    FValue: Variant;
  protected
    procedure WriteTo(const Writer: IWriter); override; 
  protected
    function GetValue: Variant; override;
    procedure SetValue(const Value: Variant); override;     
    procedure DoParseText(const Text: string); override;
  end;

implementation

uses
  SilSfHttpParser, SilSfHttpTags,
  SilSgHttpTagName, SilSgHttpTagKind, SilBtVart;

{ TSilHttpTag }

class function TSilHttpTag.Create(Owner: TSilHttpTagList; const Name, Value: string): IHttpTag;
begin
  Result := Lookup(Name).CreateNew(Owner, Name, Value);
end;

class function TSilHttpTag.Create(Owner: TSilHttpTagList; ID: THttpTag; const Value: string): IHttpTag;
begin
  Result := Lookup(ID).CreateNew(Owner, GHttpTagName[ID], Value);
end;

constructor TSilHttpTag.CreateNew(Owner: TSilHttpTagList; const Name, Value: string);
begin
  inherited Create;
  FOwner := Owner;
  FName := Name;
  FId := HttpTagID(Name);
  DoParseText(Value);
end;

destructor TSilHttpTag.Destroy;
begin
  FOwner := nil;
  inherited;
end;

function TSilHttpTag.GetID: THttpTag;
begin
  Result := FId;
end;

function TSilHttpTag.GetKind: THttpTagKind;
begin
  Result := GHttpTagKind[FId];
end;

function TSilHttpTag.GetName: string;
begin
  Result := FName;
end;

{ TSilHttpTagVariant }

procedure TSilHttpTagVariant.DoParseText(const Text: string);
begin
  FValue := Sil.Vart.FromStr(Sil.Str.Trim(Text), Vart.Unassigned);
end;

function TSilHttpTagVariant.GetValue: Variant;
begin
  Result := FValue;
end;

procedure TSilHttpTagVariant.SetValue(const Value: Variant);
begin
  FValue := Value;
end;

procedure TSilHttpTagVariant.WriteTo(const Writer: IWriter);
begin
  Writer.WriteChar(ccSPC);
  Writer.WriteString(FValue);
end;

end.
 