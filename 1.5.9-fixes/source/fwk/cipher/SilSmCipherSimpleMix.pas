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

unit SilSmCipherSimpleMix;

{$I Defines.inc}

interface

uses
  Sil,
  SilSiCipher,
  SilSiHash;

type
  TSimpleMix = class (
    // extends
    TSilInterfacedObject,
    // implements
    ICipher)
  private
    FKey: String;
  protected // ICipher
    function GetName: String;
    function GetId: TGuid;
    procedure Initialize(const Key: String; const Hash: IHash = nil);
    procedure Encode(const Source; out Dest: String; Size: LongWord);
    procedure Decode(const Source; out Dest: String; Size: LongWord);
  public
    constructor Create;
    class function Name: String;
  end;

implementation

const
  CId: TGuid = '{2E7219D9-B1AE-4B6B-BA12-91ABC2025F4D}';

{ TSimpleMix }

constructor TSimpleMix.Create;
begin
  inherited Create;
end;

procedure TSimpleMix.Encode(const Source; out Dest: String; Size: LongWord);
var
  Src: String;
begin
  SetLength(Src, Size);
  Move(Source, Src[1], Size);
  Str.Crypt(PChar(Src), Length(Src), FKey);
  Dest := Src;
end;

procedure TSimpleMix.Decode(const Source; out Dest: String; Size: LongWord);
var
  Src: String;
begin
  SetLength(Src, Size);
  Move(Source, Src[1], Size);
  Str.Crypt(PChar(Src), Length(Src), FKey);
  Dest := Src;
end;

function TSimpleMix.GetName: String;
begin
  Result := Name;
end;

procedure TSimpleMix.Initialize(const Key: String; const Hash: IHash);
begin
  FKey := Key;
end;

class function TSimpleMix.Name: String;
begin
  Result := 'SimpleMix';
end;

function TSimpleMix.GetId: TGuid;
begin
  Result := CId;
end;

end.
