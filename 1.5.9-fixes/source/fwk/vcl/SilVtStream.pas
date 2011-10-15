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

unit SilVtStream;

{$I Defines.inc}

interface

uses
  Sil,
  Classes;

type
  TStreamConvertionClass = class of TStreamConvertion;
  TStreamConvertion = class
    class function Create(const AStream: Sil.IStream): TStream;
  end;

implementation

type
  TDelphiStream = class (TStream, IUnknown)
  private
    FStream: Sil.IStream;
    FRandomStream: Sil.IRandomStream;
  private
    function GetStream: IStream;
    procedure SetStream(const Value: IStream);
  protected
    procedure SetSize(NewSize: Longint); override;
  public
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(Offset: Longint; Origin: Word): Longint; override;
  public
    constructor Create( const AStream: Sil.IStream );
    destructor Destroy; override;
  public
    property TheStream: Sil.IStream read GetStream write SetStream implements IUnknown;
  end;

{ TStreamConvertion }

class function TStreamConvertion.Create(const AStream: IStream): TStream;
begin
  Result := TDelphiStream.Create(AStream);
end;

{ TDelphiStream }

constructor TDelphiStream.Create(const AStream: IStream);
begin
  SetStream( AStream );
end;

destructor TDelphiStream.Destroy;
begin
  FRandomStream := nil;
  FStream := nil;
  inherited;
end;

procedure TDelphiStream.SetSize(NewSize: Integer);
begin
  if ( FRandomStream <> nil ) then
    FRandomStream.Size := NewSize;
end;

function TDelphiStream.GetStream: IStream;
begin
  result := FStream;
end;

procedure TDelphiStream.SetStream(const Value: IStream);
begin
  FStream := Value;
  Sil.Reference.GetInterface( Value, Sil.IRandomStream, FRandomStream );
end;

function TDelphiStream.Read(var Buffer; Count: Integer): Longint;
begin
  if ( FStream <> nil ) then
    result := FStream.Read( Buffer, Count ) else
    result := 0;
end;

function TDelphiStream.Write(const Buffer; Count: Integer): Longint;
begin
  if ( FStream <> nil ) then
    result := FStream.Write( Buffer, Count ) else
    result := 0;
end;

function TDelphiStream.Seek(Offset: Integer; Origin: Word): Longint;
const
  cv: array [ 0..2 ] of Sil.TSeekOrigin = ( Sil.soFromBeginning, Sil.soFromCurrent, Sil.soFromEnd );
begin
  result := 0;

  if ( FRandomStream <> nil ) then
    result := FRandomStream.Seek( Offset, cv[ Origin ] )
  else if ( FStream <> nil ) then
    case Origin of
    Classes.soFromBeginning: result := Offset;
    Classes.soFromCurrent: result := Offset;
    Classes.soFromEnd: result := Offset + Integer( FStream.Size );
    end;
end;

end.

