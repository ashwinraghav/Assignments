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

unit SilVmStreamWrapper;

{$I Defines.inc}

interface

uses
  Classes,
  Sil;

type                      
  TStreamWrapper = class(TSilInterfacedObject, IRandomStream)
  private
    FStream: TStream;
    FMustFree: Boolean;
  private
    function GetPosition: LongWord;
    procedure SetPosition(Pos: LongWord);
    function GetSize: LongWord;
    procedure SetSize(NewSize: LongWord);
    function Read(var Buffer; Count: LongWord): LongWord;
    function Write(const Buffer; Count: LongWord): LongWord;
    function Seek(Offset: Integer; Origin: TSeekOrigin): LongWord;
    procedure Truncate;
  public
    constructor Create(AStream: TStream; MustFree: Boolean {$IFDEF USE_DEFPARAMS} = True {$ENDIF});
    destructor Destroy; override;
  end;

implementation

/////////////////////////////////////////////////////////////////////////////////////////////

constructor TStreamWrapper.Create(AStream: TStream; MustFree: Boolean);
begin
  inherited Create;
  FStream := AStream;
  FMustFree := MustFree;
end;

destructor TStreamWrapper.Destroy;
begin
  if FMustFree then Reference.Free(FStream);
  inherited;
end;

function TStreamWrapper.GetPosition: LongWord;
begin
  if Assigned(FStream) then
    Result := FStream.Position else
    Result := 0;
end;

procedure TStreamWrapper.SetPosition(Pos: LongWord);
begin
  if Assigned(FStream) then
    FStream.Position := Pos;
end;

function TStreamWrapper.GetSize: LongWord;
begin
  if Assigned(FStream) then
    Result := FStream.Size else
    Result := 0;
end;

procedure TStreamWrapper.SetSize(NewSize: LongWord);
begin
  if Assigned(FStream) then
    FStream.Size := NewSize;
end;

function TStreamWrapper.Read(var Buffer; Count: LongWord): LongWord;
begin
  if Assigned(FStream) then
    Result := FStream.Read(Buffer, Count) else
    Result := 0;
end;

function TStreamWrapper.Write(const Buffer; Count: LongWord): LongWord;
begin
  if Assigned(FStream) then
    Result := FStream.Write(Buffer, Count) else
    Result := 0;
end;

function TStreamWrapper.Seek(Offset: Integer; Origin: TSeekOrigin): LongWord;
begin
  if Assigned(FStream) then
    Result := FStream.Seek(Offset, Ord(Origin)) else
    Result := 0;
end;

procedure TStreamWrapper.Truncate;
begin

end;

end.
 