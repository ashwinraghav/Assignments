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

unit SilSmHttpEntity;

{$I Defines.inc}

interface

uses
  Sil,
  SilTokens,
  SilLexer,
  SilSkHttpObject,
  SilSiHttpTypes,
  SilSiHttpData,
  SilSiHttpTags;

type
  TSilHttpEntity = class(
    TSilHttpObject,
    IHttpEntity )
  private
    FTags: IHttpEntityTags;
    FStream: IBufferStream;
  protected // IHttpEntity
    function GetEntityTags: IHttpEntityTags;
    function GetStream: IMemoryStream;
  protected
    procedure DoReadFrom(const Stream: IStream);
    procedure WriteTo(const Writer: IWriter); override; 
  public
    constructor Create(const Lexer: ITokenLexer; const Tags: IHttpTagList); reintroduce; overload;
    constructor Create(const Tags: IHttpTagList); reintroduce; overload;
    destructor Destroy; override;
  end;

implementation

uses
  SilSmHttpEntityTags;

{ TSilHttpEntity }

constructor TSilHttpEntity.Create(const Lexer: ITokenLexer; const Tags: IHttpTagList);
begin
  inherited;
  FTags := TSilHttpEntityTags.Create(Tags);
  FStream := Sil.Tk.BufferStream();
  DoReadFrom(Lexer.Stream);
end;

constructor TSilHttpEntity.Create(const Tags: IHttpTagList);
begin
  inherited Create;
  FTags := TSilHttpEntityTags.Create(Tags);
  FStream := Sil.Tk.BufferStream();
end;

destructor TSilHttpEntity.Destroy;
begin
  FStream := nil;
  FTags := nil;
  inherited;
end;

function TSilHttpEntity.GetEntityTags: IHttpEntityTags;
begin
  Result := FTags;
end;

function TSilHttpEntity.GetStream: IMemoryStream;
begin
  Result := FStream;
end;

procedure TSilHttpEntity.DoReadFrom(const Stream: IStream);
var
  Length, Size, Readen: LongWord;
begin
  if FTags.List.IsPresent(httContentLength) then
    Length := FTags.ContentLength.Value else
    Length := 0;

  FStream.Position := 0;
  FStream.Truncate;
  
  if Length > 0 then
    FStream.Size := Length else
    FStream.Size := Stream.Size;

  repeat
  
    if FStream.Remaining > 0 then
    begin
      Readen := Stream.Read(FStream.Current^, FStream.Remaining);
      FStream.Position := FStream.Position + Readen;
    end else
      Readen := 0;

    if (Length = 0) or (Readen < Length) then
    begin
      if (Length = 0) and (Readen > 0) then
      begin
        if FStream.Remaining = 0 then
        begin
          Size := Stream.Size;
          if (Size = 0) then
            Size := 1024;
          FStream.Size := FStream.Size + Size;
        end;
      end;
    end;

    if (Readen > 0) then
      Size := FStream.Remaining else
      Size := 0;

  until Size = 0;
  
  FStream.Position := 0;
end;

procedure TSilHttpEntity.WriteTo(const Writer: IWriter);
begin
  if FStream.Size > 0 then
    Writer.Write(FStream.Memory^, FStream.Size);
end;

end.
