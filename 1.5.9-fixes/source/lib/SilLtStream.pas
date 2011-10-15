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

unit SilLtStream;

{$I Defines.inc}

interface

uses
  SilLiStream,
  SilLjFiler,
  SilBkTool;

type
  IStream       = SilLiStream.IStream;
  IRandomStream = SilLiStream.IRandomStream;

type
  Stream = class(Tool)
    class function Buffer(const AStream: IRandomStream; BufferSize: Integer {$IFDEF USE_DEFPARAMS} = 4096 {$ENDIF} ): IRandomStream;
    class function Text(const Source: IStream = nil): ITextStream;
    class function RawFactory: FilerFactoryType; {$IFDEF USE_DEPRECATED} deprecated; {$ENDIF}
    class function TypeFactory: FilerFactoryType; {$IFDEF USE_DEPRECATED} deprecated; {$ENDIF}
    class function Raw: FilerFactoryType;
    class function Typed: FilerFactoryType;
    class function Memory(Size: Integer {$IFDEF USE_DEFPARAMS} = 0 {$ENDIF}; const Memory: Pointer {$IFDEF USE_DEFPARAMS} = nil {$ENDIF}): IMemoryStream; overload;
    class function Memory(const Text: string; ResetPosition: Boolean = True): IMemoryStream; overload;
    class function Store(const Source: IMemoryStream; const Text: string; ResetPosition: Boolean = True; Appending: Boolean = False): IMemoryStream;
    class function Page(const Source: IStream): IMemoryStream;
    class function WriteOnly: IWriteOnlyStream;
  end;

  MemoryStream = class(Tool)
    class function Create(Size: Integer {$IFDEF USE_DEFPARAMS} = 0 {$ENDIF}; const Memory: Pointer {$IFDEF USE_DEFPARAMS} = nil {$ENDIF}): IMemoryStream; overload;
    class function Create(const Text: string; ResetPosition: Boolean = True): IMemoryStream; overload;
    class function Store(const Source: IMemoryStream; const Text: string; ResetPosition: Boolean = True; Appending: Boolean = False): IMemoryStream;
  end;

implementation

uses
  SilLmStreamBuffer,
  SilLmMemoryStream,
  SilLmTextStream,
  SilLtRawFiler,
  SilLtTypeFiler,
  SilLmPageStream,
  SilLmWOStream;

{ Stream }

class function Stream.Buffer(const AStream: IRandomStream; BufferSize: Integer): IRandomStream;
begin
  Result := TStreamBuffer.Create(AStream, BufferSize);
end;

class function Stream.Text(const Source: IStream): ITextStream;
begin
  Result := TSilTextStream.Create(Source);
end;

class function Stream.Raw: FilerFactoryType;
begin
  Result := SilLtRawFiler.Factory
end;

class function Stream.Typed: FilerFactoryType;
begin
  Result := SilLtTypeFiler.Factory;
end;

{$IFDEF USE_DEPRECATED}
{$WARN SYMBOL_DEPRECATED OFF}
{$ENDIF}

class function Stream.RawFactory: FilerFactoryType;
begin
  Result := SilLtRawFiler.Factory;
end;

class function Stream.TypeFactory: FilerFactoryType;
begin
  Result := SilLtTypeFiler.Factory;
end;

{$IFDEF USE_DEPRECATED}
{$WARN SYMBOL_DEPRECATED ON}
{$ENDIF}

class function Stream.Memory(Size: Integer; const Memory: Pointer): IMemoryStream;
begin
  Result := TMemoryStream.Create(Memory, Size);
end;

class function Stream.WriteOnly: IWriteOnlyStream;
begin
  Result := TWOStream.Create;
end;

class function Stream.Memory(const Text: string; ResetPosition: Boolean): IMemoryStream;
begin
  Result := Memory(Length(Text), PChar(Text));
  if ResetPosition then Result.Position := 0;
end;

class function Stream.Store(const Source: IMemoryStream; const Text: string; ResetPosition, Appending: Boolean): IMemoryStream;
var
  Position: Integer;
begin
  if ResetPosition then
    Position := Source.Position else
    Position := 0;

  if not Appending then
  begin
    Source.Size := 0;
    Source.Position := 0;
  end;

  if Length(Text) > 0 then
    Source.Write(Text[1], Length(Text));

  if ResetPosition then
    Source.Position := Position;
end;

class function Stream.Page(const Source: IStream): IMemoryStream;
begin
  Result := TSilPageStream.Create(Source);
end;

{ MemoryStream }

{$IFDEF USE_DEPRECATED}
{$WARN SYMBOL_DEPRECATED OFF}
{$ENDIF}

class function MemoryStream.Create(Size: Integer; const Memory: Pointer): IMemoryStream;
begin
  Result := Stream.Memory(Size, Memory);
end;

class function MemoryStream.Create(const Text: string; ResetPosition: Boolean): IMemoryStream;
begin
  Result := Stream.Memory(Text, ResetPosition);
end;

class function MemoryStream.Store(const Source: IMemoryStream; const Text: string; ResetPosition, Appending: Boolean): IMemoryStream;
begin
  Result := Stream.Store(Source, Text, ResetPosition, Appending);
end;

{$IFDEF USE_DEPRECATED}
{$WARN SYMBOL_DEPRECATED ON}
{$ENDIF}

end.
