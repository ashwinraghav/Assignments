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

unit SilOmTextFile;

{$I Defines.inc}

interface

uses
  SilLiStream,
  SilOiFile,
  SilOiTextFile,
  SilOsClasses;

type
  TTextFile = class (
    // extends
    TSilOsFile,
    // implements
    ITextFile,
    ITextStream)
  private
    FBuffer: String;
    FIndex: Integer;
    function DoReadChar(var Buffer: String; const Index: Integer): Boolean;
  protected // ITextFile
    function GetStream: ITextStream;
  protected // ITextStream
    function ReadStr(var Buffer: String): LongWord;
    function WriteStr(const Buffer: String): LongWord;
    function ReadLn(var Buffer: String): Boolean;
    function WriteLn(const Buffer: String): Boolean;
  public
    constructor OpenFile(const FileName: string; Access: TFileAccessMode; Share: TFileShareMode; MustExists: Boolean);
    constructor CreateFile(const FileName: string; Access: TFileAccessMode; Share: TFileShareMode; MustCreate: Boolean);
  end;

implementation

uses
  SilBcChr;

{ TTextFile }

constructor TTextFile.CreateFile(const FileName: string; Access: TFileAccessMode; Share: TFileShareMode; MustCreate: Boolean);
begin
  inherited CreateFile(FileName, Access, Share, MustCreate);
end;

constructor TTextFile.OpenFile(const FileName: string; Access: TFileAccessMode; Share: TFileShareMode; MustExists: Boolean);
begin
  inherited OpenFile(FileName, Access, Share, MustExists);
end;

function TTextFile.GetStream: ITextStream;
begin
  Result := Self;
end;

function TTextFile.DoReadChar(var Buffer: String; const Index: Integer): Boolean;
var
  iSize: Integer;
begin
  if (FIndex > Length(FBuffer)) or (FIndex < 1) then
  begin
    FIndex := 1;
    SetLength(FBuffer, 8192);
    iSize := Read(FBuffer[1], Length(FBuffer));
    if iSize <> Length(FBuffer) then SetLength(FBuffer, iSize);
  end;

  Result := Length(FBuffer) > 0;

  if Result then
  begin
    Buffer[Index] := FBuffer[FIndex];
    Inc(FIndex);
  end;
end;

function TTextFile.ReadLn(var Buffer: String): Boolean;
const
  FindEol: array [0..1] of Char = (ccCR, ccLF);
var
  i: Integer;
  bFound: Byte;
begin
  i := 1;
  bFound := 0;
  SetLength(Buffer, 1024);

  while DoReadChar(Buffer, i) do
  begin
    if Buffer[i] = FindEol[bFound] then
    begin
      Inc(bFound);
      if bFound > 1 then
      begin
        SetLength(Buffer, i - 2);
        Result := true;
        Exit;
      end;
    end else
    if bFound > 0 then bFound := 0;

    if i >= Length(Buffer) then SetLength(Buffer, i + 1024);
    Inc(i);
  end;

  if i > 0 then SetLength(Buffer, i - 1);
  Result := false;
end;

function TTextFile.ReadStr(var Buffer: String): LongWord;
begin
  Result := Read(Buffer[1], Length(Buffer));
end;

function TTextFile.WriteLn(const Buffer: String): Boolean;
begin
  Result := WriteStr(Buffer + ccCRLF) > 0;
end;

function TTextFile.WriteStr(const Buffer: String): LongWord;
begin
  Result := Write(Buffer[1], Length(Buffer));
end;

end.
