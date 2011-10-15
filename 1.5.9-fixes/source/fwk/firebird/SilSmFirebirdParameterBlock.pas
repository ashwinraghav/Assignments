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

unit SilSmFirebirdParameterBlock;

{$INCLUDE Defines.inc}

interface

uses
  Sil, 
  SilSiFirebird,
  SilShFirebird;

type
  TSilFirebirdParameterBlock = class(
    TSilFirebirdBlockType,
    IFbParamBlock )
  protected // IFbParamBlock
    procedure AddBoolean(Info: PFbParameterInfo; var Buffer: string; const Value: Boolean); virtual; 
    function GetBoolean(Info: PFbParameterInfo; var Buffer: PChar): Boolean; virtual;
    procedure AddByte(Info: PFbParameterInfo; var Buffer: string; const Value: Byte); virtual;
    function GetByte(Info: PFbParameterInfo; var Buffer: PChar): Byte; virtual;
    procedure AddWord(Info: PFbParameterInfo; var Buffer: string; const Value: Word); virtual;
    function GetWord(Info: PFbParameterInfo; var Buffer: PChar): Word; virtual;
    procedure AddInteger(Info: PFbParameterInfo; var Buffer: string; const Value: Integer); virtual;
    function GetInteger(Info: PFbParameterInfo; var Buffer: PChar): Integer; virtual;
    procedure AddString(Info: PFbParameterInfo; var Buffer: string; const Value: String); virtual;
    function GetString(Info: PFbParameterInfo; var Buffer: PChar): String; virtual;
  public
    constructor Create(Dummy: Integer); override;
  end;

type
  TSilFirebirdServiceRequestBuffer = class(TSilFirebirdParameterBlock)
  protected // IFbParamBlock
    procedure AddBoolean(Info: PFbParameterInfo; var Buffer: string; const Value: Boolean); override;  
    function GetBoolean(Info: PFbParameterInfo; var Buffer: PChar): Boolean; override;
    procedure AddByte(Info: PFbParameterInfo; var Buffer: string; const Value: Byte); override;
    function GetByte(Info: PFbParameterInfo; var Buffer: PChar): Byte; override;
    procedure AddWord(Info: PFbParameterInfo; var Buffer: string; const Value: Word); override;
    function GetWord(Info: PFbParameterInfo; var Buffer: PChar): Word; override;
    procedure AddInteger(Info: PFbParameterInfo; var Buffer: string; const Value: Integer); override;
    function GetInteger(Info: PFbParameterInfo; var Buffer: PChar): Integer; override;
    procedure AddString(Info: PFbParameterInfo; var Buffer: string; const Value: String); override;
    function GetString(Info: PFbParameterInfo; var Buffer: PChar): String; override; 
  end;

implementation

uses SilSdFirebird;

{ TSilFirebirdParameterBlock }

constructor TSilFirebirdParameterBlock.Create(Dummy: Integer); 
begin
  inherited Create;
end;

procedure TSilFirebirdParameterBlock.AddBoolean(Info: PFbParameterInfo; var Buffer: string; const Value: Boolean);
begin
  if Value then Sil.Str.Add(Buffer, Char(Info.Code));
end;

function TSilFirebirdParameterBlock.GetBoolean(Info: PFbParameterInfo; var Buffer: PChar): Boolean;
begin
  Inc(Buffer);
  Result := True;
end;

procedure TSilFirebirdParameterBlock.AddByte(Info: PFbParameterInfo; var Buffer: string; const Value: Byte);
begin
  Sil.Str.Add(Buffer, Char(Info.Code));
  Sil.Str.Append(Buffer, Byte(SizeOf(Value)));
  Sil.Str.Append(Buffer, Value);
end;

function TSilFirebirdParameterBlock.GetByte(Info: PFbParameterInfo; var Buffer: PChar): Byte;
begin
  Inc(Buffer);
  Sil.Error.Check(Byte(Buffer^) = SizeOf(Result), SAssertIncorrectSize, [Info.Name^, Byte(Buffer^), SizeOf(Result)]);
  Inc(Buffer, SizeOf(Byte));
  Result := PByte(Buffer)^;
  Inc(Buffer, SizeOf(Result));
end;

procedure TSilFirebirdParameterBlock.AddWord(Info: PFbParameterInfo; var Buffer: string; const Value: Word);
begin
  Sil.Str.Add(Buffer, Char(Info.Code));
  Sil.Str.Append(Buffer, Byte(SizeOf(Value)));
  Sil.Str.Append(Buffer, Value);
end;

function TSilFirebirdParameterBlock.GetWord(Info: PFbParameterInfo; var Buffer: PChar): Word;
begin
  Inc(Buffer);
  Sil.Error.Check(Byte(Buffer^) = SizeOf(Result), SAssertIncorrectSize, [Info.Name^, Byte(Buffer^), SizeOf(Result)]);
  Inc(Buffer, SizeOf(Byte));
  Result := PByte(Buffer)^;
  Inc(Buffer, SizeOf(Result));
end;

procedure TSilFirebirdParameterBlock.AddInteger(Info: PFbParameterInfo; var Buffer: string; const Value: Integer);
begin
  Sil.Str.Add(Buffer, Char(Info.Code));
  Sil.Str.Append(Buffer, Byte(SizeOf(Value)));
  Sil.Str.Append(Buffer, Value);
end;

function TSilFirebirdParameterBlock.GetInteger(Info: PFbParameterInfo; var Buffer: PChar): Integer;
begin
  Inc(Buffer);
  Sil.Error.Check(Byte(Buffer^) = SizeOf(Result), SAssertIncorrectSize, [Info.Name^, Byte(Buffer^), SizeOf(Result)]);
  Inc(Buffer, SizeOf(Byte));
  Result := PByte(Buffer)^;
  Inc(Buffer, SizeOf(Result));
end;

procedure TSilFirebirdParameterBlock.AddString(Info: PFbParameterInfo; var Buffer: string; const Value: String);
begin
  Sil.Str.Add(Buffer, Char(Info.Code));
  Sil.Str.Append(Buffer, Byte(Length(Value)));
  Sil.Str.Append(Buffer, Value);
end;

function TSilFirebirdParameterBlock.GetString(Info: PFbParameterInfo; var Buffer: PChar): String;
var
  Len: Byte;
begin
  Inc(Buffer);
  Len := Byte(Buffer^);
  Inc(Buffer, SizeOf(Byte));
  SetLength(Result, Len);
  Sil.Mem.Move(Buffer^, Result[1], Len);
  Inc(Buffer, Len);
end;

{ TSilFirebirdServiceRequestBuffer }

procedure TSilFirebirdServiceRequestBuffer.AddBoolean(Info: PFbParameterInfo; var Buffer: string; const Value: Boolean);
begin
  inherited;
end;

function TSilFirebirdServiceRequestBuffer.GetBoolean(Info: PFbParameterInfo; var Buffer: PChar): Boolean;
begin
  Result := inherited GetBoolean(Info, Buffer);
end;

procedure TSilFirebirdServiceRequestBuffer.AddByte(Info: PFbParameterInfo; var Buffer: string; const Value: Byte);
begin
  inherited;
end;

function TSilFirebirdServiceRequestBuffer.GetByte(Info: PFbParameterInfo; var Buffer: PChar): Byte;
begin
  Result := inherited GetByte(Info, Buffer);
end;

procedure TSilFirebirdServiceRequestBuffer.AddWord(Info: PFbParameterInfo; var Buffer: string; const Value: Word);
begin
  inherited;
end;

function TSilFirebirdServiceRequestBuffer.GetWord(Info: PFbParameterInfo; var Buffer: PChar): Word;
begin
  Result := inherited GetWord(Info, Buffer);
end;

procedure TSilFirebirdServiceRequestBuffer.AddInteger(Info: PFbParameterInfo; var Buffer: string; const Value: Integer);
begin
  Sil.Str.Add(Buffer, Char(Info.Code));
  Sil.Str.Append(Buffer, Value);
end;

function TSilFirebirdServiceRequestBuffer.GetInteger(Info: PFbParameterInfo; var Buffer: PChar): Integer;
begin
  Inc(Buffer);
  Result := PInteger(Buffer)^;
  Inc(Buffer, SizeOf(Result));
end;

procedure TSilFirebirdServiceRequestBuffer.AddString(Info: PFbParameterInfo; var Buffer: string; const Value: String);
var
  Len: Word;
begin
  Len := Length(Value);
  Sil.Str.Add(Buffer, Char(Info.Code));
  Sil.Str.Append(Buffer, Len);
  Sil.Str.Append(Buffer, Value);
end;

function TSilFirebirdServiceRequestBuffer.GetString(Info: PFbParameterInfo; var Buffer: PChar): String;
var
  Len: Word;
begin
  Inc(Buffer);
  Len := PWord(Buffer)^;
  SetLength(Result, Len);
  Inc(Buffer, SizeOf(Word));
  Sil.Mem.Move(Buffer^, Result[1], Len);  
  Inc(Buffer, Len);
end;

end.
 