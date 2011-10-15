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

unit SilVmStringProfile;

interface

uses
	Classes,

  Sil,

	SilViProfiler,
	SilVmProfiler;

type

{ TStringProfile }
                          
	TStringProfile = class(TSilInterfacedObject, IProfileSource)
	private
		FFileName: String;
		FLine: String;
		FItemName: String;
		FValue: String;
		FLines: IStringList;
		FIndex: Integer;
	protected
		function SourceName: String;
		function BeginRead(const Name: String): Boolean;
		procedure EndRead(const Name: String);
		function HasMore: Boolean;
		procedure ReadNext;
		function ItemIsOwner: Boolean;
		function ItemIsObject: Boolean;
		function ItemIsValue: Boolean;
		function ItemName: String;
		function PropName: String;
		function PropValue: String;
		procedure NotifyOnChanges(Enable: Boolean);
	public
		constructor Create(const FileName: String);
		destructor Destroy; override;
	end;


implementation

{ TStringProfile }

function TStringProfile.BeginRead(const Name: String): Boolean;
begin
	FLines := Sil.List.StringList;
	FIndex := 0;
	try
    Sil.Serializer.LoadFromFile(FLines, FFileName);
		Result := true;
	except
		Result := false;
	end;
end;

constructor TStringProfile.Create(const FileName: String);
begin
	inherited Create;
	FFileName := FileName;
end;

destructor TStringProfile.Destroy;
begin
	FLines := nil;
	inherited;
end;

procedure TStringProfile.EndRead(const Name: String);
begin
	FLines := nil;
end;

function TStringProfile.HasMore: Boolean;
begin
	Result := FIndex < FLines.Count;
end;

function TStringProfile.ItemIsObject: Boolean;
begin
	Result := Str.CompareText(FLine, 'object ') = 0;
	FItemName := Str.Copy(FLine, 8);
end;

function TStringProfile.ItemIsOwner: Boolean;
begin
	Result := Str.CompareText(FLine, 'owner ') = 0;
	FItemName := Str.Copy(FLine, 7);
end;

function TStringProfile.ItemIsValue: Boolean;
begin
	Result := (Length(FLine) > 0) and (FLine[1] = '$');
end;

function TStringProfile.ItemName: String;
begin
	Result := FItemName;
end;

procedure TStringProfile.NotifyOnChanges(Enable: Boolean);
begin
//
end;

function TStringProfile.PropName: String;
var
	iPos: Integer;
begin
	iPos := Pos('=', FLine);
	if iPos > 0 then
	begin
		Result := Str.Trim(Str.Copy(FLine, 1, iPos - 1), #9);
		FValue := Str.Trim(Str.Copy(FLine, iPos + 1));
	end else
	begin
		Result := '';
		FValue := '';
	end;
end;

function TStringProfile.PropValue: String;
begin
	if Length(FValue) = 0 then PropName;
	Result := FValue;
end;

procedure TStringProfile.ReadNext;
begin
	FLine := Str.Trim(FLines[FIndex]);
	Inc(FIndex);
end;

function TStringProfile.SourceName: String;
begin
	Result := FFileName;
end;

end.
 