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

unit SilSmNotifyReg;

interface

uses
  Sil,
  SilSiNotify;

type
  TRegistryEventConfig = class(
    // extends
    TSilInterfacedObject,
    // implements
    IEventConfig)
  private
    FRoot: String;
    FDefParams: IParameterList;
  protected // IEventConfig
    function GetDefParams: IParameters;
    function Configure(const Event: IEventData; const Server: IEventServer): Boolean;
  public
    constructor Create(const Root: String);
    destructor Destroy; override;
  end;

implementation

{ TRegistryEventConfig }

constructor TRegistryEventConfig.Create(const Root: String);
begin
  inherited Create;
  FRoot := Root;
end;

destructor TRegistryEventConfig.Destroy;
begin
  FDefParams := nil;
  inherited;
end;

function TRegistryEventConfig.Configure(const Event: IEventData; const Server: IEventServer): Boolean;
var
  Key, Sub: INamedKey;
  Params: IParameterList;
  sValue, sItem, sTo, sRcpt: String;
  i1, i2: Integer;
begin
  try
    Key := Sil.OS.Registry.Open(FRoot + '\Notifier', true);
    Params := Sil.List.Parameters;

    Sub := Key.Keys.Get('Events', true);
    sValue := Sub.Values.ReadString(Event.Name, '', true);
    i1 := 0;

    while Str.Enumerate(sValue, ';', sItem, i1) do
    begin
      sItem := Str.Trim(sItem);
      i2 := Str.Pos('=', sItem);
      if i2 > 0 then Params[Str.Left(sItem, i2 - 1)] := Str.Copy(sItem, i2 + 1);
    end;

    if Str.IsEmpty(Event.Text) then
      Event.Text := Params['text'];
      
    sTo := Params['to'];
    i1 := 0;
    Sub := Key.Keys.Get('Recipients', true);

    while Str.Enumerate(sTo, ',', sItem, i1) do
    begin
      sRcpt := Sub.Values.ReadString(Str.Trim(sItem), '', true);
      if Str.NotEmpty(sRcpt) then Event.Recipients.Add(sRcpt)
    end;

    Result := true;
  except
    Result := false;
  end;
end;

function TRegistryEventConfig.GetDefParams: IParameters;
var
  Key: INamedKey;

  procedure DoCollect(const Key: INamedKey; const Name: String);
  var
    Enum: IEnumerator;
    sItem, sName: String;
  begin
    if Str.NotEmpty(Name) then
      sName := Name + '.' else
      sName := '';

    while Key.Keys.Enumerate(Enum, sItem) do
      DoCollect(Key.Keys.Get(sItem), sName + sItem);

    while Key.Values.Enumerate(Enum, sItem) do
      FDefParams[sName + sItem] := Key.Values.ReadString(sItem);
  end;

begin
  if FDefParams = nil then
  begin
    FDefParams := Sil.List.Parameters;
    Key := Sil.OS.Registry.Open(FRoot + '\Notifier\Parameters', true);

    DoCollect(Key, '');
  end;

  Result := FDefParams;
end;

end.
