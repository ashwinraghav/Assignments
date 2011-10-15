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

unit SilSmIniFileConfig;

{$I Defines.inc}

interface

uses
  Sil,
  SilTool,
  SilUrl,

  SilLmField,
  SilSiConfig;

type
  TSilIniFileConfig = class (TSilObject, IConfiguration)
  private
    FSource: IUrl;
    FTree: IValueKeys;
    FMustSave: Boolean;
  private
    procedure DoSave;
  protected // IConfiguration
    function GetSource: IUrl;
    function GetContent: IConfigNode;
  public
    constructor Create(const Url: IUrl; CanCreate: Boolean);
    destructor Destroy; override;
  end;

  TSilIniConfigNode = class (TSilObject, IConfigNode)
  private
    FName: String;
    FNode: IValueKeys;
    FValues: INamedValues;
  protected // IConfigNode
    function GetName: string;
    function GetData: IConfigData;
    function Enumerate(var Enum: IEnumerator; out Name: string): Boolean; overload;
    function Enumerate(var Enum: IEnumerator; out Item: IConfigNode): Boolean; overload;
    function Find(const Name: string): Boolean; overload;
    function Find(const Name: string; out Item: IConfigNode): Boolean; overload;
    function Get(const Name: string; CanCreate: Boolean = False): IConfigNode;
    function Remove(const Name: string; Recursive: Boolean = True): Boolean;
    procedure Clear(Recursive: Boolean = True);
  public
    constructor Create(const Name: String; const Node: IValueKeys; const Values: INamedValues);
    destructor Destroy; override;
  end;

  TSilIniConfigArguments = class (TSilObject, IConfigData)
  private
    FNode: INamedValues;
  protected // IConfigData
    function Find(const Name: string): Boolean; overload;
    function Find(const Name: string; out Value: IVariable): Boolean; overload;
    function Enumerate(var Enum: IEnumerator; out Name: string): Boolean; overload;
    function Enumerate(var Enum: IEnumerator; out Item: RConfigData): Boolean; overload;
    function Get(const Name: string; CanCreate: Boolean = False): IVariable;
    function Remove(const Name: string): Boolean;
    procedure Clear;
  public
    constructor Create(const Node: INamedValues);
    destructor Destroy; override;
  end;

  TSilIniConfigArgument = class (TSilField)
  private
    FNode: INamedValues;
    FName: String;
    FValue: String;
  protected
    procedure DoWrite(const Buffer; Size: LongWord); override;
    procedure DoRead(var Buffer; Size: LongWord); override;
    function DoIsAssigned: Boolean; override;
  public
    constructor Create(const Node: INamedValues; const Name: String; CanCreate: Boolean);
    destructor Destroy; override;
  end;

implementation

uses SilLiKey;

{ TSilIniFileConfig }

constructor TSilIniFileConfig.Create(const Url: IUrl; CanCreate: Boolean);
begin
  inherited Create;

  FMustSave := CanCreate;
  FSource := Url;

  FTree := SilTool.Sv.Configuration.Open(FSource.Address.Text, CanCreate);
end;

destructor TSilIniFileConfig.Destroy;
begin
  if FMustSave then DoSave;
  FSource := nil;

  inherited;
end;

procedure TSilIniFileConfig.DoSave;
begin

end;

function TSilIniFileConfig.GetContent: IConfigNode;
begin
  Result := TSilIniConfigNode.Create('', FTree, nil);
end;

function TSilIniFileConfig.GetSource: IUrl;
begin
  Result := FSource;
end;

{ TSilIniConfigNode }

constructor TSilIniConfigNode.Create(const Name: String; const Node: IValueKeys; const Values: INamedValues);
begin
  inherited Create;

  FName := Name;
  FNode := Node;
  FValues := Values;
end;

destructor TSilIniConfigNode.Destroy;
begin
  FNode := nil;
  FValues := nil;

  inherited;
end;

procedure TSilIniConfigNode.Clear(Recursive: Boolean);
begin

end;

function TSilIniConfigNode.Enumerate(var Enum: IEnumerator; out Name: string): Boolean;
begin
  Result := Assigned(FNode) and FNode.Enumerate(Enum, Name);
end;

function TSilIniConfigNode.Enumerate(var Enum: IEnumerator; out Item: IConfigNode): Boolean;
var
  Name: String;
  Node: INamedValues;
begin
  Result := Assigned(FNode) and FNode.Enumerate(Enum, Name);

  if Result then
  begin
    Node := FNode.Get(Name, false);
    Item := TSilIniConfigNode.Create(Name, nil, Node);
  end;
end;

function TSilIniConfigNode.Find(const Name: string): Boolean;
begin
  Result := Assigned(FNode) and FNode.Exists(Name);
end;

function TSilIniConfigNode.Find(const Name: string; out Item: IConfigNode): Boolean;
var
  Node: INamedValues;
begin
  Result := Assigned(FNode) and FNode.Find(Name, Node);
  if Result then Item := TSilIniConfigNode.Create(Name, nil, Node);
end;

function TSilIniConfigNode.Get(const Name: string; CanCreate: Boolean): IConfigNode;
var
  Values: INamedValues;
begin
  Values := FNode.Get(Name, CanCreate);

  if Assigned(Values) then
    Result := TSilIniConfigNode.Create(Name, nil, Values);
end;

function TSilIniConfigNode.GetData: IConfigData;
begin
  Result := TSilIniConfigArguments.Create(FValues);
end;

function TSilIniConfigNode.GetName: string;
begin
  Result := FName;
end;

function TSilIniConfigNode.Remove(const Name: string; Recursive: Boolean): Boolean;
begin
  Result := Assigned(FNode) and FNode.Remove(Name);
end;

{ TSilIniConfigArguments }

constructor TSilIniConfigArguments.Create(const Node: INamedValues);
begin
  inherited Create;
  FNode := Node;
end;

destructor TSilIniConfigArguments.Destroy;
begin
  FNode := nil;
  inherited;
end;

procedure TSilIniConfigArguments.Clear;
begin

end;

function TSilIniConfigArguments.Enumerate(var Enum: IEnumerator; out Name: string): Boolean;
begin
  Result := FNode.Enumerate(Enum, Name);
end;

function TSilIniConfigArguments.Enumerate(var Enum: IEnumerator; out Item: RConfigData): Boolean;
var
  Name: String;
begin
  Result := FNode.Enumerate(Enum, Name);

  if Result then
  begin
    Item.Name := Name;
    Item.Item := Get(Name, false);
  end;
end;

function TSilIniConfigArguments.Find(const Name: string): Boolean;
begin
  Result := FNode.Exists(Name);
end;

function TSilIniConfigArguments.Find(const Name: string; out Value: IVariable): Boolean;
begin
  Result := FNode.Exists(Name);

  if Result then
    Value := Get(Name, false);
end;

function TSilIniConfigArguments.Get(const Name: string; CanCreate: Boolean): IVariable;
begin
  Result := TSilIniConfigArgument.Create(FNode, Name, CanCreate);
end;

function TSilIniConfigArguments.Remove(const Name: string): Boolean;
begin
  Result := FNode.Remove(Name);
end;

{ TSilIniConfigArgument }

constructor TSilIniConfigArgument.Create(const Node: INamedValues; const Name: String; CanCreate: Boolean);
begin
  inherited Create(ftString);

  FNode := Node;
  FName := Name;

  FValue := FNode.ReadString(FName, '', false);
  SetSize(Length(FValue));
end;

destructor TSilIniConfigArgument.Destroy;
begin
  FNode := nil;
  inherited;
end;

function TSilIniConfigArgument.DoIsAssigned: Boolean;
begin
  Result := Str.NotEmpty(FValue);
end;

procedure TSilIniConfigArgument.DoRead(var Buffer; Size: LongWord);
begin
  if Size > 0 then
  begin
    Move(FValue[1], Buffer, Size);
    FNode.ReadString(FName);
  end;
end;

procedure TSilIniConfigArgument.DoWrite(const Buffer; Size: LongWord);
begin
  if Size > 0 then
  begin
    SetLength(FValue, Size);
    Move(Buffer, FValue[1], Size);
    FNode.WriteString(FName, FValue);
  end;
end;

end.
