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

unit SilSmRegistryConfig;

{$I Defines.inc}

interface

uses
  Sil,
  SilUrl,

  SilLmField,
  SilSiConfig;

const
  CTagSeparator = '/';

type
  TSilRegistryConfig = class (TSilObject, IConfiguration)
  private
    FSource: IUrl;
    FTree: INamedKey;
    FMustSave: Boolean;
    FPerm: TNamedKeyPermision;
  private
    procedure DoSave;
  protected // IConfiguration
    function GetSource: IUrl;
    function GetContent: IConfigNode;
  public
    constructor Create(const Url: IUrl; CanCreate: Boolean);
    destructor Destroy; override;
  end;

  TSilRegistryConfigNode = class (TSilObject, IConfigNode)
  private
    FNode: INamedKey;
    FPerm: TNamedKeyPermision;
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
    constructor Create(const Node: INamedKey; Perm: TNamedKeyPermision);
    destructor Destroy; override;
  end;

  TSilRegistryConfigArguments = class (TSilObject, IConfigData)
  private
    FNode: INamedKey;
  protected // IConfigData
    function Find(const Name: string): Boolean; overload;
    function Find(const Name: string; out Value: IVariable): Boolean; overload;
    function Enumerate(var Enum: IEnumerator; out Name: string): Boolean; overload;
    function Enumerate(var Enum: IEnumerator; out Item: RConfigData): Boolean; overload;
    function Get(const Name: string; CanCreate: Boolean = False): IVariable;
    function Remove(const Name: string): Boolean;
    procedure Clear;
  public
    constructor Create(const Node: INamedKey);
    destructor Destroy; override;
  end;

  TSilRegistryArgument = class (TSilField)
  private
    FNode: INamedKey;
    FName: String;
    FValue: String;
  protected
    procedure DoWrite(const Buffer; Size: LongWord); override;
    procedure DoRead(var Buffer; Size: LongWord); override;
    function DoIsAssigned: Boolean; override;
  public
    constructor Create(const Node: INamedKey; const Name: String; CanCreate: Boolean);
    destructor Destroy; override;
  end;

implementation

uses SilSiUrl, SilLiKey;

{ TSilRegistryConfig }

constructor TSilRegistryConfig.Create(const Url: IUrl; CanCreate: Boolean);
var
  Path: String;
  Value: Variant;
begin
  inherited Create;

  FMustSave := CanCreate;
  FSource := Url;

  if FSource.Params.List.Find('Permision', Value) then
    FPerm := TNamedKeyPermision(Sil.Enum.Value(TypeInfo(TNamedKeyPermision), Value, 'kp')) else
    FPerm := kpReadWrite;

  Path := Str.Format('$%s\%s', [FSource.Address.Text, Str.Replace(FSource.Path.Text, '/', '\')]);
  FTree := Sil.OS.Registry.Open(Path, FPerm, CanCreate);
end;

destructor TSilRegistryConfig.Destroy;
begin
  if FMustSave then DoSave;
  FSource := nil;

  inherited;
end;

procedure TSilRegistryConfig.DoSave;
begin

end;

function TSilRegistryConfig.GetContent: IConfigNode;
begin
  Result := TSilRegistryConfigNode.Create(FTree, FPerm);
end;

function TSilRegistryConfig.GetSource: IUrl;
begin
  Result := FSource;
end;

{ TSilRegistryConfigNode }

constructor TSilRegistryConfigNode.Create(const Node: INamedKey; Perm: TNamedKeyPermision);
begin
  inherited Create;

  FNode := Node;
  FPerm := Perm;
end;

destructor TSilRegistryConfigNode.Destroy;
begin
  FNode := nil;
  inherited;
end;

procedure TSilRegistryConfigNode.Clear(Recursive: Boolean);
begin
  // not implemented
end;

function TSilRegistryConfigNode.Enumerate(var Enum: IEnumerator; out Name: string): Boolean;
begin
  Result := FNode.Keys.Enumerate(Enum, Name);
end;

function TSilRegistryConfigNode.Enumerate(var Enum: IEnumerator; out Item: IConfigNode): Boolean;
var
  Name: String;
  Node: INamedKey;
begin
  Result := FNode.Keys.Enumerate(Enum, Name);

  if Result then
  begin
    Node := FNode.Keys.Get(Name, FPerm, false);
    Item := TSilRegistryConfigNode.Create(Node, FPerm);
  end;
end;

function TSilRegistryConfigNode.Find(const Name: string): Boolean;
begin
  Result := FNode.Keys.Exists(Name);
end;

function TSilRegistryConfigNode.Find(const Name: string; out Item: IConfigNode): Boolean;
var
  Node: INamedKey;
begin
  Result := FNode.Keys.Find(Name, Node);
  if Result then Item := TSilRegistryConfigNode.Create(Node, FPerm);
end;

function TSilRegistryConfigNode.Get(const Name: string; CanCreate: Boolean): IConfigNode;
var
  Node: INamedKey;
  Right, Left: string;
begin
  Str.Split(Name, CTagSeparator, Left, Right);

  if Str.NotEmpty(Right) then
  begin
    Result := Get(Left, CanCreate).Get(Right, CanCreate);
  end else
  begin
    if FNode.Keys.Find(Name, Node) then
      Result := TSilRegistryConfigNode.Create(Node, FPerm);

    if not Assigned(Result) then
      if CanCreate then
      begin
        Node := FNode.Keys.Add(Name, FPerm);
        Result := TSilRegistryConfigNode.Create(Node, FPerm);
      end else
        raise Error.Create('no existe el nodo %s', [Name]);
  end;
end;

function TSilRegistryConfigNode.GetData: IConfigData;
begin
  Result := TSilRegistryConfigArguments.Create(FNode);
end;

function TSilRegistryConfigNode.GetName: string;
begin
  Result := FNode.Name;
end;

function TSilRegistryConfigNode.Remove(const Name: string; Recursive: Boolean): Boolean;
var
  Key: INamedKey;
  Enum: IEnumerator;
  Item: String;
begin
  Result := Recursive or FNode.Keys.Find(Name, Key) and (not Key.Keys.Enumerate(Enum, Item));

  if Result then
  begin
    Enum := nil;
    FNode.Keys.Remove(Name);
  end;
end;

{ TSilRegistryConfigArguments }

constructor TSilRegistryConfigArguments.Create(const Node: INamedKey);
begin
  inherited Create;
  FNode := Node;
end;

destructor TSilRegistryConfigArguments.Destroy;
begin
  FNode := nil;
  inherited;
end;

procedure TSilRegistryConfigArguments.Clear;
begin
  //
end;

function TSilRegistryConfigArguments.Enumerate(var Enum: IEnumerator; out Name: string): Boolean;
begin
  Result := FNode.Values.Enumerate(Enum, Name);
end;

function TSilRegistryConfigArguments.Enumerate(var Enum: IEnumerator; out Item: RConfigData): Boolean;
var
  Name: String;
begin
  Result := FNode.Values.Enumerate(Enum, Name);

  if Result then
  begin
    Item.Name := Name;
    Item.Item := Get(Name, false);
  end;
end;

function TSilRegistryConfigArguments.Find(const Name: string): Boolean;
begin
  Result := FNode.Keys.Exists(Name);
end;

function TSilRegistryConfigArguments.Find(const Name: string; out Value: IVariable): Boolean;
var
  Key: INamedKey;
begin
  Result := FNode.Keys.Find(Name, Key);

  if Result then
    Value := Get(Name, false);
end;

function TSilRegistryConfigArguments.Get(const Name: string; CanCreate: Boolean): IVariable;
begin
  Result := TSilRegistryArgument.Create(FNode, Name, CanCreate);
end;

function TSilRegistryConfigArguments.Remove(const Name: string): Boolean;
begin
  Result := FNode.Keys.Remove(Name);
end;

{ TSilRegistryArgument }

constructor TSilRegistryArgument.Create(const Node: INamedKey; const Name: String; CanCreate: Boolean);
const
  Types: array [TNamedKeyDataType] of TDataFieldType = (ftUnknown, ftString, ftInteger, ftMemo, ftBytes, ftLargeInt);
begin
  inherited Create(Types[Node.Values.GetDataType(Name)]);

  FNode := Node;
  FName := Name;

  FValue := FNode.Values.ReadString(FName, '', false);
  SetSize(Length(FValue));
end;

destructor TSilRegistryArgument.Destroy;
begin
  FNode := nil;
  inherited;
end;

function TSilRegistryArgument.DoIsAssigned: Boolean;
begin
  Result := Str.NotEmpty(FValue);
end;

procedure TSilRegistryArgument.DoRead(var Buffer; Size: LongWord);
begin
  if Size > 0 then
    case DataType of
      ftString,
      ftMemo,
      ftBytes:
      begin
        Move(FValue[1], Buffer, Size);
        FNode.Values.ReadString(FName);
      end;

      ftInteger:  Integer(Buffer) := FNode.Values.ReadInteger(FName);
      ftLargeInt: LargeInt(Buffer) := FNode.Values.ReadLargeInt(FName);
    end;
end;

procedure TSilRegistryArgument.DoWrite(const Buffer; Size: LongWord);
begin
  if Size > 0 then
  begin
    SetLength(FValue, Size);
    Move(Buffer, FValue[1], Size);

    case DataType of
      ftString,
      ftMemo,
      ftBytes:    FNode.Values.WriteString(FName, FValue);
      ftInteger:  FNode.Values.WriteInteger(FName, Integer(Buffer));
      ftLargeInt: FNode.Values.WriteLargeInt(FName, LargeInt(Buffer));
    end;

    SetSize(Size);
  end;
end;

end.
