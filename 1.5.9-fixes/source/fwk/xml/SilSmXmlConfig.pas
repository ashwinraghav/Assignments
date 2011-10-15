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

unit SilSmXmlConfig;

{$I Defines.inc}

interface

uses
  Sil,
  SilUrl,
  SilXml,

  SilLmField,
  SilSiConfig;

const
  CTagSeparator = '/';

type
  TSilXmlConfig = class (TSilObject, IConfiguration)
  private
    FSource: IUrl;
    FTree: IXmlTree;
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

  TSilXmlConfigNode = class (TSilObject, IConfigNode)
  private
    FTree: IXmlTree;
    FNode: IXmlNode;
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
    constructor Create(const Tree: IXmlTree; const Node: IXmlNode);
    destructor Destroy; override;
  end;

  TSilXmlConfigArguments = class (TSilObject, IConfigData)
  private
    FTag: IXmlTag;
  private
    function DoGetDefault(CanCreate: Boolean): IVariable;
  protected // IConfigData
    function Find(const Name: string): Boolean; overload;
    function Find(const Name: string; out Value: IVariable): Boolean; overload;
    function Enumerate(var Enum: IEnumerator; out Name: string): Boolean; overload;
    function Enumerate(var Enum: IEnumerator; out Item: RConfigData): Boolean; overload;
    function Get(const Name: string; CanCreate: Boolean = False): IVariable;
    function Remove(const Name: string): Boolean;
    procedure Clear;
  public
    constructor Create(const Node: IXmlNode);
    destructor Destroy; override;
  end;

  TSilXmlArgument = class (TSilField)
  private
    FArgs: IXmlArguments;
    FName: String;
    FValue: String;
  protected
    procedure DoWrite(const Buffer; Size: LongWord); override;
    procedure DoRead(var Buffer; Size: LongWord); override;
    function DoIsAssigned: Boolean; override;
  public
    constructor Create(const Args: IXmlArguments; const Name: String; CanCreate: Boolean);
    destructor Destroy; override;
  end;

  TSilXmlData = class (TSilField)
  private
    FData: IStringList;
    FValue: String;
  protected
    procedure DoWrite(const Buffer; Size: LongWord); override;
    procedure DoRead(var Buffer; Size: LongWord); override;
    function DoIsAssigned: Boolean; override;
  public
    constructor Create(const Data: IStringList; const Name: String; CanCreate: Boolean);
    destructor Destroy; override;
  end;

implementation

{ TSilXmlConfig }

constructor TSilXmlConfig.Create(const Url: IUrl; CanCreate: Boolean);
begin
  inherited Create;

  FMustSave := CanCreate;
  FSource := Url;
  FTree := SilXml.Tool.ReadFile(FSource.Address.Text, nil, fmAccessRead, fmShareRead, not CanCreate);
end;

destructor TSilXmlConfig.Destroy;
begin
  if FMustSave then DoSave;
  FSource := nil;

  inherited;
end;

procedure TSilXmlConfig.DoSave;
begin
  if FTree.Modified and Assigned(FSource) then
    SilXml.Tool.WriteFile(FTree, FSource.Address.Text);
end;

function TSilXmlConfig.GetSource: IUrl;
begin
  Result := FSource;
end;

function TSilXmlConfig.GetContent: IConfigNode;
begin
  Result := TSilXmlConfigNode.Create(FTree, FTree.Root);
end;

{ TSilXmlConfigNode }

constructor TSilXmlConfigNode.Create(const Tree: IXmlTree; const Node: IXmlNode);
begin
  inherited Create;

  FTree := Tree;
  FNode := Node;
end;

destructor TSilXmlConfigNode.Destroy;
begin
  FTree := nil;
  FNode := nil;

  inherited;
end;

procedure TSilXmlConfigNode.Clear(Recursive: Boolean);
begin
  // not implemented
end;

function TSilXmlConfigNode.Enumerate(var Enum: IEnumerator; out Name: string): Boolean;
begin
  Result := FNode.Childs.Enumerate(Enum, Name);
end;

function TSilXmlConfigNode.Enumerate(var Enum: IEnumerator; out Item: IConfigNode): Boolean;
var
  Node: IXmlNode;
begin
  Result := FNode.Childs.Enumerate(Enum, Node);

  if Result then
    Item := TSilXmlConfigNode.Create(FTree, Node);
end;

function TSilXmlConfigNode.Find(const Name: string): Boolean;
begin
  Result := FNode.Childs.Exists(Name);
end;

function TSilXmlConfigNode.Find(const Name: string; out Item: IConfigNode): Boolean;
var
  Enum: IEnumerator;
  Node: IXmlNode;
begin
  while FNode.Childs.Enumerate(Enum, Node) do
    if Node.NodeKind = nkTag then
      if Sil.Text.Compare(Node.AsTag.Name, Name) = 0 then
      begin
        Item := TSilXmlConfigNode.Create(FTree, Node);
        Break;
      end;

  Result := Assigned(Item);
end;

function TSilXmlConfigNode.Get(const Name: string; CanCreate: Boolean): IConfigNode;
var
  Enum: IEnumerator;
  Node: IXmlNode;
  Right, Left: string;
begin
  Str.Split(Name, CTagSeparator, Left, Right);

  if Str.NotEmpty(Right) then
  begin
    Result := Get(Left, CanCreate).Get(Right, CanCreate);
  end else
  begin
    while FNode.Childs.Enumerate(Enum, Node) do
      if Node.NodeKind = nkTag then
        if Sil.Text.Compare(Node.AsTag.Name, Name) = 0 then
        begin
          Result := TSilXmlConfigNode.Create(FTree, Node);
          Break;
        end;

    if not Assigned(Result) then
      if CanCreate then
      begin
        Node := FNode.Childs.Add(nkTag);
        Node.AsTag.Name := Name;

        Result := TSilXmlConfigNode.Create(FTree, Node);
      end else
        raise Error.Create('no existe el nodo %s', [Name]);
  end;
end;

function TSilXmlConfigNode.GetData: IConfigData;
begin
  Result := TSilXmlConfigArguments.Create(FNode);
end;

function TSilXmlConfigNode.GetName: string;
begin
  Result := FNode.AsTag.Name;
end;

function TSilXmlConfigNode.Remove(const Name: string; Recursive: Boolean): Boolean;
var
  Tag: IXmlTag;
begin
  Result := Recursive or FNode.AsTag.FindTag(Name, Tag) and (Tag.Childs.Count = 0);
  if Result then FNode.Childs.Remove(Name);
end;

{ TSilXmlConfigArguments }

constructor TSilXmlConfigArguments.Create(const Node: IXmlNode);
begin
  inherited Create;
  FTag := Node.AsTag;
end;

destructor TSilXmlConfigArguments.Destroy;
begin
  FTag := nil;
  inherited;
end;

procedure TSilXmlConfigArguments.Clear;
begin
  FTag.Arguments.Clear;
end;

function TSilXmlConfigArguments.Enumerate(var Enum: IEnumerator; out Name: string): Boolean;
begin
  Result := FTag.Arguments.Enumerate(Enum, Name);
end;

function TSilXmlConfigArguments.Enumerate(var Enum: IEnumerator; out Item: RConfigData): Boolean;
var
  Name: String;
begin
  Result := FTag.Arguments.Enumerate(Enum, Name);

  if Result then
  begin
    Item.Name := Name;
    Item.Item := Get(Name, false);
  end;
end;

function TSilXmlConfigArguments.Find(const Name: string): Boolean;
begin
  Result := FTag.Arguments.Exists(Name);
end;

function TSilXmlConfigArguments.DoGetDefault(CanCreate: Boolean): IVariable;
begin
  Result := TSilXmlData.Create(FTag.Data, '@', CanCreate);
end;

function TSilXmlConfigArguments.Find(const Name: string; out Value: IVariable): Boolean;
var
  Enum: IEnumerator;
  Item: String;
begin
  if Name = '@' then
  begin
    Result := Assigned(FTag.Data) and (FTag.Data.Count > 0);
    if Result then Value := DoGetDefault(false);
    Exit;
  end;

  while FTag.Arguments.Enumerate(Enum, Item) do
    if Sil.Text.Compare(Name, Item) = 0 then
    begin
      Result := true;
      Value := Get(Name, false);
      Exit;
    end;

  Result := false;
end;

function TSilXmlConfigArguments.Get(const Name: string; CanCreate: Boolean): IVariable;
begin
  if Name = '@' then
    Result := DoGetDefault(CanCreate) else
    Result := TSilXmlArgument.Create(FTag.Arguments, Name, CanCreate);
end;

function TSilXmlConfigArguments.Remove(const Name: string): Boolean;
begin
  Result := FTag.Arguments.Remove(Name);
end;

{ TSilXmlArgument }

constructor TSilXmlArgument.Create(const Args: IXmlArguments; const Name: String; CanCreate: Boolean);
begin
  inherited Create(ftString);

  FArgs := Args;
  FName := Name;
  FValue := FArgs.ReadString(FName, '', CanCreate);
  SetSize(Length(FValue));
end;

destructor TSilXmlArgument.Destroy;
begin
  FArgs := nil;
  inherited;
end;

function TSilXmlArgument.DoIsAssigned: Boolean;
begin
  Result := Str.NotEmpty(FValue);
end;

procedure TSilXmlArgument.DoRead(var Buffer; Size: LongWord);
begin
  if Size > 0 then Move(FValue[1], Buffer, Size);
end;

procedure TSilXmlArgument.DoWrite(const Buffer; Size: LongWord);
begin
  if Size > 0 then
  begin
    SetLength(FValue, Size);
    Move(Buffer, FValue[1], Size);
    FArgs.WriteString(FName, FValue);
    SetSize(Size);
  end;
end;

{ TSilXmlData }

constructor TSilXmlData.Create(const Data: IStringList; const Name: String; CanCreate: Boolean);
begin
  inherited Create(ftString);

  FData := Data;
  FValue := FData.Text;
  SetSize(Length(FValue));
end;

destructor TSilXmlData.Destroy;
begin
  FData := nil;
  inherited;
end;

function TSilXmlData.DoIsAssigned: Boolean;
begin
  Result := Str.NotEmpty(FValue);
end;

procedure TSilXmlData.DoRead(var Buffer; Size: LongWord);
begin
  if Size > 0 then Move(FValue[1], Buffer, Size);
end;

procedure TSilXmlData.DoWrite(const Buffer; Size: LongWord);
begin
  if Size > 0 then
  begin
    SetLength(FValue, Size);
    Move(Buffer, FValue[1], Size);
    FData.Text := FValue;
    SetSize(Size);
  end;
end;

end.
