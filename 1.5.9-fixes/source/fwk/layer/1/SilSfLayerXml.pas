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

unit SilSfLayerXml;

interface

{$include Defines.inc}

uses
  Sil,
  SilXml,
  SilSiLayer;

function ReadXmlChain(const Tag: IXmlTag; const Bindings: ILayerBindings): ILayerChain;

implementation

{uses
  //UmLayerProtocolText,
  SilLayer;}

function DoLookupLayer(const Id: String; const Params: IParameters): IUnknown;
begin
(*)
  if Id = 'sil.layer.chain' then Result := SilLayer.Layer.Chain;
  if Id = 'sil.layer.device.socketclient' then Result := SilLayer.Device.SocketClient(Params);
  if Id = 'sil.layer.device.socketserver' then Result := SilLayer.Device.SocketServer(Params);
  if Id = 'sil.layer.packer.imate' then Result := SilLayer.Packer.Imate(Params);
  if Id = 'sil.layer.slot' then Result := SilLayer.Layer.Slot;
  if Id = 'sil.layer.protocol.imate' then Result := SilLayer.Protocol.Imate(0, Params);
  if Id = 'sil.layer.protocol.blind' then Result := SilLayer.Protocol.Blind;
  //if Id = 'sil.layer.protocol.text' then Result := TTextProtocol.Create;
(*)  
end;

function DoReadParams(const Tag: IXmlTag; const Bindings: ILayerBindings): IParameterList;
var
  Enum: IEnumerator;
  Node: IXmlNode;
  NameVal, GetVal: String;
begin
  Result := Sil.List.Parameters;

  if Tag.HasChilds then
    while Tag.Childs.Enumerate(Enum, Node) do
      if Node.NodeKind = nkTag then
        with Node.AsTag do
          if Name = 'param' then
          begin
            NameVal := Arguments.ReadString('name');
            GetVal := Arguments.ReadString('get');

            if Str.NotEmpty(GetVal) then
            begin
              if Assigned(Bindings) then
                Result[NameVal] := Bindings[GetVal];
            end else
              Result[NameVal] := Arguments.ReadString('value');
          end;
end;

function DoRead(const Tag: IXmlTag; const List: ILayerLinkList; const Bindings: ILayerBindings): ILayerChain;
var
  Id, GetVal, SetVal: String;
  Params: IParameterList;
  Item: IUnknown;
  Enum: IEnumerator;
  Node: IXmlNode;
  LinkList: ILayerLinkList;
begin
  Id := Tag.Arguments.ReadString('id');
  GetVal := Tag.Arguments.ReadString('get');
  SetVal := Tag.Arguments.ReadString('set');

  Params := DoReadParams(Tag, Bindings);

  if Str.NotEmpty(GetVal) then
  begin
    if Assigned(Bindings) then
      Item := Bindings[GetVal]
  end else
    Item := DoLookupLayer(Id, Params);

  if Str.NotEmpty(SetVal) and Assigned(Bindings) then
    Bindings[SetVal] := Item;

  if Assigned(List) then List.Add(Item);
  Ref.GetInterface(Item, ILayerLinkList, LinkList);

  if Assigned(LinkList) and Tag.HasChilds then
    while Tag.Childs.Enumerate(Enum, Node) do
      if Node.NodeKind = nkTag then
        if Node.AsTag.Name = 'layer' then
          DoRead(Node.AsTag, LinkList, Bindings);

  Ref.GetInterface(Item, ILayerChain, Result);
end;

function ReadXmlChain(const Tag: IXmlTag; const Bindings: ILayerBindings): ILayerChain;
begin
  Result := DoRead(Tag.GetTag('layer'), nil, Bindings);
end;

end.
