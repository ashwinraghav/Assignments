{********************************************************************************
 *                  Standard Interface Library (SIL)                            *
 *                                                                              *
 *       General purpose library whose design is based in STRONG                *
 *   use of interfaces.                                                         *
 *                                                                              *
 *                                                                              *
 *     Copyright (C) 2000 Mariano Podestá    marianop@intercom.com.ar           *
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

unit UtXml;

interface

uses
  Sil,
  SilXml;

type
  AgentXml = class (Tool)
    class function Command(const Buffer: String; out Kind: String; out Params: IParameterList): Boolean;
    class function ReadNotification(const ServiceName, Buffer: String; out Params: IParameterList): Boolean;
  end;

implementation

uses SilLtList, SilLiKey;

{ Xml }

class function AgentXml.Command(const Buffer: String; out Kind: String; out Params: IParameterList): Boolean;
var
  Enum: IEnumerator;
  Tree: IXmlTree;
  Root, Tag: IXmlTag;
  Node: IXmlNode;
  Stream: IMemoryStream;
begin
  Result := false;

  try
    Stream := Sil.Stream.Memory(Buffer);
    Tree := SilXml.Tool.ReadStream(Stream);
    Params := Sil.List.Parameters;
    Root := Tree.Root.AsTag;

    if Assigned(Root) then
    begin
      Kind := Str.ToLower(Str.Trim(Root.Arguments.ReadString('type')));
      Result := Str.NotEmpty(Kind);

      Tag := Root.GetTag('command', true);
      Params['command'] := Tag.Data.Text;

      Tag := Root.GetTag('params', true);

      while Tag.Childs.Enumerate(Enum, Node) do
        if Node.NodeKind = nkTag then
        begin
          Tag := Node.AsTag;
          Params[Tag.Name] := Tag.Data.Text;
        end;
    end;
  except
    // log
  end;
end;

(*)
<data>
  <smtp>
    <event>failed|succeeded|all</event>
    <recipients>
      pp@mail.com
      kcho@mail.com
    </recipients>
    <params>
      <attach value="1"/>
      <attach_file value="c:\back\*.fbk"/>
      <attach_count value="1"/>
    </params>
  </smtp>
  <netsend>
    <event>failed|succeeded|all</event>
    <recipients>
      hcc2616
      hcc2619
    </recipients>
  </netsend>
</data>
(*)

class function AgentXml.ReadNotification(const ServiceName, Buffer: String; out Params: IParameterList): Boolean;
var
  Enum: IEnumerator;
  Root: IXmlTag;
  Node: IXmlNode;
begin
  Result := false;

  if Str.NotEmpty(Buffer) then
    try
      Root := SilXml.Tool.FromStr(Buffer).Root.AsTag;

      if Assigned(Root) then
        while Root.Childs.Enumerate(Enum, Node) do
          if Node.NodeKind = nkTag then
            with Node.AsTag do
              if Sil.Text.IsEqual(Name, ServiceName) then
              begin
                Params := Sil.List.Parameters;
                Params['event'] := Childs.ReadString('event');
                Params['recipients'] := Childs.ReadStrings('recipients');

                Result := true;
                Break;
              end;
    except
      // log
    end;
end;

end.
