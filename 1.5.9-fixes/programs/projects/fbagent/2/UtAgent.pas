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

unit UtAgent;

interface

uses
  Forms, DBGrids,

  Sil;

type
  Global = class (Tool)
    class procedure ReadForm(const Name: String; Form: TForm);
    class procedure WriteForm(const Name: String; Form: TForm);
    class procedure WriteGrid(const Name: String; Grid: TDBGrid);
    class procedure ReadGrid(const Name: String; Grid: TDBGrid);
    class procedure ResizeGrid(Grid: TDBGrid);
  end;

implementation

uses
  UfConfig,
  Classes;

{ Global }

class procedure Global.ReadForm(const Name: String; Form: TForm);
begin
  with Config.GetTag('config/window/' + Name, true) do
  begin
    Form.Width := Childs.ReadInteger('width', Form.Width, true);
    Form.Height := Childs.ReadInteger('height', Form.Height, true);
  end;
end;

class procedure Global.WriteForm(const Name: String; Form: TForm);
begin
  with Config.GetTag('config/window/' + Name, true) do
  begin
    Childs.WriteInteger('width', Form.Width);
    Childs.WriteInteger('height', Form.Height);
  end;
end;

class procedure Global.ReadGrid(const Name: String; Grid: TDBGrid);
var
  Enum: IEnumerator;
  Item: String;
begin
  with Config.GetTag(Str.Format('config/grid/%s/columnsize', [Name]), true) do
    while Data.Enumerate(Enum, Item) do
      if Enum.Iteration < Grid.Columns.Count - 1 then
        Grid.Columns[Enum.Iteration].Width := Str.ToInt(Item, 64);
end;

class procedure Global.WriteGrid(const Name: String; Grid: TDBGrid);
var
  i: Integer;
begin
  with Config.GetTag(Str.Format('config/grid/%s/columnsize', [Name]), true) do
  begin
    Data.Clear;

    for i := 0 to Grid.Columns.Count - 1 do
      Data.Add(Int.ToStr(Grid.Columns[i].Width));
  end;
end;

class procedure Global.ResizeGrid(Grid: TDBGrid);
var
  Sizes: TFloatArray;
  Width, i: Integer;
begin
  SetLength(Sizes, Grid.Columns.Count);
  Width := 0;

  for i := 0 to Length(Sizes) - 1 do
    Inc(Width, Grid.Columns[i].Width);

  for i := 0 to Length(Sizes) - 1 do
    Sizes[i] := Grid.Columns[i].Width * 100 / Width;

  Width := Grid.Width;

  for i := 0 to Length(Sizes) - 1 do
    Grid.Columns[i].Width := Trunc(Sizes[i] * Width / 100);
end;

end.
