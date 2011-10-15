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

unit SilViProfileEditor;

interface

uses
  SilViProfileDocument,
  SilViTreeView;

type
  IProfileEditorLevel = interface;
  IProfileEditorBind = interface;
  IProfileEditorEvents = interface;

  IProfileEditor = interface
    function GetRoot: IProfileEditorLevel;
    function BuildDocument: IProfileDocument;
    procedure ApplyDocument(const Document: IProfileDocument);
    property Root: IProfileEditorLevel read GetRoot;
    property Document: IProfileDocument read BuildDocument write ApplyDocument;
  end;

  IProfileEditorItem = interface
    ['{6475A374-6B12-11D4-9D76-00C0DFE46337}']
    function GetNode: ITreeNode;
    property Node: ITreeNode read GetNode;
  end;

  IProfileEditorLevel = interface(IProfileEditorItem)
    ['{981E7C23-67FB-11D4-9D6B-00C0DFE46337}']
    function Add(const Text: string): IProfileEditorLevel;
    function Bind(const Text: string; const OwnerName, ObjectName, PropertyName: string): IProfileEditorBind;
   end;

  IProfileEditorBind = interface(IProfileEditorItem)
    ['{6475A373-6B12-11D4-9D76-00C0DFE46337}']
    function GetValue: Variant;
    procedure SetValue(const Value: Variant);
    property Value: Variant read GetValue write SetValue;
  end;

  IProfileEditorEvents = interface
    ['{6475A372-6B12-11D4-9D76-00C0DFE46337}']
    procedure OnNewLevel(const Level: IProfileEditorLevel);
    procedure OnNewBind(const Bind: IProfileEditorBind);
  end;

implementation
end.
