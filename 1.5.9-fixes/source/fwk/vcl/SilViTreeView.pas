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

unit SilViTreeView;

interface

type
  ITreeNode = interface;
  ITreeView = interface
    ['{981E7C21-67FB-11D4-9D6B-00C0DFE46337}']
    function GetControl: Pointer;
    function Add(const Parent: ITreeNode; const S: string = ''; const O: Pointer = nil): ITreeNode;
    property Control: Pointer read GetControl;
  end;

  ITreeNodeOptions = interface;
  
  ITreeNode = interface
    ['{981E7C22-67FB-11D4-9D6B-00C0DFE46337}']
    function GetItem: Pointer;
    function GetCaption: string;
    procedure SetCaption(const Value: string);
    function GetData: Pointer;
    procedure SetData(Value: Pointer);
    function GetOptions: ITreeNodeOptions;
    function GetExpanded: Boolean;
    procedure SetExpanded(Value: Boolean);
    function Add(const S: string = ''; const O: Pointer = nil): ITreeNode;
    procedure Expand(Recursive: Boolean);
    property Item: Pointer read GetItem;
    property Caption: string read GetCaption write SetCaption;
    property Data: Pointer read GetData write SetData;
    property Options: ITreeNodeOptions read GetOptions;
    property Expanded: Boolean read GetExpanded write SetExpanded; 
  end;

  ITreeNodeOptions = interface
    ['{6475A371-6B12-11D4-9D76-00C0DFE46337}']
    function GetChecked: Boolean;
    procedure SetChecked(Value: Boolean);
    property Checked: Boolean read GetChecked write SetChecked;
  end;

implementation
end.
 