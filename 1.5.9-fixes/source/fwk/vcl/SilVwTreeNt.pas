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
                              
unit SilVwTreeNt;

interface

uses
  Sil,
  SilViTreeView,
  TreeNT;

type
  TTreeNode = class;

  TTreeView = class(
    TInterfacedObject,
    ITreeView )
  private
    FView: TTreeNT;
    FOnDeletion: TTVExpandedEvent;
    procedure ViewDeletion(Sender: TObject; Node: TTreeNTNode);
    function AddNode(Parent: TTreeNode; const S: string): TTreeNode;
  protected
    function GetControl: Pointer;
    function Add(const Parent: ITreeNode; const S: string = ''; const O: Pointer = nil): ITreeNode;
  public
    constructor Create(View: TTreeNT);
    destructor Destroy; override;
  end;

  TTreeNode = class(
    TNonCountedObject,
    ITreeNode,
    ITreeNodeOptions )
  private
    FTree: TTreeView;
    FNode: TTreeNTNode;
    FData: Pointer;
  protected
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
  protected
    function GetChecked: Boolean;
    procedure SetChecked(Value: Boolean);
  public
    constructor Create(Tree: TTreeView; Node: TTreeNTNode);
    destructor Destroy; override;
  end;

implementation

{ TTreeView }

constructor TTreeView.Create(View: TTreeNT);
begin
  inherited Create;
  Assert(View <> nil);
  FView := View;
  FOnDeletion := FView.OnDeletion; 
  FView.OnDeletion := ViewDeletion;
end;

destructor TTreeView.Destroy;
begin
  FView.OnDeletion := FOnDeletion;
  FView := nil;
  inherited;
end;

function TTreeView.GetControl: Pointer;
begin
  Result := FView;
end;

function TTreeView.Add(const Parent: ITreeNode; const S: string; const O: Pointer): ITreeNode;
begin
  Result := AddNode(Reference.GetInstance(Parent), S);
end;

procedure TTreeView.ViewDeletion(Sender: TObject; Node: TTreeNTNode);
var
  Temp: TTreeNode;
begin
  if Assigned(FOnDeletion) then FOnDeletion(Sender, Node);
  Temp := Node.Data;
  if Temp <> nil then
  begin
    Temp.FNode := nil;
    Temp.Free;
  end;
end;

function TTreeView.AddNode(Parent: TTreeNode; const S: string): TTreeNode;
var
  Temp: TTreeNTNode;
begin
  if Parent <> nil then
    Temp := FView.Items.AddChild(Parent.FNode, S) else
    Temp := FView.Items.Add(nil, S);
  Result := TTreeNode.Create(Self, Temp);
end;

{ TTreeNode }

constructor TTreeNode.Create(Tree: TTreeView; Node: TTreeNTNode);
begin
  inherited Create;
  Assert(Node <> nil);
  FTree := Tree;
  FNode := Node;
  FNode.Data := Self;
end;

destructor TTreeNode.Destroy;
(*var
  Temp: TTreeNTNode;*)
begin
(*  if FNode <> nil then
  begin
    Temp := FNode;
    FNode.Data := nil;
    FNode := nil;
    Temp.Delete;
  end;*)
  inherited;
end;

function TTreeNode.GetOptions: ITreeNodeOptions;
begin
  Result := Self;
end;

function TTreeNode.GetItem: Pointer;
begin
  Result := FNode;
end;

function TTreeNode.GetCaption: string;
begin
  Result := FNode.Text;
end;

procedure TTreeNode.SetCaption(const Value: string);
begin
  FNode.Text := Value;
end;

function TTreeNode.GetData: Pointer;
begin
  Result := FData;
end;

procedure TTreeNode.SetData(Value: Pointer);
begin
  FData := Value;
end;

function TTreeNode.GetExpanded: Boolean;
begin
  Result := FNode.Expanded;
end;

procedure TTreeNode.SetExpanded(Value: Boolean);
begin
  FNode.Expanded := Value;
end;

function TTreeNode.Add(const S: string; const O: Pointer): ITreeNode;
begin
  Result := FTree.AddNode(Self, S);
end;

procedure TTreeNode.Expand(Recursive: Boolean);
begin
  FNode.Expand(Recursive);
end;

function TTreeNode.GetChecked: Boolean;
begin
  Result := FNode.CheckState = csChecked;
end;

procedure TTreeNode.SetChecked(Value: Boolean);
const State: array[Boolean] of TCheckState = (csUnchecked, csChecked);
begin
  FNode.CheckState := State[Value];
end;

end.
