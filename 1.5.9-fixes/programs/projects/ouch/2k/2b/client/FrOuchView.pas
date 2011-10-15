unit FrOuchView;

interface

uses
  Sil,
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, VirtualTrees, ImgList, ExtCtrls, UiOuch, Menus;

type
  TItemKind = (ikRoot, ikUser);

  TFrameOuchView = class(
    TFrame,
    IOuchView,
    IOuchContactsEvents )
    vtList: TVirtualStringTree;
    imImages: TImageList;
    imStates: TImageList;
    puUser: TPopupMenu;
    miSend: TMenuItem;
    miHistory: TMenuItem;
    miSep: TMenuItem;
    miSendFile: TMenuItem;
    procedure vtListInitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
    procedure vtListFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure vtListGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: WideString);
    procedure vtListCompareNodes(Sender: TBaseVirtualTree; Node1, Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
    procedure vtListPaintText(Sender: TBaseVirtualTree; const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType);
    procedure vtListFocusChanging(Sender: TBaseVirtualTree; OldNode, NewNode: PVirtualNode; OldColumn, NewColumn: TColumnIndex; var Allowed: Boolean);
    procedure vtListBeforeCellPaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; CellRect: TRect);
    procedure vtListBeforeItemErase(Sender: TBaseVirtualTree; Canvas: TCanvas; Node: PVirtualNode; ItemRect: TRect; var ItemColor: TColor; var EraseAction: TItemEraseAction);
    procedure vtListIncrementalSearch(Sender: TBaseVirtualTree; Node: PVirtualNode; const SearchText: WideString; var Result: Integer);
    procedure vtListGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: Integer);
    procedure vtListNodeMoved(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure vtListDblClick(Sender: TObject);
    procedure miSendClick(Sender: TObject);
    procedure miHistoryClick(Sender: TObject);
    procedure miSendFileClick(Sender: TObject);
  private
    FApplication: IOuchApplication;
    FOnlines: PVirtualNode;
    FOfflines: PVirtualNode;
  private
    function DoGetRoot(State: TUserStatus): PVirtualNode;
    function DoGetText(Node: PVirtualNode; const Default: string): String;
    procedure DoAddUser(const User: IOuchAccount);
    procedure DoRemoveUser(const User: IOuchAccount);
    procedure DoUpdateUser(const User: IOuchAccount);
    function DoGetSelection(out List: IOuchAccountList): Boolean;
    procedure DoCompose;
    procedure DoSendFiles;
    procedure DoHistory;
  protected // IOuchView
    procedure BeginUpdate;
    procedure EndUpdate;
  protected // IOuchContactsEvents
    procedure OnUserAdd(const User: IOuchAccount);
    procedure OnUserRemove(const User: IOuchAccount);
    procedure OnUserChanged(const User: IOuchAccount);
  public
    class function Create(Owner: TComponent; Parent: TWinControl): IOuchView; reintroduce; overload;  
    constructor Create(Owner: TComponent); overload; override;
    destructor Destroy; override;
    procedure Loaded; override;
    function Add(Kind: TItemKind; const Text: string; Parent: PVirtualNode = nil): PVirtualNode;
  public
    property Onlines: PVirtualNode read FOnlines;
    property Offlines: PVirtualNode read FOfflines;
  end;

implementation

{$R *.dfm}

uses
  SilVCL, Math, FmOuchHistory;

type
  TVirtualTreeAccess = class(TBaseVirtualTree);

type
  PTreeItem = ^RTreeItem;
  RTreeItem = record
    Kind: TItemKind;
    User: IOuchAccount;
    Root: TUserStatus;
    Text: string;
  end;

class function TFrameOuchView.Create(Owner: TComponent; Parent: TWinControl): IOuchView;
var
  Instance: TFrameOuchView;
begin
  Instance := Create(Owner);
  Instance.Parent := Parent;
  Instance.Visible := True;
  Result := Instance;
end;

constructor TFrameOuchView.Create(Owner: TComponent);
begin
  inherited;
  FApplication := Owner as IOuchApplication;
  VCL.ComObj.Make(Self);
end;

destructor TFrameOuchView.Destroy;
begin
  VCL.ComObj.Free(Self);
  FApplication := nil;
  inherited;
end;

procedure TFrameOuchView.Loaded;
begin
  inherited;
  vtList.NodeDataSize := SizeOf(RTreeItem);
  FOnlines := Add(ikRoot, 'Onlines');
  FOfflines := Add(ikRoot, 'Offlines');
end;

function TFrameOuchView.Add(Kind: TItemKind; const Text: string; Parent: PVirtualNode): PVirtualNode;
var
  Data: PTreeItem;
begin
  Result := vtList.AddChild(Parent);
  vtList.ReinitNode(Result, False);
  Data := vtList.GetNodeData(Result);
  Data.Kind := Kind;
  Data.Text := Text;
end;

procedure TFrameOuchView.vtListInitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
var
  Data: PTreeItem;
begin
  Data := Sender.GetNodeData(Node);
  if Assigned(Data) then System.Initialize(Data^);
end;

procedure TFrameOuchView.vtListFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
var
  Data: PTreeItem;
begin
  Data := Sender.GetNodeData(Node);
  if Assigned(Data) then System.Finalize(Data^);
end;

procedure TFrameOuchView.vtListGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: WideString);
begin
  CellText := DoGetText(Node, CellText);
end;

procedure TFrameOuchView.vtListCompareNodes(Sender: TBaseVirtualTree; Node1, Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
var
  Data1, Data2: PTreeItem;
begin
  Data1 := Sender.GetNodeData(Node1);
  Data2 := Sender.GetNodeData(Node2);
  if Assigned(Data1)
      and Assigned(Data2)
      and (Data1.Kind = ikUser)
      and (Data2.Kind = ikUser) then
    Result := Sil.Str.CompareText(Data1.Text, Data2.Text, True);
end;

procedure TFrameOuchView.vtListPaintText(Sender: TBaseVirtualTree; const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType);
var
  Data: PTreeItem;
begin
  Data := Sender.GetNodeData(Node);
  if Data.Kind = ikRoot then
  begin
    TargetCanvas.Font.Style := [fsBold];
    TargetCanvas.Font.Color := clMaroon;
    TargetCanvas.Font.Size := 10;
  end else
  begin
    TargetCanvas.Font.Style := [fsBold];
    TargetCanvas.Font.Name := 'Tahoma';
    TargetCanvas.Font.Size := 8;
    if Node = Sender.HotNode then
      TargetCanvas.Font.Color := clWhite
    else if Sender.Selected[Node] then
      TargetCanvas.Font.Color := clNavy
    else
      TargetCanvas.Font.Color := clBlack
  end;
end;

procedure TFrameOuchView.vtListFocusChanging(Sender: TBaseVirtualTree; OldNode,
  NewNode: PVirtualNode; OldColumn, NewColumn: TColumnIndex;
  var Allowed: Boolean);
(*)var
  Data: PTreeItem;(*)
begin
//  Data := Sender.GetNodeData(NewNode);
//  Allowed := Data.Kind <> ikRoot;
end;

procedure TFrameOuchView.vtListBeforeCellPaint(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  CellRect: TRect);
var
  Color: TColor;
  Darker, Brighter: TColor;
begin
  Color := TargetCanvas.Brush.Color;
  if Sender.Selected[Node] or (Node = Sender.HotNode) then
  begin
    Darker :=  Sil.Rgb.Darker(Color, 20);
    Brighter :=  Sil.Rgb.Brighter(Color, 20);
    Windows.InflateRect(CellRect, -2, -1);
    Frame3D(TargetCanvas, CellRect, Darker, Brighter, 1);
  end;
end;

procedure TFrameOuchView.vtListBeforeItemErase(Sender: TBaseVirtualTree; Canvas: TCanvas; Node: PVirtualNode; ItemRect: TRect; var ItemColor: TColor; var EraseAction: TItemEraseAction);
var
  Data: PTreeItem;
begin
  Data := Sender.GetNodeData(Node);
  EraseAction := eaColor;
  if (Data.Kind = ikUser) then
    ItemColor := TVirtualTreeAccess(Sender).Color else
    ItemColor := $0095B0CC;

  if (Node = Sender.HotNode) then
    ItemColor := Sil.Rgb.Darker(ItemColor, 10);
end;

procedure TFrameOuchView.vtListIncrementalSearch(Sender: TBaseVirtualTree;
  Node: PVirtualNode; const SearchText: WideString; var Result: Integer);
var
  Data: PTreeItem;
begin
  Data := Sender.GetNodeData(Node);
  if (Data.Kind = ikUser) then
    Result := Sil.Str.CompareText(DoGetText(Node, ''), SearchText) else
    Result := -1;
end;

procedure TFrameOuchView.vtListGetImageIndex(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
  var Ghosted: Boolean; var ImageIndex: Integer);
var
  Data: PTreeItem;
begin
  Data := Sender.GetNodeData(Node);
  case Kind of
    ikNormal:
      if (Data.Kind = ikUser) then
        begin
          if Node <> Sender.HotNode then
          begin
            if Sender.NodeParent[Node] = FOnlines then
              ImageIndex := 0 else
              ImageIndex := 9;
          end else
            ImageIndex := 1;
        end
      else if Node = FOnlines then
        ImageIndex := 4
      else if Node = FOfflines then
        ImageIndex := 3;
    ikSelected:
      if (Data.Kind = ikUser) then
        ImageIndex := 8
      else if Node = FOnlines then
        ImageIndex := 4
      else if Node = FOfflines then
        ImageIndex := 3;
    ikState:
      ImageIndex := -1;
  end;
end;

procedure TFrameOuchView.vtListNodeMoved(Sender: TBaseVirtualTree; Node: PVirtualNode);
var
  Parent: PVirtualNode;
begin
  Parent := Sender.NodeParent[Node];
  if Assigned(Parent) and (Parent = FOnlines) then
  begin
    if not vtList.Expanded[FOnlines] then
      vtList.ToggleNode(FOnlines);
  end;
end;

procedure TFrameOuchView.vtListDblClick(Sender: TObject);
begin
  DoCompose;
end;

procedure TFrameOuchView.miSendClick(Sender: TObject);
begin
  DoCompose;
end;

procedure TFrameOuchView.miSendFileClick(Sender: TObject);
begin
  DoSendFiles;
end;

procedure TFrameOuchView.miHistoryClick(Sender: TObject);
begin
  DoHistory;
end;

procedure TFrameOuchView.BeginUpdate;
begin
  vtList.BeginUpdate;
end;

procedure TFrameOuchView.EndUpdate;
begin
  vtList.EndUpdate;
  vtList.Invalidate;
end;

procedure TFrameOuchView.OnUserAdd(const User: IOuchAccount);
begin
  DoAddUser(User);
end;

procedure TFrameOuchView.OnUserChanged(const User: IOuchAccount);
begin
  DoUpdateUser(User);
end;

procedure TFrameOuchView.OnUserRemove(const User: IOuchAccount);
begin
  DoRemoveUser(User);
end;

procedure TFrameOuchView.DoAddUser(const User: IOuchAccount);
var
  Node: PVirtualNode;
  Item: PTreeItem;
begin
  Node := Add(ikUser, User.Nick, DoGetRoot(User.State));
  Item := vtList.GetNodeData(Node);
  Item.User := User;
  User.Data := Node;
end;

procedure TFrameOuchView.DoRemoveUser(const User: IOuchAccount);
var
  Node: PVirtualNode;
begin
  Node := User.Data;
  User.Data := nil;
  if Assigned(Node) then
    vtList.DeleteNode(Node);
end;

procedure TFrameOuchView.DoUpdateUser(const User: IOuchAccount);
var
  Root, Node: PVirtualNode;
  Item: PTreeItem;
begin
  Node := User.Data;
  if Assigned(Node) then
  begin
    Root := vtList.NodeParent[Node];
    Item := vtList.GetNodeData(Root);
    if Item.Root <> User.State then
    begin
      vtList.InvalidateChildren(Node.Parent, False);
      vtList.NodeParent[Node] := DoGetRoot(User.State);
      vtList.InvalidateChildren(Node.Parent, False);
    end;
  end;
end;

function TFrameOuchView.DoGetRoot(State: TUserStatus): PVirtualNode;
begin
  case State of
    usOnline: Result := FOnlines;
    else      Result := FOfflines;
  end;
end;

function TFrameOuchView.DoGetText(Node: PVirtualNode; const Default: string): String;
var
  Item: PTreeItem;
begin
  Item := vtList.GetNodeData(Node);
  if Assigned(Item) then
    case Item.Kind of
      ikRoot:
          Result := Sil.Str.Format('%s (%d)', [Item.Text, Node.ChildCount]);
      else
          if Assigned(Item.User) then
            Result := Item.User.Nick else
            Result := Item.Text;
    end
  else
    Result := Default;
end;

function TFrameOuchView.DoGetSelection(out List: IOuchAccountList): Boolean;

  procedure DoAdd(Node: PVirtualNode);
  var
    Item: PTreeItem;
  begin
    Item := vtList.GetNodeData(Node);
    if (Item.Kind = ikUser) and Assigned(Item.User) then
      List.Add(Item.User);
  end;

var
  Node: PVirtualNode;
begin
  List := FApplication.Engine.Factory.AccountList();

  with vtList do
  begin
    Node := GetFirstSelected();
    if Assigned(Node) then
      repeat
        DoAdd(Node);
        Node := GetNextSelected(Node);
      until Node = nil
    else if Assigned(HotNode) then
      DoAdd(HotNode)
    else if Assigned(FocusedNode) then
      DoAdd(FocusedNode);
  end;

  Result := List <> nil;
end;

procedure TFrameOuchView.DoCompose;
var
  Contacts: IOuchAccountList;
begin
  if DoGetSelection(Contacts) then
    FApplication.Compose(Contacts);
end;

procedure TFrameOuchView.DoSendFiles;
var
  Contacts: IOuchAccountList;
begin
  if DoGetSelection(Contacts) then
    FApplication.FileTransfer(Contacts);
end;

procedure TFrameOuchView.DoHistory;
var
  Contacts: IOuchAccountList;
  Enum: IEnumerator;
  Item: IOuchAccount;
  Window: IOuchHistoryWindow;
begin
  if DoGetSelection(Contacts) then
    while Contacts.Enumerate(Enum, Item) do
      if FApplication.Windows.Find(IOuchHistoryWindow, Window)
        or FApplication.Windows.Create(TFormOuchHistory, IOuchHistoryWindow, Window) then
      begin
        Window.Account := Item;
        Window.Show;
      end;
end;

end.
