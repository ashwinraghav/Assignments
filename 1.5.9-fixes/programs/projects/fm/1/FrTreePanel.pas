unit FrTreePanel;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, VirtualTrees, Sil, SilTool, UiDefs;

const
  CThreadLoader = EV_FIRST + 1;

type
  PData = ^RData;

  IFmTreeItem = interface (IFmTreeNode)
    ['{BCA4F738-F70A-4697-BADC-FABE2BA203B5}']
    procedure SetText(const Value: string);
    procedure SetInfo(const Value: IFileInfo);
    function GetDummy: PVirtualNode;
    procedure SetDummy(const Value: PVirtualNode);
    function GetNode: PVirtualNode;
    property Text: string read GetText write SetText;
    property Info: IFileInfo read GetInfo write SetInfo;
    property Node: PVirtualNode read GetNode;
    property Dummy: PVirtualNode read GetDummy write SetDummy;
  end;

  RData = record
    Item: IFmTreeItem;
  end;

  TFrameTree = class(
    TFrame,
    IDispatchable,
    IFmTreeView,
    IConnectable )
    vwTree: TVirtualStringTree;
    procedure vwTreeGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: WideString);
    procedure vwTreeFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure vwTreeInitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
    procedure vwTreeExpanding(Sender: TBaseVirtualTree; Node: PVirtualNode; var Allowed: Boolean);
    procedure vwTreeFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex);
  private
    FEvents: IEventList;
    FQueue: IInterfaceQueue;
    FLoader: IThread;
  private
    procedure DoAddRoot(const Text: string);
    procedure DoAddTree(Node: PVirtualNode);
    function DoAddNode(Parent: PVirtualNode; const Text: string = ''): PVirtualNode;
    procedure DoAddItem(Parent: PVirtualNode; const Info: IFileInfo = nil; const Text: string = ''); overload; 
    procedure DoAddItem(Parent: PVirtualNode; const Text: string); overload; 
    procedure DoThreadLoader(var Msg: RThreadRunMessage); message CThreadLoader;
    procedure DoAddChilds(const Item: IUnknown; Param: Pointer); overload;
    procedure DoAddChilds(Node: PVirtualNode; const List: IFileInfoList); overload;
    procedure DoDeleteChilds(const Item: IUnknown; Param: Pointer); overload;
    procedure DoDeleteChilds(Node: PVirtualNode); overload;
    procedure DoDeleteNode(const Item: IUnknown; Param: Pointer); overload;
    procedure DoDeleteNode(Node: PVirtualNode); overload;
    procedure DoLoadTree(const Item: IFmTreeItem); overload;
    procedure DoLoadTree(Node: PVirtualNode); overload;
    procedure DoFireSelected(const Item: IFmTreeItem);
  protected // IConnectable
    procedure AddListener(const Listener: IUnknown; KeepRef: Boolean = true);
    procedure RemoveListener(const Listener: IUnknown);
  protected // IFmTreeView
  public
    constructor Create(Owner: TComponent); override;
    destructor Destroy; override;
  end;

  TFmTreeItem = class(
    TSilObject,
    IFmTreeNode,
    IFmTreeItem )
  private
    FText: string;
    FInfo: IFileInfo;
    FNode: PVirtualNode;
    FDummy: PVirtualNode;
  protected // IFmTreeNode
    function GetText: string;
    function GetInfo: IFileInfo;
  protected // IFmTreeItem
    procedure SetText(const Value: string);
    procedure SetInfo(const Value: IFileInfo);
    function GetDummy: PVirtualNode;
    procedure SetDummy(const Value: PVirtualNode);
    function GetNode: PVirtualNode;
  public
    constructor Create(Node: PVirtualNode);
    destructor Destroy; override;
  end;

implementation

{$R *.dfm}

{ TFrameTree }

constructor TFrameTree.Create(Owner: TComponent);
begin
  inherited;
  FQueue := Sil.Tk.InterfaceQueue();
  FLoader := Sil.Os.Thread.Spawn(CThreadLoader, Self);
  vwTree.NodeDataSize := SizeOf(RData);
  DoAddRoot('D:\');
end;

destructor TFrameTree.Destroy;
begin
  if Assigned(FQueue) then FQueue.Cancel;
  if Assigned(FLoader) then FLoader.Termination.WaitFor(INFINITE, True);
  FQueue := nil;
  FLoader := nil;
  inherited;
end;

procedure TFrameTree.DoThreadLoader(var Msg: RThreadRunMessage);
var
  Item: IFmTreeItem;
begin
  while Assigned(FQueue) and FQueue.Get(IFmTreeItem, Item) do
    DoLoadTree(Item);
end;

procedure TFrameTree.DoAddChilds(const Item: IInterface; Param: Pointer);
var
  List: IFileInfoList absolute Item;
  Node: PVirtualNode absolute Param;
begin
  DoAddChilds(Node, List);
end;

procedure TFrameTree.DoAddChilds(Node: PVirtualNode; const List: IFileInfoList);
var
  Enum: IEnumerator;
  Info: IFileInfo;
begin
  with List do
    while Enumerate(Enum, Info) do
      if (faDirectory in Info.Attributes) then
        DoAddItem(Node, Info);
end;

procedure TFrameTree.DoDeleteChilds(Node: PVirtualNode);
begin
  vwTree.DeleteChildren(Node);
end;

procedure TFrameTree.DoDeleteChilds(const Item: IInterface; Param: Pointer);
begin
  DoDeleteChilds(Param);
end;

procedure TFrameTree.DoDeleteNode(const Item: IInterface; Param: Pointer);
begin
  DoDeleteNode(Param);
end;

procedure TFrameTree.DoDeleteNode(Node: PVirtualNode);
begin
  vwTree.DeleteNode(Node);
end;

procedure TFrameTree.DoLoadTree(const Item: IFmTreeItem);
begin
  DoLoadTree(Item.Node);
end;

procedure TFrameTree.DoLoadTree(Node: PVirtualNode);
var
  Data: PData;
  Reader: IDirectoryReader;
  Path: string;
begin
  Data := vwTree.GetNodeData(Node);
  if Assigned(Data.Item.Info) then
    Path := Data.Item.Info.FullName else
    Path := Data.Item.Text;
    
  Reader := Sil.Os.Filesystem.ReadDirectory(Path);
  Reader.BufferSize := 25;
  
  with Reader do
    while Read do
      Sil.Os.Thread.SyncCall(DoAddChilds, Recent, Node);

  if Assigned(Data.Item.Dummy) then
  begin
    Sil.Os.Thread.SyncCall(DoDeleteNode, Data.Item.Dummy);
    Data.Item.Dummy := nil;
  end;

end;

function TFrameTree.DoAddNode(Parent: PVirtualNode; const Text: string): PVirtualNode;
var
  Data: PData;
begin
  Result := vwTree.AddChild(Parent);
  vwTree.ReinitNode(Result, False);
  vwTree.NodeParent[Result] := Parent;
  Data := vwTree.GetNodeData(Result);
  Data.Item.Text := Text;
end;

procedure TFrameTree.DoAddItem(Parent: PVirtualNode; const Info: IFileInfo; const Text: string);
var
  Node: PVirtualNode;
  Data: PData;
begin
  Node := DoAddNode(Parent);
  Data := vwTree.GetNodeData(Node);
  Data.Item.Info := Info;
  if Assigned(Info) then
    Data.Item.Text := Info.Name else
    Data.Item.Text := Text;
  Data.Item.Dummy := DoAddNode(Node, 'leyendo ...');
end;

procedure TFrameTree.DoAddItem(Parent: PVirtualNode; const Text: string);
begin
  DoAddItem(Parent, nil, Text);
end;

procedure TFrameTree.DoAddRoot(const Text: string);
begin
  DoAddItem(vwTree.RootNode, Text);
end;

procedure TFrameTree.DoAddTree(Node: PVirtualNode);
begin
  if Assigned(FQueue) then FQueue.Put(TFmTreeItem.Create(Node))
end;

procedure TFrameTree.AddListener(const Listener: IInterface; KeepRef: Boolean);
begin
  SilTool.Sv.Events.Add(FEvents, Listener, KeepRef);
end;

procedure TFrameTree.RemoveListener(const Listener: IInterface);
begin
  SilTool.Sv.Events.Remove(FEvents, Listener);
end;

procedure TFrameTree.DoFireSelected(const Item: IFmTreeItem);
var
  Enum: IEnumerator;
  Sink: IFmTreeOnSelected;
begin
  if Assigned(FEvents) then
    with FEvents do
      while Enumerate(Enum, Sink, IFmTreeOnSelected) do
        Sink.OnTreeSelected(Self, Item);
end;

procedure TFrameTree.vwTreeInitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
var
  Data: PData;
begin
  Data := vwTree.GetNodeData(Node);
  System.Initialize(Data^);
  Data.Item := TFmTreeItem.Create(Node);
end;

procedure TFrameTree.vwTreeFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
var
  Data: PData;
begin
  Data := vwTree.GetNodeData(Node);
  System.Finalize(Data^);
end;

procedure TFrameTree.vwTreeGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: WideString);
var
  Data: PData;
begin
  Data := vwTree.GetNodeData(Node);
  if Assigned(Data.Item.Info) then
    CellText := Data.Item.Info.Name else
    CellText := Data.Item.Text;
end;

procedure TFrameTree.vwTreeExpanding(Sender: TBaseVirtualTree; Node: PVirtualNode; var Allowed: Boolean);
var
  Data: PData;
begin
  Data := vwTree.GetNodeData(Node);
  if Assigned(Data.Item.Dummy) then
    DoAddTree(Node);
  Allowed := True;
end;

procedure TFrameTree.vwTreeFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex);
var
  Data: PData;
begin
  Data := vwTree.GetNodeData(Node);
  DoFireSelected(Data.Item);
end;

{ TFmTreeItem }

constructor TFmTreeItem.Create(Node: PVirtualNode);
begin
  inherited Create;
  FNode := Node;
end;

destructor TFmTreeItem.Destroy;
begin
  inherited;
end;

function TFmTreeItem.GetNode: PVirtualNode;
begin
  Result := FNode;
end;

function TFmTreeItem.GetText: string;
begin
  Result := FText;
end;

function TFmTreeItem.GetInfo: IFileInfo;
begin
  Result := FInfo;
end;

function TFmTreeItem.GetDummy: PVirtualNode;
begin
  Result := FDummy;
end;

procedure TFmTreeItem.SetDummy(const Value: PVirtualNode);
begin
  FDummy := Value;
end;

procedure TFmTreeItem.SetInfo(const Value: IFileInfo);
begin
  FInfo := Value;
end;

procedure TFmTreeItem.SetText(const Value: string);
begin
  FText := Value;
end;

end.
