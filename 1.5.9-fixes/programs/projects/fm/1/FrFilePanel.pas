unit FrFilePanel;

interface

uses
  Windows, ShellApi, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, VirtualTrees, ImgList, Sil, UiDefs, UiWorker, ExtCtrls, StdCtrls;

const
  CFileLoader = EV_FIRST + 2;
  CIconLoader = EV_FIRST + 3;

type
  IFmFrameFiles = interface;
  IFmFileItem = interface;

  PData = ^RData;

  IFmFrameFiles = interface
    ['{D721614E-C8DA-4CBA-A0AE-2B9A4A7D6202}']
    procedure DoLoadFiles(const Item: IFmTreeNode);
    procedure DoLoadIcon(const Item: IFmFileItem);
  end;

  IFmFileItem = interface (IFmFileNode)
    ['{197F5B18-B4EF-415E-9E63-85AD13324B7E}']
    function GetNode: PVirtualNode;
    procedure SetNode(Value: PVirtualNode);
    property Node: PVirtualNode read GetNode write SetNode;
  end;

  TFrameFiles = class(
    TFrame,
    IFmFrameFiles,
    IDispatchable,
    IFmTreeOnSelected )
    vwList: TVirtualStringTree;
    imFileIcons: TImageList;
    pnPath: TPanel;
    stPath: TEdit;
    procedure vwListInitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
    procedure vwListFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure vwListGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: WideString);
    procedure vwListGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: Integer);
  private
    FWorker: IFmWorkerThread;
  private
    function DoAddNode(const Info: IFileInfo): PVirtualNode;
    procedure DoAddItem(const Info: IFileInfo);
    procedure DoAddItems(const Item: IUnknown; Param: Pointer); overload;
    procedure DoAddItems(const List: IFileInfoList); overload;
    procedure DoDeleteItems(const Item: IUnknown; Param: Pointer); overload;
    procedure DoDeleteItems; overload;
    function DoGetIcon(Handle: THandle): Integer;
    procedure DoLoadIcon(const Item: IFmFileItem); overload;
    procedure DoInvalidateNode(const Item: IUnknown; Param: Pointer); overload;
    procedure DoInvalidateNode(Node: PVirtualNode); overload;
  protected // IFmFrameFiles
    procedure DoLoadFiles(const Item: IFmTreeNode);
    function DoLoadIcon(const Info: IFileInfo): THandle; overload;
  protected // IFmTreeOnSelected
    procedure OnTreeSelected(const Sender: IFmTreeView; const Item: IFmTreeNode);
  public
    constructor Create(Owner: TComponent); override;
    destructor Destroy; override;
  end;

  RData = record
    Item: IFmFileItem;
    IconIndex: Integer;
    IconHandle: THandle;
  end;

  TFmFileItem = class(
    TSilObject,
    IFmFileNode,
    IFmFileItem )
  private
    FInfo: IFileInfo;
    FNode: PVirtualNode;
  protected // IFmFileNode
    function GetInfo: IFileInfo;
  protected // IFmFileItem 
    function GetNode: PVirtualNode;
    procedure SetNode(Value: PVirtualNode);
  public
    constructor Create(Node: PVirtualNode; const Info: IFileInfo);
    destructor Destroy; override; 
  end;

implementation

uses
  UmWorker, SilOiFile;

{$R *.dfm}

type
  TFmWorkLoadFile = class(
    TSilObject,
    IFmWorkerItem )
  protected // IFmWorkerItem
    procedure Execute(const Owner, Data: IUnknown; Param: Pointer; out Result: LongWord);
  end;

  TFmWorkLoadIcon = class(
    TSilObject,
    IFmWorkerItem )
  protected // IFmWorkerItem
    procedure Execute(const Owner, Data: IUnknown; Param: Pointer; out Result: LongWord);
  end;

{ TFmWorkLoadFile }

procedure TFmWorkLoadFile.Execute(const Owner, Data: IUnknown; Param: Pointer; out Result: LongWord);
begin
  IFmFrameFiles(Owner).DoLoadFiles(IFmTreeNode(Data));
end;

{ TFmWorkLoadIcon }

procedure TFmWorkLoadIcon.Execute(const Owner, Data: IInterface; Param: Pointer; out Result: LongWord);
begin
  IFmFrameFiles(Owner).DoLoadIcon(IFmFileItem(Data))
end;

{ TFmFileItem }

constructor TFmFileItem.Create(Node: PVirtualNode; const Info: IFileInfo);
begin
  inherited Create;
  FInfo := Info;
  FNode := Node;
end;

destructor TFmFileItem.Destroy;
begin
  FInfo := nil;
  inherited;
end;

function TFmFileItem.GetInfo: IFileInfo;
begin
  Result := FInfo;
end;

function TFmFileItem.GetNode: PVirtualNode;
begin
  Result := FNode;
end;

procedure TFmFileItem.SetNode(Value: PVirtualNode);
begin
  FNode := nil;
end;

{ TFrameFiles }

constructor TFrameFiles.Create(Owner: TComponent);
begin
  inherited;
  FWorker := TFmWorkerThread.Create(IFmFrameFiles(Self));
  vwList.NodeDataSize := SizeOf(RData);
end;

destructor TFrameFiles.Destroy;
begin
  if Assigned(FWorker) then FWorker.Terminate;
  FWorker := nil;
  inherited;
end;

function TFrameFiles.DoAddNode(const Info: IFileInfo): PVirtualNode;
var
  Data: PData;
begin
  Result := vwList.AddChild(nil);
  vwList.ReinitNode(Result, False);
  Data := vwList.GetNodeData(Result);
  Data.Item := TFmFileItem.Create(Result, Info);
end;

procedure TFrameFiles.DoAddItem(const Info: IFileInfo);
begin
  DoAddNode(Info);
end;

procedure TFrameFiles.DoLoadFiles(const Item: IFmTreeNode);
var
  Reader: IDirectoryReader;
  Path: string;
begin
  Sil.Trace.Enter('TFrameFiles.DoLoadFiles');
  
  if Assigned(Item.Info) then
    Path := Item.Info.FullName else
    Path := Item.Text;

  Reader := Sil.Os.Filesystem.ReadDirectory(Path);
  Reader.BufferSize := 25;

  Sil.Os.Thread.SyncCall(DoDeleteItems);

  with Reader do
    while Read do
      Sil.Os.Thread.SyncCall(DoAddItems, Recent);
      
  Sil.Trace.Leave;
end;

procedure TFrameFiles.DoAddItems(const Item: IInterface; Param: Pointer);
var
  List: IFileInfoList absolute Item;
begin
  DoAddItems(List);
end;

procedure TFrameFiles.DoAddItems(const List: IFileInfoList);
var
  Enum: IEnumerator;
  Info: IFileInfo;
begin
  Sil.Trace.Enter('TFrameFiles.DoAddItems');
  
  with List do
    while Enumerate(Enum, Info) do
      if not (faDirectory in Info.Attributes) then
        DoAddItem(Info);
        
  Sil.Trace.Leave;
end;

procedure TFrameFiles.DoDeleteItems(const Item: IInterface; Param: Pointer);
begin
  Sil.Trace.Enter('TFrameFiles.DoDeleteItems');
  
  DoDeleteItems();
  
  Sil.Trace.Leave;
end;

procedure TFrameFiles.DoDeleteItems;
begin
  Sil.Trace.Enter('TFrameFiles.DoDeleteItems');
  
  vwList.Clear;
  
  Sil.Trace.Leave;
end;

function TFrameFiles.DoGetIcon(Handle: THandle): Integer;
var
  NewIcon: TIcon;
begin
  Sil.Trace.Enter('TFrameFiles.DoGetIcon');
  
  if Handle <> 0 then
  begin
    NewIcon := TIcon.Create;
    NewIcon.Handle := Handle;
    Result := imFileIcons.AddIcon(NewIcon);
  end else
    Result := -1;
    
  Sil.Trace.Leave;
end;

function TFrameFiles.DoLoadIcon(const Info: IFileInfo): THandle;
var
  Buffer: array[0..MAX_PATH] of Char;
  Dummy: Word;
begin
  Sil.Trace.Enter('TFrameFiles.DoLoadIcon');
  
  Sil.Str.Move(Info.FullName, @Buffer, SizeOf(Buffer));
  Result := ShellApi.ExtractAssociatedIcon(HInstance, @Buffer, Dummy);
  
  Sil.Trace.Leave;
end;

procedure TFrameFiles.DoLoadIcon(const Item: IFmFileItem);
var
  Data: PData;
begin
  Sil.Trace.Enter('TFrameFiles.DoLoadIcon');

  if Assigned(Item.Node) then
  begin
    Data := vwList.GetNodeData(Item.Node);
    if Assigned(Data.Item.Info) then
    begin
      Data.IconHandle := DoLoadIcon(Data.Item.Info);
      Sil.Os.Thread.SyncCall(DoInvalidateNode, Item.Node);
    end;
  end;

  Sil.Trace.Leave;
end;

procedure TFrameFiles.DoInvalidateNode(const Item: IInterface; Param: Pointer);
begin
  Sil.Trace.Enter('TFrameFiles.DoInvalidateNode');
  
  if Assigned(Param) then DoInvalidateNode(Param);
  
  Sil.Trace.Leave;
end;

procedure TFrameFiles.DoInvalidateNode(Node: PVirtualNode);
begin
  Sil.Trace.Enter('TFrameFiles.DoInvalidateNode');
  
  vwList.InvalidateNode(Node);
  
  Sil.Trace.Leave;
end;

procedure TFrameFiles.OnTreeSelected(const Sender: IFmTreeView; const Item: IFmTreeNode);
begin
  Sil.Trace.Enter('TFrameFiles.OnTreeSelected');

  vwList.Clear;
  
  if Assigned(Item.Info) then
    stPath.Text := Item.Info.FullName else
    stPath.Text := Item.Text;
    
  FWorker.Post(TFmWorkLoadFile.Create(), Item);

  Sil.Trace.Leave;
end;

procedure TFrameFiles.vwListInitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
var
  Data: PData;
begin
  Data := vwList.GetNodeData(Node);
  System.Initialize(Data^);
  Data.IconIndex := -1;
  Data.IconHandle := 0;
end;

procedure TFrameFiles.vwListFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
var
  Data: PData;
begin
  Data := vwList.GetNodeData(Node);
  if Assigned(Data.Item) then Data.Item.Node := nil;
  System.Finalize(Data^);
end;

procedure TFrameFiles.vwListGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: WideString);
var
  Data: PData;
begin
  Data := vwList.GetNodeData(Node);
  if Assigned(Data.Item.Info) then
    case Column of
      0:    CellText := Data.Item.Info.Name;
      1:    CellText := Sil.Os.FileSystem.GetFileExt(Data.Item.Info.Name);
      2:    CellText := Sil.Large.BytesToStr(Data.Item.Info.Size);
      3:    CellText := '';
      4:    CellText := Sil.DateTime.ToStr(Data.Item.Info.Time);
      5:    CellText := '';
      else  CellText := '';
    end;
end;

procedure TFrameFiles.vwListGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: Integer);
var
  Data: PData;
begin
  if Column = 0 then
  begin
    Data := vwList.GetNodeData(Node);
    if (Data.IconIndex = -1) then
      if (Data.IconHandle <> 0) then
        Data.IconIndex := DoGetIcon(Data.IconHandle) else
        FWorker.Post(TFmWorkLoadIcon.Create(), Data.Item);
    ImageIndex := Data.IconIndex;
  end;
end;

end.
