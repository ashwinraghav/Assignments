unit FrPanel;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ImgList, ComCtrls, StdCtrls, ExtCtrls, ShellApi, VirtualTrees,

  Sil,
  SilLayer,

  UiFileServer;

type
  PData = ^RData;

  RData = record
    IconIndex: Integer;
    IconHandle: THandle;
    Info: IFileInfo;
  end;

  TfaPanel = class(TFrame, ILayerConnectionManager)
    paBar: TPanel;
    Splitter1: TSplitter;
    edPath: TEdit;
    ImgSmall: TImageList;
    sbStatus: TStatusBar;
    btGo: TButton;
    tiReader: TTimer;
    lvFile: TVirtualStringTree;
    imFileIcons: TImageList;
    tvFolder: TVirtualStringTree;
    procedure btGoClick(Sender: TObject);
    procedure edPathKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure tiReaderTimer(Sender: TObject);
    procedure lvFileInitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
    procedure lvFileFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure lvFileGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: WideString);
    procedure lvFileGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: Integer);
    procedure lvFileCompareNodes(Sender: TBaseVirtualTree; Node1, Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
    procedure tvFolderFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure tvFolderInitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
    procedure tvFolderGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: WideString);
    procedure tvFolderGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: Integer);
    procedure tvFolderExpanding(Sender: TBaseVirtualTree; Node: PVirtualNode; var Allowed: Boolean);
    procedure tvFolderFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex);
    procedure tvFolderKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
  protected // ILayerConnectionManager
    procedure Initialize(const Connection: ILayerConnection; const Chain: ILayerChain);
    procedure Connected(const Connection: ILayerConnection);
    procedure Disconnected(const Connection: ILayerConnection);
    procedure ConnectFailed(const Connection: ILayerConnection);
  private
    FFSys: IClientSideFileProtocol;
    FReader: IDirectoryReader;
    FIsLocal: Boolean;
    FClient: IFileClient;
    FSeparator: Char;
    FCurrentNode: PVirtualNode;
    FTotalSize: LargeInt;
    procedure DoReadFiles(Node: PVirtualNode);
    function DoGetList(const PathName: String; const Attributes: TFileAttributes; ChunkCount: Integer): IFileInfoList;
    procedure DoReadFilesChunk;
    procedure DoObjectCount(Count: Integer; const Size: LargeInt);
    //procedure DoLoadIcon(Data: PData);
    function DoAddNode(Sender: TVirtualStringTree; Parent: PVirtualNode; const Info: IFileInfo): PVirtualNode;
    function DoGetRemoteFSys(const Url: String): IClientSideFileProtocol;
    procedure DoStop;
  private
    function DoBuildPath(Node: PVirtualNode; CheckRoot: Boolean = false): String;
    procedure DoReadPath(Node: PVirtualNode; const Path: String);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

uses
  UmFileClient, UmLocalFSys;

{$R *.dfm}

(*)
  silfp://host[:port]
(*)

function DoGetPath(const Path: String): String;
// /xxx/xx
// //xxx/xx
begin
  if Str.Left(Path, 2) = '//' then
    Result := Str.Copy(Path, Str.Pos('/', Path, 3)) else
    Result := Path;
end;

constructor TfaPanel.Create(AOwner: TComponent);
begin
  inherited;

  FCurrentNode := nil;
  tvFolder.NodeDataSize := SizeOf(RData);
  lvFile.NodeDataSize := SizeOf(RData);

  //FFSys := TLocalFSys.Create;
end;

destructor TfaPanel.Destroy;
begin
  DoStop;
  inherited;
end;

procedure TfaPanel.DoStop;
begin
  if Assigned(FClient) then
  begin
    FClient.Stop;
    FClient := nil;
  end;
end;

function TfaPanel.DoGetRemoteFSys(const Url: String): IClientSideFileProtocol;
var
  Params: IParameterList;
begin
  DoStop;

  Params := Sil.List.Parameters;
  Params['url'] := Url + ':25670';

  FClient := TFileClient.Create(Self);
  FClient.Start(Params);
  FClient.WaitConnect(INFINITE);

  Result := FClient.Protocol;
end;

function TfaPanel.DoBuildPath(Node: PVirtualNode; CheckRoot: Boolean): String;
var
  Data: PData;
begin
  Result := '';

  repeat
    Data := tvFolder.GetNodeData(Node);
    if not Assigned(Data) then Break;
    Result := Sil.OS.Fsys.AddSlash(Data.Info.Name, FSeparator) + Result;
    Node := Node.Parent;
  until not Assigned(Node);

  Result := DoGetPath(Result);
  //if not FIsLocal and not CheckRoot then
    //Result := Str.Copy(Result, Str.Pos('/', Result, 3));
end;

procedure TfaPanel.btGoClick(Sender: TObject);
var
  Info: IFileInfoDef;
  IsLocal: Boolean;
begin
  tvFolder.Clear;

  IsLocal := Str.Pos('//', edPath.Text) = 0;

  if (IsLocal <> FIsLocal) or not Assigned(FFSys) then
  begin
    if IsLocal then
    begin
      FSeparator := CPathSeparator;
      FFSys := TLocalFSys.Create;
    end else
    begin
      FSeparator := '/';
      FFSys := DoGetRemoteFSys(edPath.Text);
    end;

    FIsLocal := IsLocal;
  end;

  edPath.Text := Sil.OS.Fsys.AddSlash(edPath.Text, FSeparator);

  if FIsLocal then
    Info := Sil.OS.FSys.FileInfo(Sil.OS.Fsys.DeleteSlash(edPath.Text)) else
    Info := Sil.OS.FSys.FileInfo(DoGetPath(edPath.Text));

  DoReadPath(DoAddNode(tvFolder, tvFolder.RootNode, Info), edPath.Text);
end;

function TfaPanel.DoGetList(const PathName: String; const Attributes: TFileAttributes; ChunkCount: Integer): IFileInfoList;
var
  Reader: IDirectoryReader;
  perf: IPerformanceCounter;
begin
  perf := Sil.OS.Performance.Create;

  Reader := FFSys.ReadDirectory(PathName, Attributes);
  Reader.BufferSize := ChunkCount;

  sbStatus.Panels[2].Text := Float.ToStr(perf.ToMilliseconds);

  while Reader.Read do;

  Result := Reader.List;
end;

procedure TfaPanel.DoReadPath(Node: PVirtualNode; const Path: String);
var
  Enum: IEnumerator;
  List: IFileInfoList;
  Info: IFileInfo;
  Child: PVirtualNode;
begin
  tvFolder.BeginUpdate;

  tvFolder.DeleteChildren(Node, true);

  if FIsLocal then
    List := DoGetList(Path, [faDirectory], 0) else
    List := DoGetList(DoGetPath(Path), [faDirectory], 0);

  while List.Enumerate(Enum, Info) do
  begin
    Child := DoAddNode(tvFolder, Node, Info);

    if DoGetList(Info.FullName, [faDirectory], 1).Count > 0 then
      DoAddNode(tvFolder, Child, nil);
  end;

  tvFolder.EndUpdate;
end;

procedure TfaPanel.tvFolderFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex);
begin
  edPath.Text := DoBuildPath(Node, true);
  FCurrentNode := Node;
  DoReadFiles(Node);
end;

procedure TfaPanel.tvFolderKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Assigned(FCurrentNode) then
  begin
    DoReadFiles(FCurrentNode);
    FCurrentNode := nil;
  end;
end;

procedure TfaPanel.tvFolderExpanding(Sender: TBaseVirtualTree; Node: PVirtualNode; var Allowed: Boolean);
begin
  DoReadPath(Node, DoBuildPath(Node));
end;

procedure TfaPanel.DoReadFiles(Node: PVirtualNode);
{var
  Item: TListItem;}
begin
  lvFile.Clear;

  {if (tvFolder.Selected <> nil) and (tvFolder.Selected.Level > 0) then
  begin
    Item := lvFile.Items.Add;
    Item.Caption := '..';
    Item.ImageIndex := 2;
  end;}

  FReader := FFSys.ReadDirectory(DoBuildPath(Node));
  FReader.BufferSize := Int.IIf(FIsLocal, 1000, 100);

  FTotalSize := 0;

  DoReadFilesChunk;
end;

procedure TfaPanel.edPathKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_RETURN then btGo.Click;
end;

procedure TfaPanel.tiReaderTimer(Sender: TObject);
begin
  DoReadFilesChunk;
end;

procedure TfaPanel.DoReadFilesChunk;
var
  Enum: IEnumerator;
  FileInfo: IFileInfo;
//  TotalSize: LargeInt;
begin
  if Assigned(FReader) then
  begin
    lvFile.BeginUpdate;
    if FReader.Read then
    begin
      while FReader.Recent.Enumerate(Enum, FileInfo) do
      begin
        DoAddNode(lvFile, nil, FileInfo);
        Inc(FTotalSize, FileInfo.Size);
      end;

      lvFile.Sort(nil, 0, sdAscending);
    end;

    DoObjectCount(FReader.List.Count, FTotalSize);
    tiReader.Enabled := not FReader.IsComplete;

    lvFile.EndUpdate;
  end;
end;

procedure TfaPanel.DoObjectCount(Count: Integer; const Size: LargeInt);
begin
  sbStatus.Panels[0].Text := Int.ToStr(Count) + ' Objeto(s)';
  sbStatus.Panels[1].Text := Large.BytesToStr(Size);
end;

function TfaPanel.DoAddNode(Sender: TVirtualStringTree; Parent: PVirtualNode; const Info: IFileInfo): PVirtualNode;
var
  Data: PData;
begin
  Result := Sender.AddChild(Parent);
  Sender.ReinitNode(Result, false);

  // vwTree.NodeParent[Result] := Parent;

  Data := Sender.GetNodeData(Result);
  Data.Info := Info;
end;

procedure TfaPanel.lvFileInitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
var
  Data: PData;
begin
  Data := lvFile.GetNodeData(Node);

  System.Initialize(Data^);
  Data.IconIndex := -1;
  Data.IconHandle := 0;
end;

procedure TfaPanel.lvFileFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
var
  Data: PData;
begin
  Data := lvFile.GetNodeData(Node);

  if Assigned(Data.Info) then Data.Info := nil;
  System.Finalize(Data^);
end;

procedure TfaPanel.lvFileGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: WideString);
var
  Data: PData;
begin
  Data := lvFile.GetNodeData(Node);

  if Assigned(Data.Info) then
    case Column of
      0:    CellText := Data.Info.Name;
      1:    CellText := Str.IIf(faDirectory in Data.Info.Attributes, '', Sil.Large.BytesToStr(Data.Info.Size));
      2:    CellText := Sil.DateTime.ToStr(Data.Info.Time);
      else  CellText := '';
    end;
end;

procedure TfaPanel.lvFileGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: Integer);
var
  Data: PData;
  FileExt: String;
begin
  if Column = 0 then
  begin
    Data := lvFile.GetNodeData(Node);

    //if Data.IconIndex = -1 then
      //DoLoadIcon(Data);

    if faDirectory in Data.Info.Attributes then
      Data.IconIndex := 1 else
    begin
      FileExt := ExtractFileExt(Data.Info.Name);

      if (CompareText(FileExt, '.com') = 0) or (CompareText(FileExt, '.exe') = 0) then
        Data.IconIndex := 2 else
        Data.IconIndex := 3;
    end;

    ImageIndex := Data.IconIndex;
  end;
end;

(*)procedure TfaPanel.DoLoadIcon(Data: PData);
var
  FileName: String;
  Buffer: array[0..MAX_PATH] of Char;
  Dummy: Word;
  NewIcon: TIcon;
begin
  FileName := Data.Info.FullName;
  Sil.Str.Move(FileName, @Buffer, Length(FileName) + 1);

  NewIcon := TIcon.Create;
  NewIcon.Handle := ShellApi.ExtractAssociatedIcon(HInstance, @Buffer, Dummy);
  Data.IconIndex := imFileIcons.AddIcon(NewIcon);
end;(*)

procedure TfaPanel.lvFileCompareNodes(Sender: TBaseVirtualTree; Node1, Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
var
  Data1, Data2: PData;
  IsDir1, IsDir2: Boolean;
begin
  Data1 := lvFile.GetNodeData(Node1);
  Data2 := lvFile.GetNodeData(Node2);

  if Assigned(Data1.Info) and Assigned(Data2.Info) then
  begin
    IsDir1 := faDirectory in Data1.Info.Attributes;
    IsDir2 := faDirectory in Data2.Info.Attributes;

    if IsDir1 = IsDir2 then
      Result := Str.TextCompare(Data1.Info.Name, Data2.Info.Name) else
    if IsDir1 then
      Result := -1 else
      Result := 1;
  end;
end;

procedure TfaPanel.tvFolderFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
var
  Data: PData;
begin
  Data := tvFolder.GetNodeData(Node);

  if Assigned(Data.Info) then Data.Info := nil;
  System.Finalize(Data^);
end;

procedure TfaPanel.tvFolderInitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
var
  Data: PData;
begin
  Data := tvFolder.GetNodeData(Node);

  System.Initialize(Data^);
  Data.IconIndex := -1;
  Data.IconHandle := 0;
end;

procedure TfaPanel.tvFolderGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: WideString);
var
  Data: PData;
begin
  Data := tvFolder.GetNodeData(Node);
  if Assigned(Data.Info) then CellText := Data.Info.Name;
end;

procedure TfaPanel.tvFolderGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: Integer);
begin
  case Kind of
    ikNormal:   ImageIndex := 1;
    ikSelected: ImageIndex := 0;
  end;
end;

procedure TfaPanel.Initialize(const Connection: ILayerConnection; const Chain: ILayerChain);
begin

end;

procedure TfaPanel.Connected(const Connection: ILayerConnection);
begin

end;

procedure TfaPanel.ConnectFailed(const Connection: ILayerConnection);
begin

end;

procedure TfaPanel.Disconnected(const Connection: ILayerConnection);
begin

end;

end.
