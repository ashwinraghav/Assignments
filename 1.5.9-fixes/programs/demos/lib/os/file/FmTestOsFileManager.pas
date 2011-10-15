unit FmTestOsFileManager;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Buttons, ExtCtrls, ComCtrls, Menus, ShellApi,
  ImgList, StdCtrls,

  Sil;

type
  TFormTestFileManager = class(TForm, IRunnable)
    MainMenu1: TMainMenu;
    miFile: TMenuItem;
    miExit: TMenuItem;
    paTree: TPanel;
    tvFolder: TTreeView;
    paToolbar: TPanel;
    Bevel1: TBevel;
    Panel1: TPanel;
    btCloseTree: TSpeedButton;
    sbStatus: TStatusBar;
    Splitter1: TSplitter;
    paFiles: TPanel;
    lvFile: TListView;
    paFileHeader: TPanel;
    ImgSmall: TImageList;
    ComboBox1: TComboBox;
    procedure miExitClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure tvFolderExpanding(Sender: TObject; Node: TTreeNode; var AllowExpansion: Boolean);
    procedure tvFolderChange(Sender: TObject; Node: TTreeNode);
    procedure tvFolderGetImageIndex(Sender: TObject; Node: TTreeNode);
    procedure FormDestroy(Sender: TObject);
    procedure lvFileCompare(Sender: TObject; Item1, Item2: TListItem;
      Data: Integer; var Compare: Integer);
  protected // IThread
    procedure Run(const Thread: IThread);
  private
    FStop: IEvent;
    FFolderReader: IThread;
    FRead: IEvent;
    FCurrentRoot: TTreeNode;
    FRootChanged: Boolean;
  private
    procedure DoReadDrives;
    function DoHasSubDir(const Path: String): Boolean;
    function DoBuildPath(Node: TTreeNode; CheckRoot: Boolean = false): String;
    procedure DoSetup;
    procedure DoReadFiles;
  public
    class function GetVolume(const Drive: String; FileSysName: Boolean = true): String;
    class function GetNetVolume(const Drive: String): String;
  end;

var
  FormTestFileManager: TFormTestFileManager;

implementation

{$R *.DFM}

procedure TFormTestFileManager.miExitClick(Sender: TObject);
begin
  Close;
end;

class function TFormTestFileManager.GetVolume(const Drive: String; FileSysName: Boolean): String;
var
  OldErrorMode: Integer;
  NotUsed, VolFlags: DWORD;
  Buf, FSys: array [0..MAX_PATH] of Char;
begin
  OldErrorMode := SetErrorMode(SEM_FAILCRITICALERRORS);
  try
    Buf[0] := #$00;
    if GetVolumeInformation(PChar(Drive), Buf, DWORD(sizeof(Buf)),
      nil, NotUsed, VolFlags, FSys, SizeOf(FSys)) then
    begin
      SetString(Result, Buf, StrLen(Buf));
      if FileSysName then Result := Str.Trim(Result + ' - ' + FSys);
    end else
      Result := '';
  finally
    SetErrorMode(OldErrorMode);
  end;
end;

class function TFormTestFileManager.GetNetVolume(const Drive: String): String;
var
  Buf: array [0..MAX_PATH] of Char;
  BufferSize: DWORD;
begin
  BufferSize := sizeof(Buf);
  if WNetGetConnection(PChar(Drive), Buf, BufferSize) = WN_SUCCESS then
    SetString(Result, Buf, BufferSize) else
    Result := GetVolume(Drive);
end;

function TFormTestFileManager.DoHasSubDir(const Path: String): Boolean;
var
  e: IEnumerator;
  List: IFileInfoList;
  FileInfo: IFileInfo;
begin
  List := Sil.OS.FileSystem.GetList(Path + Str.IIf(Str.LastChar(Path) = '\', '', '\'));

  while List.Enumerate(e, FileInfo) do
    if faDirectory in FileInfo.Attributes then
    begin
      Result := true;
      Exit;
    end;

  Result := false;
end;

procedure TFormTestFileManager.DoReadDrives;

  procedure DoAddItem(const Volume, Drive: String; CheckSubDir: Boolean);
  var
    Node: TTreeNode;
  begin
    Node := tvFolder.Items.AddChild(nil, Str.Trim(Volume + ' (' + Drive + ')'));
    Node.ImageIndex := 8;
    Node.SelectedIndex := 8;

    if CheckSubDir and (tvFolder.Selected = nil) then
      tvFolder.Selected := Node;

    if not CheckSubDir or DoHasSubDir(Drive) then
      tvFolder.Items.AddChild(Node, 'leyendo...');
  end;

var
  sBuf, sItem: String;
  i: Integer;
begin
  tvFolder.Items.BeginUpdate;
  tvFolder.Items.Clear;

  SetLength(sBuf, 1024);
  SetLength(sBuf, GetLogicalDriveStrings(Length(sBuf), PChar(sBuf)));
  Str.TranslateChar(sBuf, #0, #255);

  i := 1;
  repeat
    sItem := Str.Token(sBuf, #255, i);
    if Length(sItem) > 0 then
    begin
      case GetDriveType(PChar(sItem)) of
        DRIVE_REMOVABLE:  DoAddItem('Disco Extraible', sItem, false);
        DRIVE_FIXED:      DoAddItem(GetVolume(sItem), sItem, true);
        DRIVE_REMOTE:     DoAddItem(GetNetVolume(sItem), sItem, false);
        DRIVE_CDROM:      DoAddItem(GetVolume(sItem), sItem, true);
        DRIVE_RAMDISK:    DoAddItem(GetVolume(sItem), sItem, true);
      end;
    end;
  until i = 0;

  tvFolder.Items.EndUpdate;
end;

procedure TFormTestFileManager.DoSetup;
begin
  btCloseTree.Align := alRight;
end;

procedure TFormTestFileManager.FormCreate(Sender: TObject);
begin
  FStop := Sil.OS.Ipc.Event;
  FRead := Sil.OS.Ipc.Event;
  FFolderReader := Sil.OS.Thread.Spawn(Self);

  DoSetup;
  DoReadDrives;
end;

function TFormTestFileManager.DoBuildPath(Node: TTreeNode; CheckRoot: Boolean): String;
begin
  Result := '';

  while Node.Level > 0 do
  begin
    Result := Node.Text + '\' + Result;
    Node := Node.Parent;
  end;

  if CheckRoot then
  begin
    FRootChanged := FCurrentRoot <> Node;
    FCurrentRoot := Node;
  end;
  
  Result := Str.Copy(Node.Text, Str.LastPos('(', Node.Text) + 1, -2) + Result;
end;

procedure TFormTestFileManager.tvFolderExpanding(Sender: TObject; Node: TTreeNode; var AllowExpansion: Boolean);
var
  e: IEnumerator;
  List: IFileInfoList;
  FileInfo: IFileInfo;
  Child: TTreeNode;
begin
  tvFolder.Items.BeginUpdate;
  Node.DeleteChildren;
  List := Sil.OS.FileSystem.GetList(DoBuildPath(Node));

  while List.Enumerate(e, FileInfo) do
    if faDirectory in FileInfo.Attributes then
    begin
      Child := tvFolder.Items.AddChild(Node, FileInfo.Name);
      Child.ImageIndex := 2;
      Child.SelectedIndex := 1;

      if DoHasSubDir(FileInfo.FullName) then
        tvFolder.Items.AddChild(Child, 'leyendo...');
    end;

  tvFolder.Items.EndUpdate;
end;

procedure TFormTestFileManager.DoReadFiles;
var
  Item: TListItem;
  List: IFileInfoList;
  e: IEnumerator;
  FileInfo: IFileInfo;
  sExt: String;
begin
  lvFile.Items.BeginUpdate;
  lvFile.Items.Clear;

  if (tvFolder.Selected <> nil) and (tvFolder.Selected.Level > 0) then
  begin
    Item := lvFile.Items.Add;
    Item.Caption := '..';
    Item.ImageIndex := 2;
  end;

  try
    List := Sil.OS.FileSystem.GetList(DoBuildPath(tvFolder.Selected), false);

    while List.Enumerate(e, FileInfo) do
    begin
      Item := lvFile.Items.Add;
      Item.Caption := FileInfo.Name;

      if faDirectory in FileInfo.Attributes then
      begin
        Item.SubItems.Add('');
        Item.ImageIndex := 2;
      end else
      begin
        Item.SubItems.Add(Sil.Int.ToStr(FileInfo.Size));
        sExt := ExtractFileExt(Item.Caption);
        if (CompareText(sExt, '.com') = 0) or (CompareText(sExt, '.exe') = 0) then
          Item.ImageIndex := 4 else
          Item.ImageIndex := 5;
      end;
      Item.SubItems.Add(DateTime.ToStr(FileInfo.Time));
    end;

    sbStatus.Panels[0].Text := Int.ToStr(List.Count) + ' Objeto(s)';
  except
  end;

  lvFile.Items.EndUpdate;
end;

procedure TFormTestFileManager.tvFolderChange(Sender: TObject; Node: TTreeNode);
var
  sText: String;
  iSize: Int64;
begin
  paFileHeader.Caption := DoBuildPath(tvFolder.Selected, true);
  DoReadFiles;

  if (tvFolder.Selected <> nil) and FRootChanged then
  begin
    tvFolder.Selected.ImageIndex := 1;
    sText := DoBuildPath(tvFolder.Selected);
    iSize := DiskFree(Ord(Str.ToChr(sText, 'C')) - 64);
    sbStatus.Panels[1].Text := 'Espacio libre: ' + Sil.Int.ToStr(iSize);
  end;
end;

procedure TFormTestFileManager.tvFolderGetImageIndex(Sender: TObject; Node: TTreeNode);
begin
  if Node.Level = 0 then Node.ImageIndex := 8;
end;

procedure TFormTestFileManager.Run(const Thread: IThread);
var
  i: Integer;
begin
  repeat
    Sil.OS.Wait.Any([FStop, FRead], INFINITE, i);
    if i = 1 then
  until i = 0;
end;

procedure TFormTestFileManager.FormDestroy(Sender: TObject);
begin
  FStop.Signal;
  FFolderReader.Termination.WaitFor(INFINITE, true);
end;

procedure TFormTestFileManager.lvFileCompare(Sender: TObject; Item1, Item2: TListItem;
  Data: Integer; var Compare: Integer);
begin
  if (Item1.ImageIndex <> Item2.ImageIndex) and ((Item1.ImageIndex = 2) or (Item2.ImageIndex = 2)) then
    Compare := Item1.ImageIndex - Item2.ImageIndex else
    Compare := CompareText(Item1.Caption, Item2.Caption);
end;

end.
