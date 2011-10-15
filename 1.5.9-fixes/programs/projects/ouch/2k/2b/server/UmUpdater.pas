unit UmUpdater;

interface

uses
  Sil,
  UiUpdater;

type
  TUpdater = class (TSilObject, IRunnable, IUpdater)
  private
    FInterval: LongWord;
    FPath: String;
    FFileName: String;
    FThread: IThread;
    FExit: IEvent;
    FFiles: IFileInfoList;
  private
    procedure DoFillFiles;
    procedure DoCheckFiles;
    function DoCheck(const FileInfo: IFileInfo): Boolean;
    procedure DoRegenerate(const Files: IFileInfoList);
  protected // IRunnable
    procedure Run(const Thread: IThread);
  protected // IUpdater
    procedure Start;
    procedure Stop;
  public
    constructor Create(const Path, FileName: String);
    destructor Destroy; override;
  end;

implementation

uses SilOjThread, SilOjFile, SilOiFile, SilLtXml, SilLiXml, DateUtils,
  SilLiKey;

{ TUpdater }

constructor TUpdater.Create(const Path, FileName: String);
begin
  inherited Create;

  FInterval := 600000;
  FPath := Sil.OS.FileSystem.AddSlash(Path);
  FFileName := FPath + FileName;

  Sil.OS.FileSystem.ForceDirectories(FPath);
end;

destructor TUpdater.Destroy;
begin
  inherited;
end;

procedure TUpdater.Start;
begin
  DoFillFiles;
  FExit := Sil.OS.Ipc.Event;
  FThread := Sil.OS.Thread.Spawn('updater', Self);
end;

procedure TUpdater.Stop;
begin
  if Assigned(FExit) then
  begin
    FExit.Signal;
    FThread.Termination.WaitFor;

    FExit := nil;
    FThread := nil;
    FFiles := nil;
  end;
end;

procedure TUpdater.DoFillFiles;
var
  Xml: IXmlTree;
  Node: IXmlTag;
  Item: IXmlNode;
  Enum: IEnumerator;
  Info: IFileInfoDef;
  FileName, FileTime: String;
begin
  FFiles := Sil.OS.FileSystem.FileInfoList;
  if not Sil.OS.FileSystem.Exists(FFileName) then Exit;

  Xml := Sil.Xml.ReadFile(FFileName);

  if Xml.FindTag('update/filelist', Node) then
    while Node.Childs.Enumerate(Enum, Item) do
      with Item.AsTag do
      begin
        FileName := Arguments.ReadString('path');
        FileTime := Arguments.ReadString('time');

        Info := Sil.OS.FileSystem.FileInfo(Sil.OS.FileSystem.GetFileName(FileName));
        Info.Time := DateTime.FromStr(FileTime, 'yyyy/mm/dd hh:nn:ss');

        FFiles.Add(Info);
      end;
end;

procedure TUpdater.Run(const Thread: IThread);
begin
  repeat
    DoCheckFiles;
  until FExit.WaitFor(FInterval) <> wrTimeOut;
end;

procedure TUpdater.DoCheckFiles;
var
  Enum: IEnumerator;
  Files: IFileInfoList;
  Item: IFileInfo;
  Changed: Boolean;
begin
  Changed := false;
  Files := Sil.OS.FileSystem.GetList(FPath);

  while Files.Enumerate(Enum, Item) do
    if (Str.TextCompare(Item.Name, 'update.xml') <> 0) and not DoCheck(Item) then
    begin
      Changed := true;
      Break;  
    end;

  if Changed then DoRegenerate(Files);
end;

function TUpdater.DoCheck(const FileInfo: IFileInfo): Boolean;
var
  Enum: IEnumerator;
  Item: IFileInfo;
begin
  Result := false;

  while FFiles.Enumerate(Enum, Item) do
    if Item.Name = FileInfo.Name then
    begin
      Result := Item.Time = FileInfo.Time;
      Break;
    end;
end;

procedure TUpdater.DoRegenerate(const Files: IFileInfoList);
var
  Xml: IXmlTree;
  Enum: IEnumerator;
  Item: IFileInfo;
  FilePath: String;
begin
  FFiles := Files;
  Xml := Sil.Xml.Tree;

  with Xml.Root.AsTag do
  begin
    Name := 'update';

    with Childs.Add(nkTag).AsTag do
    begin
      Name := 'filelist';

      while FFiles.Enumerate(Enum, Item) do
        if Str.TextCompare(Item.Name, 'update.xml') <> 0 then
          with Childs.Add(nkTag).AsTag do
          begin
            Name := 'file';

            if Item.Name <> 'update.exe' then
              FilePath := Item.Name else
              FilePath := 'update\' + Item.Name;

            Arguments.WriteString('path', '%SELF_PROCESS%\' + FilePath);
            Arguments.WriteString('time', DateTime.ToStr(Item.Time, 'yyyy/mm/dd hh:nn:ss'));
          end;
    end;

    with Childs.Add(nkTag).AsTag do
    begin
      Name := 'startup';

      with Childs.Add(nkTag).AsTag do
      begin
        Name := 'file';
        Arguments.WriteString('path', '%SELF_PROCESS%\ouch.exe');
      end;
    end;
  end;

  Sil.Xml.WriteFile(Xml, FFileName);
end;

end.
