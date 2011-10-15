unit UtUpdate;

interface

{$include Defines.inc}

uses
  Sil,
  SilLayer;

type
  Updater = class (Tool)
  private
    class function DoGetFile(const FileProtocol: IClientSideFileProtocol; Path, FileName: String; out Local: IFile): Boolean;
    class function DoReadUpdate(const FileProtocol: IClientSideFileProtocol; const Path: String; const Update: IFile): Boolean;
  public
    class function Download(const FileProtocol: IClientSideFileProtocol; Path, FileName: String): Boolean;
    class procedure ChangeFiles(Path, FileName: String);
  end;

implementation

uses SilOjFile, SilSiLayerProtocolFile;

{ Update }

class function Updater.DoGetFile(const FileProtocol: IClientSideFileProtocol; Path, FileName: String; out Local: IFile): Boolean;
var
  FileInfo: IFileInfo;
  Remote: IFile;
  LocalFile: String;

  procedure DoDownload;
  begin
    Sil.Serializer.WriteStream(Remote.Stream, Local.Stream);
  end;

begin
  Result := false;
  LocalFile := Path + FileName;
  FileName := 'updates\' + FileName;

  try
    FileInfo := FileProtocol.GetInfo(FileName);

    if FileInfo.Time > 0 then
    begin
      Remote := FileProtocol.OpenFile(FileName);
      Local := Sil.OS.FileSystem.CreateFile(LocalFile);

      DoDownload;

      Local.Info.Time := FileInfo.Time;
      Local.Stream.Truncate;
      Local.Stream.Position := 0;

      Result := true;
    end;
  except
    // log
  end;
end;

class function Updater.DoReadUpdate(const FileProtocol: IClientSideFileProtocol; const Path: String; const Update: IFile): Boolean;
var
  Tree: IXmlTree;
  Node: IXmlTag;
  Child: IXmlNode;
  Enum: IEnumerator;
  FileName: String;
  FileTime: TDateTime;
  FileInfo: IFileInfo;
  Local: IFile;
begin
  Result := false;
  Tree := Sil.Xml.ReadStream(Update.Stream);

  if Tree.Root.AsTag.FindTag('filelist', Node) then
    while Node.Childs.Enumerate(Enum, Child) do
      if Child.NodeKind = nkTag then
        with Child.AsTag do
          if Str.TextCompare(Name, 'file') = 0 then
          begin
            FileName := Sil.OS.Environment.Expand(Arguments.ReadString('path'));
            FileTime := DateTime.FromStr(Arguments.ReadString('time'), 'yyyy/mm/dd hh:nn:ss');
            FileInfo := Sil.OS.FileSystem.GetInfo(FileName);

            if FileTime <> FileInfo.Time then
            begin
              FileName := Sil.OS.FileSystem.GetFileName(FileName);
              DoGetFile(FileProtocol, Path, FileName, Local);

              if not Result then
                Result := Str.TextCompare(FileName, 'update.exe') <> 0;
            end;
          end;
end;

class function Updater.Download(const FileProtocol: IClientSideFileProtocol; Path, FileName: String): Boolean;
var
  Update: IFile;
  LocalInfo, RemoteInfo: IFileInfo;
begin
  Result := false;

  Path := Sil.OS.FileSystem.AddSlash(Path);
  Sil.OS.FileSystem.ForceDirectories(Path);

  try
    LocalInfo := Sil.OS.FileSystem.GetInfo(Path + FileName);
    RemoteInfo := FileProtocol.GetInfo('updates\' + FileName);

    if (LocalInfo.Time <> RemoteInfo.Time) and DoGetFile(FileProtocol, Path, FileName, Update) then
      Result := DoReadUpdate(FileProtocol, Path, Update);
  except
    Sil.Trace.Exception('Updater.Download');
  end;
end;

class procedure Updater.ChangeFiles(Path, FileName: String);
var
  Event: IEvent;
  Cmd: String;
begin
  Path := Sil.OS.FileSystem.AddSlash(Path);
  Event := Sil.OS.Ipc.Event(true, false, 'ouch.update');

  Cmd := Str.Format('%s %d %s', [Path + 'update.exe', Sil.OS.Process.Current.PID, Path + FileName]);
  Sil.OS.Process.Execute(Cmd);
  Event.WaitFor;
end;

end.
