program update;

uses
  Sil;

{$R *.res}

// update.exe <pid> <file.xml>

procedure WaitPid(Pid: Integer);
var
  Event: IEvent;
  Process: IProcess;
begin
  if Pid > 0 then
    try
      Event := Sil.OS.Ipc.Event(true, false, 'ouch.update');
      Process := Sil.OS.Process.Open(Pid);

      Sil.OS.Environment.SetValue('SELF_PROCESS', Os.Filesystem.DeleteSlash(Process.Info.Path));

      Event.Signal;
      Sil.OS.Wait.Single(Process, INFINITE);
    except
    end;
end;

procedure SwapFiles(const ConfigFile: String);
var
  Tree: IXmlTree;
  Node: IXmlTag;
  Child: IXmlNode;
  Enum: IEnumerator;
  SrcPath, DestPath, FilePath, FileName: String;
begin
  Tree := Sil.Xml.ReadFile(ConfigFile, nil, fmAccessRead);
  SrcPath := Sil.OS.FileSystem.GetFilePath(ConfigFile);

  if Tree.Root.AsTag.FindTag('filelist', Node) then
    while Node.Childs.Enumerate(Enum, Child) do
      if Child.NodeKind = nkTag then
        with Child.AsTag do
          if Str.CompareCase(Name, 'file') = 0 then
          begin
            FilePath := Sil.OS.Environment.Expand(Arguments.ReadString('path'));
            DestPath := Sil.OS.FileSystem.GetFilePath(FilePath);
            FileName := Sil.OS.FileSystem.GetFileName(FilePath);

            if Str.CompareCase(DestPath, SrcPath) <> 0 then
            begin
              Sil.OS.FileSystem.MoveFile(DestPath + FileName, SrcPath + FileName + '.back');
              Sil.OS.FileSystem.CopyFile(SrcPath + FileName, DestPath + FileName);
            end;
          end;

  if Tree.Root.AsTag.FindTag('startup', Node) then
    while Node.Childs.Enumerate(Enum, Child) do
      if Child.NodeKind = nkTag then
        with Child.AsTag do
          if Str.CompareCase(Name, 'file') = 0 then
          begin
            FilePath := Sil.OS.Environment.Expand(Arguments.ReadString('path'));
            Sil.OS.Process.Execute(FilePath);
          end;
end;

begin
  if ParamCount = 2 then
    try
      WaitPid(Str.ToInt(ParamStr(1), 0));
      SwapFiles(ParamStr(2));
    except
    end;
end.
