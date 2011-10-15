unit UmLocalFSys;

interface

uses
  Sil,
  SilLayer;

type
  TLocalFSys = class (TSilObject, IClientSideFileProtocol)
  protected // IClientSideFileProtocol
    function OpenFile(const FileName: String; Access: TFileAccessMode = fmAccessReadWrite; Share: TFileShareMode = fmShareNone; MustExists: Boolean = False): IFile;
    function CreateFile(const FileName: String; Access: TFileAccessMode = fmAccessReadWrite; Share: TFileShareMode = fmShareNone; MustCreate: Boolean = True): IFile;
    function CreateDirectory(const PathName: String; Force: Boolean = false): Boolean;
    function ReadDirectory(const PathName: String; const Include: TFileAttributes = []; const Exclude: TFileAttributes = []): IDirectoryReader;
    function GetInfo(const FileName: String): IFileInfo;
    function Move(const OldName, NewName: String): Boolean;
    function Delete(const FileName: String): Boolean;
  end;

implementation

{ TLocalFSys }

function TLocalFSys.CreateDirectory(const PathName: String; Force: Boolean): Boolean;
begin
  if Force then
    Result := Sil.OS.Fsys.ForceDirectories(PathName) else
    Result := Sil.OS.Fsys.CreateDirectory(PathName);
end;

function TLocalFSys.CreateFile(const FileName: String; Access: TFileAccessMode; Share: TFileShareMode; MustCreate: Boolean): IFile;
begin
  Result := Sil.OS.Fsys.CreateFile(FileName, Access, Share, MustCreate);
end;

function TLocalFSys.Delete(const FileName: String): Boolean;
begin
  Result := Sil.OS.Fsys.DeleteFile(FileName);
end;

function TLocalFSys.GetInfo(const FileName: String): IFileInfo;
begin
  Result := Sil.OS.Fsys.GetInfo(FileName);
end;

function TLocalFSys.Move(const OldName, NewName: String): Boolean;
begin
  Result := Sil.OS.Fsys.MoveFile(OldName, NewName);
end;

function TLocalFSys.OpenFile(const FileName: String; Access: TFileAccessMode; Share: TFileShareMode; MustExists: Boolean): IFile;
begin
  Result := Sil.OS.Fsys.OpenFile(FileName, Access, Share, MustExists);
end;

function TLocalFSys.ReadDirectory(const PathName: String; const Include, Exclude: TFileAttributes): IDirectoryReader;
begin
  Result := Sil.OS.FSys.ReadDirectory(PathName, Include, Exclude);
end;

end.
 