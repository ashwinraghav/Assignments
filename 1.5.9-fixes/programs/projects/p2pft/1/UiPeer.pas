unit UiPeer;

interface

uses
  Sil;

type
  IPeer = interface
    ['{2AD80F42-21DD-430E-97DA-11B2FD0669ED}']
    procedure ReadContent(const Path: String = '');
    procedure Download(const Path: String);
    procedure Cancel(const Path: String);
  end;

  TContentAction = (caNew, caAppend);

  IPeerHook = interface
    ['{298FC889-6E65-4234-A71B-27FB6486E57D}']
    procedure Connected(const Peer: IPeer);
    procedure Content(const Peer: IPeer; const Path: String; Action: TContentAction; const Info: IFileInfoList);
    procedure DownloadProgress(FileId, CurrentSize, Speed: LongWord);
    procedure DownloadCanceled(FileId: LongWord);
  end;

implementation

end.
 