unit UiFileServer;

interface

uses
  Sil,
  SilLayer;

type
  IService = interface
    ['{45742FBF-B8CA-4916-BAE8-70F3A7672009}']
    procedure Start(const Params: IParameters);
    procedure Stop;
  end;

  IFileServer = interface (IService)
    ['{7593C123-21EF-4E1E-80DB-15E55C3C062D}']
  end;

  IFileClient = interface (IService)
    ['{CC61B215-62B7-4EF9-9963-420798B1B2C9}']
    function GetProtocol: IClientSideFileProtocol;
    function WaitConnect(Timeout: LongWord): Boolean;
    property Protocol: IClientSideFileProtocol read GetProtocol;
  end;

implementation

end.
 