unit SilOtInterface;

interface

uses
  SilOhInterface;

type
  OsWindowsApi = class(OsApi)
    class function Thread: OsApiThreadClass; override;
    class function SharedLibrary: OsApiSharedLibraryClass; override; 
  end;

  OsWindowsThread = class(OsApiThread)
    class function GetCurrent: IOsThreadInstance; override; 
    class function Create(const Callbacks: IOsThreadCallbacks; CreateSuspended: Boolean; Arguments: Pointer = nil): IOsThreadInstance; override; 
  end;

implementation

{ OsWindowsApi }

class function OsWindowsApi.SharedLibrary: OsApiSharedLibraryClass;
begin
end;

class function OsWindowsApi.Thread: OsApiThreadClass;
begin
  Result := OsWindowsThread;
end;

{ OsWindowsThread }

class function OsWindowsThread.Create(const Callbacks: IOsThreadCallbacks; CreateSuspended: Boolean; Arguments: Pointer): IOsThreadInstance;
begin

end;

class function OsWindowsThread.GetCurrent: IOsThreadInstance;
begin

end;

end.
 