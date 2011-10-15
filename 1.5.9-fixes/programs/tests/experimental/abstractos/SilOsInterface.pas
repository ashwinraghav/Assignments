unit SilOsInterface;

interface

uses
  SilOhInterface;

function API: POsInterface;

implementation

uses
  SilOfInterface;

const
  GOsThreadFactory: ROsThreadFactory = (
      GetCurrent: SilOfInterface.OsThreadGetCurent;
    );

const
  GOsInterface: ROsInterface = (
    Thread: @GOsThreadFactory;
    );

function API: POsInterface;
begin
  Result := @GOsInterface;
end;

end.
