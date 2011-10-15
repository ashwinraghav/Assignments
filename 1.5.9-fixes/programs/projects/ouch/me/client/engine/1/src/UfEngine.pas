unit UfEngine;

interface

uses
  UiOuchEngine;

function Engine: IOuchEngine; stdcall;

implementation

uses
  UmEngine;

function Engine: IOuchEngine;
begin
  Result := TOuchEngine.Create;
end;

end.
 