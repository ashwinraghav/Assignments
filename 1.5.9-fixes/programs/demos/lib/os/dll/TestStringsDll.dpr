library TestStringsDll;

uses
  SilDll,
  Sil;

{$R *.RES}

function CreateStringList: IStringList; stdcall;
begin
  Result := Sil.List.StringList();
end;

exports
  CreateStringList;

begin
end.
 