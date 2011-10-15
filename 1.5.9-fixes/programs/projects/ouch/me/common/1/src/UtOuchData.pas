unit UtOuchData;

interface

uses
  Sil,

  UeOuchData;


type
  Data = class(Tool)
    class function ToStr(const Data: TOuchUserData): string;
    class function FromStr(const Data: string): TOuchUserData;
  end;

implementation

{ Data }

class function Data.FromStr(const Data: string): TOuchUserData;
begin
  Result := TOuchUserData(Sil.Enum.Value(TypeInfo(TOuchUserData), Data, 'odUser'));
end;

class function Data.ToStr(const Data: TOuchUserData): string;
begin
  Result := Sil.Enum.Name(TypeInfo(TOuchUserData), Ord(Data), 'odUser');
end;

end.
