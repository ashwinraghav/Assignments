unit UtProtocol;

interface

{$include Defines.inc}

uses
  Sil,
  UiProtocol;

type
  Prot = class (Tool)
    class function ReadParams(const Packet: IPacket): IParameterList;
    class procedure WriteParams(const Packet: IPacket; const Params: IParameters);
    class function ReadGuidArray(const Packet: IPacket): TGuidArray;
    class procedure WriteGuidArray(const Packet: IPacket; const List: TGuidArray);
    class function ReadUserArray(const Packet: IPacket): TUserArray;
    class procedure WriteUserArray(const Packet: IPacket; const List: TUserArray);
    class function ToGuidArray(const List: TUserArray): TGuidArray;
    class procedure UserAdd(var List: TUserArray; const Id: TGuid; const Updated: TDateTime); 
  end;

implementation

uses SilLiFiler;

{ Prot }

class function Prot.ReadGuidArray(const Packet: IPacket): TGuidArray;
var
  i: Integer;
begin
  SetLength(Result, Packet.Reader.ReadLongWord);

  for i := 0 to Length(Result) - 1 do
    Result[i] := Packet.Reader.ReadGuid;
end;

class procedure Prot.WriteGuidArray(const Packet: IPacket; const List: TGuidArray);
var
  i: Integer;
begin
  Packet.Writer.WriteLongWord(Length(List));

  for i := 0 to Length(List) - 1 do
    Packet.Writer.WriteGuid(List[i]);
end;

class function Prot.ReadParams(const Packet: IPacket): IParameterList;
begin
  Result := Sil.List.Parameters;
  Sil.Serializer.LoadFromString(Result, Packet.Reader.ReadString);
end;

class procedure Prot.WriteParams(const Packet: IPacket; const Params: IParameters);
var
  Buf: String;
begin
  Sil.Serializer.SaveToString(Params, Buf);
  Packet.Writer.WriteString(Buf);
end;

class function Prot.ReadUserArray(const Packet: IPacket): TUserArray;
var
  i: Integer;
begin
  SetLength(Result, Packet.Reader.ReadLongWord);

  for i := 0 to Length(Result) - 1 do
  begin
    Result[i].Id := Packet.Reader.ReadGuid;
    Result[i].Updated := Packet.Reader.ReadDate;
  end;
end;

class procedure Prot.WriteUserArray(const Packet: IPacket; const List: TUserArray);
var
  i: Integer;
begin
  Packet.Writer.WriteLongWord(Length(List));

  for i := 0 to Length(List) - 1 do
  begin
    Packet.Writer.WriteGuid(List[i].Id);
    Packet.Writer.WriteDate(List[i].Updated);
  end;
end;

class function Prot.ToGuidArray(const List: TUserArray): TGuidArray;
var
  i: Integer;
begin
  SetLength(Result, Length(List));

  for i := 0 to Length(List) - 1 do
    Result[i] := List[i].Id;
end;

class procedure Prot.UserAdd(var List: TUserArray; const Id: TGuid; const Updated: TDateTime);
var
  i: Integer;
begin
  i := Length(List);
  SetLength(List, i + 1);
  List[i].Id := Id;
  List[i].Updated := Updated;
end;

end.
