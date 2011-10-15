{********************************************************************************
 *                  Standard Interface Library (SIL)                            *
 *                                                                              *
 *       General purpose library whose design is based in STRONG                *
 *   use of interfaces.                                                         *
 *                                                                              *
 *                                                                              *
 *     Copyright (C) 2000 Mariano Podestá    antiriad@gmail.com                 *
 *     Copyright (C) 2000 Leandro Conde      lconde@str.com.ar                  *
 *     Copyright (C) 2000 Lisandro Podestá   lisandrop@movi.com.ar              *
 *                                                                              *
 *     See License.txt for details.                                             *
 *                                                                              *
 *   This library is free software; you can redistribute it and/or              *
 *   modify it under the terms of the GNU Lesser General Public                 *
 *   License as published by the Free Software Foundation; either               *
 *   version 2.1 of the License, or (at your option) any later version.         *
 *                                                                              *
 *   This library is distributed in the hope that it will be useful,            *
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of             *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU          *
 *   Lesser General Public License for more details.                            *
 *                                                                              *
 *   You should have received a copy of the GNU Lesser General Public           *
 *   License along with this library; if not, write to the Free Software        *
 *   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA  *
 *                                                                              *
 ********************************************************************************}

unit SilSmFirebirdParameters;

{$INCLUDE Defines.inc}

interface

uses
  Sil, SilContainer,
  SilSiFirebird,
  SilShFirebird;

type
  TSilFirebirdParameters = class(
    TSilObject,
    IEnumerable,
    IFbParametersInternal )
  private
    FList: IContainerDynamic;
    FBlock: IFbParamBlock;
  private
    function DoFind(Field: TFbParameterField; Value: HData; out Info: PFbParameterInfo): Boolean;
    procedure DoParse(var Buffer: PChar; const Parameters: IParameterList);
    function DoParseItem(Buffer: PChar; Info: PFbParameterInfo; const Parameters: IParameterList): PChar;
    function DoParseChildren(Item: PFbParameterInfo; var Buffer: PChar): IParameterList;
    function DoGetChildren(const Children: TFbParameterList; const Block: IFbParamBlock): IFbParametersInternal;
    procedure DoLoad(First: PFbParameterArray; Count: Integer);
    function DoCompare(Data1, Data2: HData; Param: Pointer): Integer;
  protected // IEnumerable
    function GetEnumerator(out Enum: IEnumerator; Locked: Boolean): Boolean;
  protected // IFbParameters
    function GetCount: Integer;
    function GetItem(Index: Integer): PFbParameterInfo;
    function Find(const Name: string; out Info: PFbParameterInfo): Boolean; overload;
    function Find(Code: Integer; out Info: PFbParameterInfo): Boolean; overload;
    function Enumerate(var Enum: IEnumerator; out Info: PFbParameterInfo): Boolean;
  protected
  protected // IFbParametersInternal
    function Add(var Buffer: string; const Param: string; const Value: Variant): Boolean; overload;
    procedure Add(var Buffer: string; const Parameters: IParameters); overload;
    function Parse(Buffer: PChar; const Parameters: IParameterList): PChar;
  public
    constructor Create(const Block: IFbParamBlock); overload;
    constructor Create(const List: array of PFbParameterInfo; const Block: IFbParamBlock); overload;
    constructor Create(Count: Integer; Info: PFbParameterArray; const Block: IFbParamBlock); overload;
    destructor Destroy; override;
  end;

implementation

uses
  SilLmContainerEnumerator, SilSdFirebird, SilLtList, SilScFirebirdClient;

{ TSilFirebirdParameters }

constructor TSilFirebirdParameters.Create(const Block: IFbParamBlock);
begin
  inherited Create;
  FBlock := Block;
  FList := Vector.Create(SizeOf(PFbParameterInfo), DoCompare);
end;

constructor TSilFirebirdParameters.Create(const List: array of PFbParameterInfo; const Block: IFbParamBlock);
begin
  Create(Length(List), @List[0], Block);
end;

constructor TSilFirebirdParameters.Create(Count: Integer; Info: PFbParameterArray; const Block: IFbParamBlock);
begin
  Create(Block);
  DoLoad(Info, Count);
end;

destructor TSilFirebirdParameters.Destroy;
begin
  FList := nil;
  FBlock := nil;
  inherited;
end;

function TSilFirebirdParameters.GetEnumerator(out Enum: IEnumerator; Locked: Boolean): Boolean;
begin
  Result := Assigned(FList) and (FList.Items.Count > 0);
  if Result then Enum := TSilContainerEnumerator.Create(FList, Sequence.Create(FList));
end;

function TSilFirebirdParameters.GetCount: Integer;
begin
  if Assigned(FList) then
    Result := FList.Items.Count else
    Result := 0;
end;

function TSilFirebirdParameters.GetItem(Index: Integer): PFbParameterInfo;
begin
  if Assigned(FList) then
    Result := PFbParameterInfo(FList[Index]^) else
    Result := nil;
end;

function TSilFirebirdParameters.Find(const Name: string; out Info: PFbParameterInfo): Boolean;
begin
  Result := DoFind(fbfdName, @Name, Info);
end;

function TSilFirebirdParameters.Find(Code: Integer; out Info: PFbParameterInfo): Boolean;
begin
  Result := DoFind(fbfdCode, @Code, Info);
end;

function TSilFirebirdParameters.Enumerate(var Enum: IEnumerator; out Info: PFbParameterInfo): Boolean;
begin
  if Enum <> nil then
    Result := Enum.Next
  else if GetEnumerator(Enum, False) then
    Result := Enum.HasMore
  else
    Result := False;

  if Result then
    Info := PFbParameterInfo(Enum.Current^) else
    Enum := nil;
end;

function TSilFirebirdParameters.Add(var Buffer: string; const Param: string; const Value: Variant): Boolean;
var
  Info: PFbParameterInfo;
begin
  Result := Find(Param, Info);
  if Result then
  begin
    case Info.VType of
      varOleStr, varString:
        FBlock.AddString(Info, Buffer, String(Value));
      varByte:
        FBlock.AddByte(Info, Buffer, Byte(Value));
      varBoolean:
        FBlock.AddBoolean(Info, Buffer, Boolean(Value));
      varInteger, varLongWord:
        FBlock.AddInteger(Info, Buffer, Integer(Value));
    end;
  end;
end;

procedure TSilFirebirdParameters.Add(var Buffer: string; const Parameters: IParameters);
var
  Enum: IEnumerator;
  Item: RParameter;
begin
  if Assigned(Parameters) then
    with Parameters do
      while Enumerate(Enum, Item) do
        Add(Buffer, Item.Name, Item.Value);
end;

function TSilFirebirdParameters.Parse(Buffer: PChar; const Parameters: IParameterList): PChar;
begin
  if Sil.Str.IsAssigned(Buffer) then
    DoParse(Buffer, Parameters);
  Result := Buffer;
end;

function TSilFirebirdParameters.DoFind(Field: TFbParameterField; Value: HData; out Info: PFbParameterInfo): Boolean;
var
  Index: HItem;
begin
  Result := Assigned(FList) and FList.Find(Value, @Index, nil, Pointer(Field));
  if Result then Info := PFbParameterInfo(FList[Index]^);
end;

procedure TSilFirebirdParameters.DoParse(var Buffer: PChar; const Parameters: IParameterList);
const
  CTerminators = [#0];
var
  Info: PFbParameterInfo;
begin
  while not (Buffer^ in CTerminators) do
    case Buffer^ of
      Char(isc_info_end):
        begin
          Inc(Buffer, SizeOf(Word) + SizeOf(Char));
          Break;
        end;
      Char(isc_info_flag_end):
        begin
          Inc(Buffer, SizeOf(Word) + SizeOf(Char));
          Break;
        end;
      Char(isc_info_truncated):
        begin
          Inc(Buffer, SizeOf(Char));
          Continue;
        end;
      Char(isc_info_error):
        begin
          Inc(Buffer, SizeOf(Char));
          Continue;
        end;
      else
        if Find(Byte(Buffer^), Info) then
          Buffer := DoParseItem(Buffer, Info, Parameters) else
          Break;
    end;
end;

function TSilFirebirdParameters.DoParseItem(Buffer: PChar; Info: PFbParameterInfo; const Parameters: IParameterList): PChar;
begin
  case Info.VType of
    varOleStr, varString:
      Parameters[Info.Name^] := FBlock.GetString(Info, Buffer);
    varByte:
      Parameters[Info.Name^] := FBlock.GetByte(Info, Buffer);
    varBoolean:
      Parameters[Info.Name^] := FBlock.GetBoolean(Info, Buffer);
    varInteger, varLongWord:
      Parameters[Info.Name^] := FBlock.GetInteger(Info, Buffer);
    else
      Parameters[Info.Name^] := DoParseChildren(Info, Buffer);
  end;
  Result := Buffer;
end;

function TSilFirebirdParameters.DoParseChildren(Item: PFbParameterInfo; var Buffer: PChar): IParameterList;
var
  Block: IFbParametersInternal;
begin
  Sil.Error.Check(Item.Children.Count > 0, SAssertZeroChildren, [Item.Name^]);
  Inc(Buffer);
  Result := Sil.List.Parameters();
  Block := DoGetChildren(Item.Children, FBlock);
  Buffer := Block.Parse(Buffer, Result);
end;

function TSilFirebirdParameters.DoGetChildren(const Children: TFbParameterList; const Block: IFbParamBlock): IFbParametersInternal;
begin
  Result := TSilFirebirdParameters.Create(Children.Count, Children.Info, Block);
end;

procedure TSilFirebirdParameters.DoLoad(First: PFbParameterArray; Count: Integer);
var
  Index: Integer;
begin
  if Assigned(First) then
    for Index := 0 to Count - 1 do
      FList.Add(@First^[Index]);
end;

function TSilFirebirdParameters.DoCompare(Data1, Data2: HData; Param: Pointer): Integer;
begin
  case TFbParameterField(Param) of
    fbfdCode:     Result := Integer(Data1^) - PFbParameterInfo(Data2^).Code;
    fbfdName:     Result := Sil.Str.TextCompare(PString(Data1)^, PFbParameterInfo(Data2^).Name^);
    else          Result := -1;
  end;
end;

end.
