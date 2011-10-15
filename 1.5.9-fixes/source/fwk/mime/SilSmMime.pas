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

unit SilSmMime;

{$I Defines.inc}

interface

uses
  Sil,
  SilCoder,

  SilLmInterfaceList,
  SilSmMimeCoder,
  SilSiMailClient;

const
  SUnknownMimeCoder = 'SUnknownMimeCoder';

type

{ TMimeParam }

  TMimeParam = class(TSilInterfacedObject, IUnknown, IMimeParam)
  private
    FName: String;
    FValue: String;
    function GetName: String;
    procedure SetName(const Value: String);
    function GetValue: String;
    procedure SetValue(const Value: String);
  public
    property Name: String read GetName write SetName;
    property Value: String read GetValue write SetValue;
  end;

{ TMimeParamList }

  TMimeParamList = class(TSilInterfaceList, IMimeParamList)
  private
    function GetItem(Index: Integer): IMimeParam;
    procedure SetItem(Index: Integer; const Value: IMimeParam);
  public
    function First: IMimeParam;
    function Last: IMimeParam;
    function AddNew(const Name, Value: String): IMimeParam;
    function Find(const Name: String): IMimeParam;
    property Items[Index: Integer]: IMimeParam read GetItem write SetItem; default;
  end;

{ TMimeLabel }

  TMimeLabel = class(TSilInterfacedObject, IUnknown, IMimeLabel)
  private
    FName: String;
    FValue: String;
    FParams: IMimeParamList;
    FCoders: IStringList;
  private // IMimeLabel
    function GetName: String;
    procedure SetName(const Value: String);
    function GetValue: String;
    procedure SetValue(const Value: String);
    function GetParams: IMimeParamList;
    function GetCoders: IStringList;
    procedure SetCoders(const Value: IStringList);
    function FindCoder(const Name: String; out Coder): Boolean;
    function Lines: IStringList;
  public
    constructor Create; 
    destructor Destroy; override;
  end;

{ TMimeLabelList }

  TMimeLabelList = class(TSilInterfaceList, IMimeLabelList)
  private // IMimeLabelList
    function GetItem(Index: Integer): IMimeLabel;
    procedure SetItem(Index: Integer; const Value: IMimeLabel);
    function GetByName(const Index: String): String;
    procedure SetByName(const Index: String; const Value: String);
    function First: IMimeLabel;
    function Last: IMimeLabel;
    function CreateNew(const Name: String; const Value: String = ''): IMimeLabel; reintroduce;
    function Find(const Name: String): IMimeLabel;
  end;

{ TMailPart }

  TMailPart = class(TSilInterfacedObject, IUnknown, IMailPart)
  private
    FBoundary: String;
    FLabels: IMimeLabelList;
    FBody: IStringList;
  private // IMailPart
    function GetBoundary: String;
    procedure SetBoundary(const Value: String);
    function GetLabels: IMimeLabelList;
    function GetBody: IStringList;
    function Lines: IStringList;
    procedure Clear;
    function AttachFile(const FileName: String; const CoderName: String = ''): Boolean; overload;
    function AttachFile(const FileName: String; const Stream: IRandomStream; const CoderName: String = ''): Boolean; overload;
  public
    constructor Create;
    destructor Destroy; override;
  end;

{ TMailPartList }

  TMailPartList = class(TSilInterfaceList, IMailPartList, IMailAttachmentList)
  private
    FFileNames: IStringList;
  private // IMailPartList
    function GetItem(Index: Integer): IMailPart;
    procedure SetItem(Index: Integer; const Value: IMailPart);
    function First: IMailPart;
    function Last: IMailPart;
    function CreateNew: IMailPart; reintroduce;
    function FileNames: IStringList;
    function SaveToStream(const Name: String; const Stream: IRandomStream): Boolean;
  public
    destructor Destroy; override;
  end;

{ TMailMessage }

  TMailMessage = class(TSilInterfacedObject, IUnknown, IMailMessage)
  private
    FHeader: IMailPart;
    FMailParts: IMailPartList;
    FNumber: Integer;
    FSize: Integer;
    FUidl: String;
  private // IMailMessage
    function GetHeader: IMailPart;
    function GetAttachments: IMailAttachmentList;
    function GetMailParts: IMailPartList;
    function GetNumber: Integer;
    procedure SetNumber(const Value: Integer);
    function GetSize: Integer;
    procedure SetSize(const Value: Integer);
    function GetUidl: String;
    procedure SetUidl(const Value: String);
    procedure Clear;
  public
    constructor Create;
    destructor Destroy; override;
  end;

{ TMailMessageList }

  TMailMessageList = class(TSilInterfaceList, IMailMessageList)
  private // IMailMessageList
    function GetItem(Index: Integer): IMailMessage;
    procedure SetItem(Index: Integer; const Value: IMailMessage);
    function First: IMailMessage;
    function Last: IMailMessage;
    function CreateNew: IMailMessage; reintroduce;
    function IndexOfUidl(const Value: String): Integer;
  end;

procedure ExtractParams(const Buffer: String; var Number: Integer; var Size: String; const CheckInt: Boolean);

const
  sMessageFormatError = 'Error in Message Format';

implementation

uses SilOiFile;

procedure ExtractParams(const Buffer: String; var Number: Integer; var Size: String; const CheckInt: Boolean);
var
  i: Integer;
begin
  i := Str.Pos(#32, Buffer);

  if i > 0 then
  begin
    Number := Str.ToInt(Copy(Buffer, 1, i - 1), -1);
    Size := Str.Trim(Str.Copy(Buffer, i + 1));

    if CheckInt then
      for i := 1 to Length(Size) do
        if (Size[i] < '0') or (Size[i] > '9') then
        begin
          SetLength(Size, i - 1);
          Break;
        end;

  end else
  begin
    Number := Str.ToInt(Buffer, -1);
    Size := '';
  end;
end;

{ TMimeLabel }

constructor TMimeLabel.Create;
begin
  inherited Create;
  FParams := TMimeParamList.Create;
end;

destructor TMimeLabel.Destroy;
begin
  FParams := nil;
  inherited;
end;

function TMimeLabel.FindCoder(const Name: String; out Coder): Boolean;
var
  i: Integer;
begin
  i := FCoders.IndexOf(Name);
  Result := i >= 0;

  if Result then
    Pointer(Coder) := FCoders.Ptrs[i] else
    Pointer(Coder) := nil;
end;

function TMimeLabel.GetCoders: IStringList;
begin
  Result := FCoders;
end;

function TMimeLabel.GetName: String;
begin
  Result := FName;
end;

function TMimeLabel.GetParams: IMimeParamList;
begin
  Result := FParams;
end;

function TMimeLabel.GetValue: String;
var
  i, iBeg, iEnd: Integer;
  sLeftText, sCharset, sCodedText, sPlainText: String;
  Coder: TMimeCoderClass;
  chCoder: Char;
begin
  Result := FValue;
  if (Length(Result) = 0) or (FCoders = nil) then Exit;

  repeat
    iBeg := Str.Pos('=?', Result);
    iEnd := Str.Pos('?', Result, iBeg + 2);
    if (iBeg = 0) or (iEnd = 0) then Break;

    sLeftText := Str.Copy(Result, 1, iBeg - 1);
    sCharset := Str.Copy(Result, iBeg + 2, iEnd - iBeg - 2);
    chCoder := Str.ToChr(Str.Copy(Result, iEnd + 1, 1), #0);

    iBeg := Str.Pos('?', Result, iEnd + 2);
    iEnd := Str.LastPos('?=', Result);
    sCodedText := Str.Copy(Result, iBeg + 1, iEnd - iBeg - 1);
    sPlainText := Str.Copy(Result, iEnd + 2);

    case chCoder of
      'B':
      begin
        if FindCoder('base64', Coder) then
          sCodedText := Coder.DecodeLine(sCodedText);
      end;
      'Q':
      begin
        if FindCoder('quoted-printable', Coder) then
        begin
          sCodedText := Coder.DecodeLine(sCodedText);
          for i := 1 to Length(sCodedText) do
            if sCodedText[i] = '_' then sCodedText[i] := #32;
        end;
      end;
      'U':
      begin
        if FindCoder('uuencode', Coder) then
          sCodedText := Coder.DecodeLine(sCodedText);
      end;
      else Break;
    end;

    Result := sLeftText + sCodedText + sPlainText;
  until false;
end;

function TMimeLabel.Lines: IStringList;
var
  e: IEnumerator;
  Param: IMimeParam;
begin
  Result := Sil.List.StringList;
  Result.Add(FName + ': ' + FValue + Str.IIf(FParams.Count > 0, ';', ''));

  while FParams.Enumerate(e, Param) do
    Result.Add(Str.Replicate(#32, 8) + Param.Name + '=' + Param.Value + Str.IIf(FParams.Count - 1 > e.Iteration, ';', ''));
end;

procedure TMimeLabel.SetCoders(const Value: IStringList);
begin
  FCoders := Value;
end;

procedure TMimeLabel.SetName(const Value: String);
begin
  FName := Str.Trim(Value);
end;

procedure TMimeLabel.SetValue(const Value: String);
begin
  FValue := Str.Trim(Value);
end;

{ TMailMessageList }

function TMailMessageList.CreateNew: IMailMessage;
begin
  Result := TMailMessage.Create;
  Add(Result);  
end;

function TMailMessageList.First: IMailMessage;
begin
  Result := inherited First as IMailMessage;
end;

function TMailMessageList.GetItem(Index: Integer): IMailMessage;
begin
  Result := inherited GetItem(Index) as IMailMessage;
end;

function TMailMessageList.IndexOfUidl(const Value: String): Integer;
var
  e: IEnumerator;
  i: IMailMessage;
begin
  while Enumerate(e, i) do
    if Sil.Text.Compare(Value, i.Uidl) = 0 then
    begin
      Result := e.Iteration;
      Exit;
    end;
  Result := -1;
end;

function TMailMessageList.Last: IMailMessage;
begin
  Result := inherited Last as IMailMessage;
end;

procedure TMailMessageList.SetItem(Index: Integer; const Value: IMailMessage);
begin
  inherited SetItem(Index, Value);
end;

{ TMimeParam }

function TMimeParam.GetName: String;
begin
  Result := FName;
end;

function TMimeParam.GetValue: String;
begin
  Result := FValue;
end;

procedure TMimeParam.SetName(const Value: String);
begin
  FName := Str.Trim(Value);
end;

procedure TMimeParam.SetValue(const Value: String);
begin
  FValue := Str.Trim(Value);
end;

{ TMimeParamList }

function TMimeParamList.AddNew(const Name, Value: String): IMimeParam;
begin
  Result := TMimeParam.Create;
  Result.Name := Name;
  Result.Value := Value;
  Add(Result);
end;

function TMimeParamList.Find(const Name: String): IMimeParam;
var
  e: IEnumerator;
begin
  while Enumerate(e, Result) do
    if Sil.Text.Compare(Result.Name, Name) = 0 then
      Exit;

  Result := nil;
end;

function TMimeParamList.First: IMimeParam;
begin
  Result := inherited First as IMimeParam;
end;

function TMimeParamList.GetItem(Index: Integer): IMimeParam;
begin
  Result := inherited GetItem(Index) as IMimeParam;
end;

function TMimeParamList.Last: IMimeParam;
begin
  Result := inherited Last as IMimeParam;
end;

procedure TMimeParamList.SetItem(Index: Integer; const Value: IMimeParam);
begin
  inherited SetItem(Index, Value);
end;

{ TMailPartList }

destructor TMailPartList.Destroy;
begin
  FFileNames := nil;
  inherited;
end;

function TMailPartList.CreateNew: IMailPart;
begin
  Result := TMailPart.Create;
  Add(Result);
end;

function TMailPartList.FileNames: IStringList;
var
  e: IEnumerator;
  Part: IMailPart;
begin
  if FFileNames = nil then
  begin
    FFileNames := Sil.List.StringList;
    while Enumerate(e, Part) do
      FFileNames.Add(Part.Labels.Find('Content-Disposition').Params.Find('filename').Value);
  end;

  Result := FFileNames;
end;

function TMailPartList.First: IMailPart;
begin
  Result := inherited First as IMailPart;
end;

function TMailPartList.GetItem(Index: Integer): IMailPart;
begin
  FFileNames := nil;
  Result := inherited GetItem(Index) as IMailPart;
end;

function TMailPartList.Last: IMailPart;
begin
  Result := inherited Last as IMailPart;
end;

function TMailPartList.SaveToStream(const Name: String; const Stream: IRandomStream): Boolean;
var
  e: IEnumerator;
  Part: IMailPart;
  ContentLabel: IMimeLabel;
  FileNameParam, CharsetParam: IMimeParam;
  sCoder: String;
  Coder: TMimeCoderClass;
  Prog: TProgressCallback;
begin
  Result := false;
  Prog := nil;

  while Enumerate(e, Part) do
  begin
    ContentLabel := Part.Labels.Find('Content-Disposition');

    if ContentLabel <> nil then
    begin
      FileNameParam := ContentLabel.Params.Find('filename');

      if (FileNameParam <> nil) and (Sil.Text.Compare(FileNameParam.Value, Name) = 0) then
      begin
        ContentLabel := Part.Labels.Find('Content-Type');

        if ContentLabel <> nil then
        begin
          CharsetParam := ContentLabel.Params.Find('charset');
          if CharsetParam <> nil then sCoder := CharsetParam.Value;
        end;

        if (ContentLabel = nil) or (CharsetParam = nil) then
        begin
          ContentLabel := Part.Labels.Find('Content-Transfer-Encoding');
          if ContentLabel <> nil then sCoder := ContentLabel.Value;
        end;

        if Length(sCoder) > 0 then
          ContentLabel.FindCoder(sCoder, Coder) else
          ContentLabel.FindCoder('default', Coder);

        if Coder <> nil then
          Coder.Decode(Part.Body, Stream, Prog);

        Result := true;
      end;
    end;
  end;
end;

procedure TMailPartList.SetItem(Index: Integer; const Value: IMailPart);
begin
  FFileNames := nil;
  inherited SetItem(Index, Value);
end;

{ TMimeLabelList }

function TMimeLabelList.CreateNew(const Name: String; const Value: String = ''): IMimeLabel;
begin
  Result := TMimeLabel.Create;
  Result.Name := Name;
  Result.Value := Value;
  Add(Result);
end;

function TMimeLabelList.Find(const Name: String): IMimeLabel;
var
  e: IEnumerator;
begin
  while Enumerate(e, Result) do if Sil.Text.Compare(Result.Name, Name) = 0 then Exit;
  Result := nil;
end;

function TMimeLabelList.First: IMimeLabel;
begin
  Result := inherited First as IMimeLabel;
end;

function TMimeLabelList.GetByName(const Index: String): String;
var
  Item: IMimeLabel;
begin
  Item := Find(Index);
  if Item <> nil then
    Result := Item.GetValue else
    Result := '';
end;

procedure TMimeLabelList.SetByName(const Index: String; const Value: String);
var
  Item: IMimeLabel;
begin
  Item := Find(Index);
  if Item <> nil then Item.SetValue(Value);
end;

function TMimeLabelList.GetItem(Index: Integer): IMimeLabel;
begin
  Result := inherited GetItem(Index) as IMimeLabel;
end;

function TMimeLabelList.Last: IMimeLabel;
begin
  Result := inherited Last as IMimeLabel;
end;

procedure TMimeLabelList.SetItem(Index: Integer; const Value: IMimeLabel);
begin
  inherited SetItem(Index, Value);
end;

{ TMailPart }

function TMailPart.AttachFile(const FileName: String; const Stream: IRandomStream; const CoderName: String = ''): Boolean;

  function DoGetId: String;
  begin
    Result := Sil.GUID.ToStr(Sil.GUID.Create);
    Result := Str.Replace(Result, '-', '');
    Result := Str.Copy(Result, 2, -3);
  end;

var
  Lbl: IMimeLabel;
  Coder: TMimeCoderClass;
  sFileName: String;
  Prog: TProgressCallback;
begin
  Prog := nil;

  try
    sFileName := Sil.OS.FileSystem.GetFileName(FileName);
    FBoundary := 'NextPart_' + DoGetId;
    Lbl := TMimeLabel.Create;

    Lbl.Coders := Sil.List.StringList;
    Lbl.Coders.IgnoreCase := true;
    Lbl.Coders.Add(TBase64Coder.Name, TBase64Coder);
    Lbl.Coders.Add(TUUCoder.Name, TUUCoder);
    Lbl.Coders.Add(TISO8859_1Coder.Name, TISO8859_1Coder);
    Lbl.Coders.Add(TQuotedPrintableCoder.Name, TQuotedPrintableCoder);
    Lbl.Coders.Add(TWindows1252Coder.Name, TWindows1252Coder);
    Lbl.Coders.Add(TDefaultCoder.Name, TDefaultCoder);

    if not Lbl.FindCoder(CoderName, Coder) then
      Lbl.FindCoder('base64', Coder);

    Lbl := FLabels.CreateNew('Content-Type', 'file/x-download');
    Lbl.Params.AddNew('name', sFileName);
    Lbl := FLabels.CreateNew('Content-Transfer-Encoding', Coder.Name);
    Lbl := FLabels.CreateNew('Content-Disposition', 'attachment');
    Lbl.Params.AddNew('filename', sFileName);

    Coder.Encode(Stream, FBody, Prog);
    Result := true;
  except
    Result := false;
  end;
end;

function TMailPart.AttachFile(const FileName: String; const CoderName: String): Boolean;
var
  Src: IRandomStream;
begin
  try
    Src := Sil.OS.FileSystem.OpenFile(FileName, fmAccessRead, fmShareReadWrite, true).Stream;
    Result := AttachFile(FileName, Src, CoderName);
  except
    Result := false;
  end;
end;

procedure TMailPart.Clear;
begin
  FBoundary := '';
  FLabels.Clear;
  FBody.Clear;
end;

constructor TMailPart.Create;
begin
  inherited Create;
  FLabels := TMimeLabelList.Create;
  FBody := Sil.List.StringList;
end;

destructor TMailPart.Destroy;
begin
  FLabels := nil;
  FBody := nil;
  inherited;
end;

function TMailPart.GetBody: IStringList;
begin
  Result := FBody;
end;

function TMailPart.GetBoundary: String;
begin
  Result := FBoundary;
end;

function TMailPart.GetLabels: IMimeLabelList;
begin
  Result := FLabels;
end;

function TMailPart.Lines: IStringList;
var
  e: IEnumerator;
  Item: IMimeLabel;
begin
  Result := Sil.List.StringList;
  while FLabels.Enumerate(e, Item) do Result.AddStrings(Item.Lines);
end;

procedure TMailPart.SetBoundary(const Value: String);
begin
  FBoundary := Str.Trim(Value);
end;

{ TMailMessage }

procedure TMailMessage.Clear;
begin
  FHeader.Clear;
  FMailParts.Clear;
  FNumber := 0;
  FSize := 0;
  FUidl := '';
end;

constructor TMailMessage.Create;
begin
  inherited Create;
  FHeader := TMailPart.Create;
  FMailParts := TMailPartList.Create;
end;

destructor TMailMessage.Destroy;
begin
  FHeader := nil;
  FMailParts := nil;
  inherited;
end;

function TMailMessage.GetAttachments: IMailAttachmentList;
var
  e: IEnumerator;
  Part: IMailPart;
  MimeLab: IMimeLabel;
begin
  Result := TMailPartList.Create;

  while FMailParts.Enumerate(e, Part) do
  begin
    MimeLab := Part.Labels.Find('Content-Disposition');
    if (MimeLab <> nil) and (Sil.Text.Compare(MimeLab.Value, 'attachment') = 0) then
      Result.Add(Part);
  end;
end;

function TMailMessage.GetHeader: IMailPart;
begin
  Result := FHeader;
end;

function TMailMessage.GetMailParts: IMailPartList;
begin
  Result := FMailParts;
end;

function TMailMessage.GetNumber: Integer;
begin
  Result := FNumber;
end;

function TMailMessage.GetSize: Integer;
begin
  Result := FSize;
end;

function TMailMessage.GetUidl: String;
begin
  Result := FUidl;
end;

procedure TMailMessage.SetNumber(const Value: Integer);
begin
  FNumber := Value;
end;

procedure TMailMessage.SetSize(const Value: Integer);
begin
  FSize := Value;
end;

procedure TMailMessage.SetUidl(const Value: String);
begin
  FUidl := Str.Trim(Value);
end;

end.
