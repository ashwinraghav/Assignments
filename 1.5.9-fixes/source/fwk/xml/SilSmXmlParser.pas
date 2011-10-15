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

unit SilSmXmlParser;

{$I Defines.inc}

interface

uses
  SilLkInterfaced,
  SilLiConnection,
  SilLiEnumerator,
  SilLiStream,
  SilLiEventList,
  SilBeTypes,
  SilSiXml,
  SilSiXmlParser,
  SilVector;

const
  SXmlMalformed = 'SXmlMalformed';

type
  TXmlLineKind = (lkUnknown, lkError, lkBegin, lkEnd, lkInline, lkData, lkComment);

  TStringBuf = class
  private
    fbuffer: string;
    fcount: integer;
  public
    constructor create(size: integer);
    procedure addc(value: char);
    procedure adds(const value: string);
    function buffer: string;
    procedure reset(size: integer);
  end;

  TXmlParser = class (
    // extends
    TSilInterfacedObject,
    // implements
    IXmlParser)
  private
    FStream: IStream;
    FBytesRead: LongWord;
    FMaxPacketSize: LongWord;
    FBuffer: String;
    FTagList: IVectorString;
    FTree: IXmlTree;
    FCurrentNode: IXmlNode;
    //FBuf: TStringBuf;
    //FRem: TStringBuf;
    FBuf: string;
    FRem: string;
  private
    function DoRead: Boolean;
    function DoGetChar(var Buf: PChar): Char;
    function DoDelSpc(var Buf: PChar): Boolean;
    function DoGet(var Buf: PChar): Char;
    procedure DoGetArgs(var Buf: PChar; out Args: TXmlArguments);
    function DoGetId(var Buf: PChar; out Id: String): Boolean;
    function DoGetTill(var Buf: PChar; const Sub: String; Kind: TXmlLineKind): Boolean;
    function DoPeek(var Buf: PChar; c: Char): Boolean;
    function DoReadLine(var Buf: PChar): TXmlLineKind;
    function DoReadStream(var Buf: PChar): Boolean;
    function DoGetVal(var Buf: PChar; out Value: String): Boolean;
  private
    procedure DoFireBegin(NodeKind: TXmlNodeKind; const Id: String; const Args: TXmlArguments; TagKind: TXmlTagKind);
    procedure DoFireEnd;
    procedure DoFireLines(const Value: String; Kind: TXmlLineKind);
  protected // IXmlParser
    function GetStream: IStream;
    procedure SetStream(const Value: IStream);
    function Read(const Tree: IXmlTree): Boolean;
  public
    constructor Create(const Stream: IStream);
    destructor Destroy; override;
  end;

implementation

uses
  SilBtText,
  SilBtError,
  SilBcChr,
  SilBtStr,
  SilLtReference,
  SilSmMimeCoder;

constructor TStringBuf.create(size: integer);
begin
  inherited create;
  setlength(fbuffer, size);
  fcount := 0;
end;

procedure TStringBuf.addc(value: char);
begin
  if fcount >= length(fbuffer) - 1 then
    setlength(fbuffer, fcount * 2);

  inc(fcount);
  fbuffer[fcount] := value;
end;

procedure TStringBuf.adds(const value: string);
var
  l: integer;
begin
  l := length(value);

  if fcount + l >= length(fbuffer) - 1 then
    setlength(fbuffer, fcount + l);

  move(value[1], fbuffer[fcount + 1], l);
  inc(fcount, l);
end;

function TStringBuf.buffer: string;
begin
  setlength(fbuffer, fcount);
  result := fbuffer;
end;

procedure TStringBuf.reset(size: integer);
begin
  fcount := 0;
  setlength(fbuffer, size);
end;

{ TXmlParser }

constructor TXmlParser.Create(const Stream: IStream);
begin
  inherited Create;

  FStream := Stream;
  FMaxPacketSize := 1024 * 1000;
  FBuf := '';//TStringBuf.create(255);
  FRem := '';//TStringBuf.create(255);
end;

destructor TXmlParser.Destroy;
begin
  FStream := nil;
  FTree := nil;
  FCurrentNode := nil;
  //FBuf.free;
  //FRem.free;

  inherited;
end;

function TXmlParser.GetStream: IStream;
begin
  Result := FStream;
end;

procedure TXmlParser.SetStream(const Value: IStream);
begin
  FStream := Value;
end;

function TXmlParser.Read(const Tree: IXmlTree): Boolean;
begin
  FTagList := SilVector.StringList;
  Result := false;
  if FStream = nil then Exit;

  FTree := Tree;
  FCurrentNode := nil;

  FBytesRead := 0;
  Result := DoRead;

  FTree := nil;
  FCurrentNode := nil;
  FTagList := nil;
end;

function TXmlParser.DoRead: Boolean;
var
  Kind: TXmlLineKind;
  i: Integer;
  pBuf: PChar;
begin
  i := 0;
  SetLength(FBuffer, FMaxPacketSize);
  FBuffer[1] := #0;
  pBuf := PChar(FBuffer);

  repeat
    Kind := DoReadLine(pBuf);

    case Kind of
      lkBegin:    Inc(i);
      lkEnd:      Dec(i);
    end;
  until (Kind <> lkUnknown) and ((Kind = lkError) or (i < 1));

  Result := Kind <> lkError;
end;

function TXmlParser.DoReadStream(var Buf: PChar): Boolean;
var
  lwSize: LongWord;
begin
  lwSize := FStream.Read(FBuffer[1], FMaxPacketSize);
  Result := lwSize > 0;

  if Result then
  begin
    FBuffer[lwSize + 1] := #0;
    Buf := PChar(FBuffer);
  end;
end;

function TXmlParser.DoGetChar(var Buf: PChar): Char;
begin
  Result := Buf^;

  if (Result <> #0) or DoReadStream(Buf) then
    Result := Buf^ else
    Error.Throw(SXmlMalformed);
end;

function TXmlParser.DoReadLine(var Buf: PChar): TXmlLineKind;
var
  sId: String;
  Args: TXmlArguments;
begin
  Result := lkError;

  if DoPeek(Buf, '<') then
  begin
    if DoPeek(Buf, '/') then
    begin
      DoGetId(Buf, sId);
      DoGet(Buf); // >

      if Str.ToLower(sId) = FTagList.Last then
      begin
        FTagList.Delete(FTagList.Count - 1);
        DoFireEnd;
        Result := lkEnd;
      end else
        Result := lkError;
    end else
    if DoPeek(Buf, '!') then
    begin
      DoGet(Buf); // -
      DoGet(Buf); // -
      DoFireBegin(nkComment, '', nil, tkUnknown);
      DoGetTill(Buf, '-->', lkComment);
      DoGet(Buf); // >
      DoFireEnd;
      Result := lkComment;
    end else
    if DoPeek(Buf, '?') then
    begin
      DoGetTill(Buf, '?>', lkUnknown);
      DoGet(Buf); // >
      Result := lkUnknown;
    end else
    if DoGetId(Buf, sId) then
    begin
      DoGetArgs(Buf, Args);

      case DoGet(Buf) of
        '>':
          begin
            FTagList.Add(Str.ToLower(sId));
            DoFireBegin(nkTag, sId, Args, tkBlock);
            Result := lkBegin;
          end;
        '/':
          if DoGet(Buf) = '>' then
          begin
            DoFireBegin(nkTag, sId, Args, tkInline);
            DoFireEnd;
            Result := lkInline;
          end else
            Result := lkError;
      end;
    end;
  end else
  begin
    DoGetTill(Buf, '<', lkData);
    Result := lkData;
  end;

  if Result = lkError then
    //! FireError
    ;
end;

function TXmlParser.DoPeek(var Buf: PChar; c: Char): Boolean;
begin
  DoDelSpc(Buf);
  Result := DoGetChar(Buf) = c;
  if Result then Inc(Buf);
end;

function TXmlParser.DoGetId(var Buf: PChar; out Id: String): Boolean;
var
  Ch: Char;
  count: integer;
begin
  DoDelSpc(Buf);
  setstring(id, nil, 255);
  count := 0;

  while true do
  begin
    Ch := DoGetChar(Buf);

    if Ch in [':', '_', '-', '.', '0'..'9', 'A'..'Z', 'a'..'z'] then
    begin
      inc(count);
      id[count] := ch;
      Inc(Buf);
    end else
      Break;
  end;

  setlength(id, count);
  Result := Length(Id) > 0;
end;

function TXmlParser.DoGet(var Buf: PChar): Char;
var
  Ch: Char;
begin
  DoDelSpc(Buf);

  while true do
  begin
    Ch := DoGetChar(Buf);
    Inc(Buf);

    if Ch in [#33..#126] then
    begin
      Result := Ch;
      Exit;
    end;
  end;

  Result := #0;
end;

procedure TXmlParser.DoGetArgs(var Buf: PChar; out Args: TXmlArguments);
var
  i: Integer;
  sId, sVal: String;
begin
  DoDelSpc(Buf);
  SetLength(Args, 0);

  while DoDelSpc(Buf) and DoGetId(Buf, sId) and DoGetVal(Buf, sVal) do
  begin
    i := Length(Args);
    SetLength(Args, i + 1);
    Args[i].Name := sId;
    Args[i].Value := TXmlCoder.DecodeLine(sVal);
  end;

  DoDelSpc(Buf);
end;

function TXmlParser.DoGetTill(var Buf: PChar; const Sub: String; Kind: TXmlLineKind): Boolean;
var
  bIn, bFire: Boolean;
  Ch: Char;
  iIdx: Integer;
begin
  Result := false;
  DoDelSpc(Buf);
  bIn := false;
  bFire := true;
  iIdx := 1;

  while true do
  begin
    Ch := DoGetChar(Buf);

    if Ch = Sub[iIdx] then
    begin
      if iIdx = Length(Sub) then
      begin
        //if bFire then DoFireLines(Str.TrimRight(FBuf.buffer), Kind);
        if bFire then DoFireLines(Str.TrimRight(FBuf), Kind);
        Result := true;
        Break;
      end else
      begin
        //FRem.addc(Sub[iIdx]);
        FRem := FRem + Sub[iIdx];
        Inc(Buf);
        Inc(iIdx);
      end;
    end else
    begin
      if iIdx > 1 then
      begin
        iIdx := 1;
        //FBuf.adds(FRem.buffer);
        FBuf := FBuf + FRem;
        //FRem.reset(255);
        FRem := '';
      end;

      if Ch in [#32..#126] then
      begin
        if not bIn then
        begin
          bIn := Ch <> #32;
          if bIn and not bFire then bFire := true;
        end;
        //if bIn then FBuf.addc(ch);
        if bIn then FBuf := FBuf + ch;
        Inc(Buf);
      end else
      begin
        if bIn then
        begin
          //DoFireLines(Str.TrimRight(FBuf.buffer), Kind);
          DoFireLines(Str.TrimRight(FBuf), Kind);
          bFire := false;

          //FBuf.reset(255);
          FBuf := '';
          bIn := false;
        end;
        Inc(Buf);
      end;
    end;
  end;

  //FBuf.reset(255);
  //FRem.reset(255);
  FBuf := '';
  FRem := '';
end;

function TXmlParser.DoDelSpc(var Buf: PChar): Boolean;
var
  Ch: Char;
begin
  Result := true;

  while true do
  begin
    Ch := DoGetChar(Buf);

    if Ch in [#0..#32] then
      Inc(Buf) else
      Break;
  end;
end;

function TXmlParser.DoGetVal(var Buf: PChar; out Value: String): Boolean;
var
  Ch: Char;
  count: integer;
begin
  DoDelSpc(Buf);
  Result := (DoGet(Buf) = '=') and (DoGet(Buf) = '"');

  if Result then
  begin
    count := 0;
    setstring(value, nil, 255);

    while true do
    begin
      Ch := DoGetChar(Buf);

      if Ch <> '"' then
      begin
        inc(count);
        value[count] := ch;
        Inc(Buf);
      end else
        Break;
    end;

    setlength(value, count);
    Result := DoGet(Buf) = '"';
  end;
end;

procedure TXmlParser.DoFireBegin(NodeKind: TXmlNodeKind; const Id: String; const Args: TXmlArguments; TagKind: TXmlTagKind);
var
  e: IEnumerator;
  Item: IXmlParserEvents;
  Event: RXmlNodeBeginEvent;
  evs: IEventList;
begin
  evs := events;
  if evs = nil then Exit;

  Event.Sender := Self;
  Event.Percent := 0;
  if FCurrentNode = nil then
    Event.Action := paSetRoot else
    Event.Action := paAddNode;
  Event.Tree := FTree;
  Event.CurrentNode := FCurrentNode;
  Event.NodeKind := NodeKind;
  Event.Name := Id;
  Event.Arguments := Args;
  Event.TagKind := TagKind;

  with evs do
    while Enumerate(e, Item, IXmlParserEvents) do
      Item.OnXmlNodeBegin(Event);

  FCurrentNode := Event.CurrentNode;
end;

procedure TXmlParser.DoFireEnd;
var
  e: IEnumerator;
  Item: IXmlParserEvents;
  Event: RXmlNodeEndEvent;
  evs: IEventList;
begin
  evs := events;
  if evs = nil then Exit;

  Event.Sender := Self;
  Event.Percent := 0;
  Event.Tree := FTree;
  Event.CurrentNode := FCurrentNode;

  with evs do
    while Enumerate(e, Item, IXmlParserEvents) do
      Item.OnXmlNodeEnd(Event);

  FCurrentNode := Event.CurrentNode;
end;

procedure TXmlParser.DoFireLines(const Value: String; Kind: TXmlLineKind);
var
  e: IEnumerator;
  Item: IXmlParserEvents;
  Event: RXmlCommentEvent;
  evs: IEventList;
begin
  evs := events;
  if evs = nil then Exit;

  Event.Sender := Self;
  Event.Percent := 0;
  Event.Tree := FTree;
  Event.CurrentNode := FCurrentNode;
  Event.TextLine := TXmlCoder.DecodeLine(Value);

  with evs do
    while Enumerate(e, Item, IXmlParserEvents) do
      case Kind of
        lkData:     Item.OnXmlNodeData(Event);
        lkComment:  Item.OnXmlComment(Event);
      end;

  FCurrentNode := Event.CurrentNode;
end;

end.
