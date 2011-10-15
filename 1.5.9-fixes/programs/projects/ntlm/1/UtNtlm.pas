unit UtNtlm;

interface

uses
  SysUtils, Classes, Hash;

{$i cipher.inc}

const {ErrorCode's for ECipherException}
  errGeneric        = 0;  {generic Error}
  errInvalidKey     = 1;  {Decode Key is not correct}
  errInvalidKeySize = 2;  {Size of the Key is too large}
  errNotInitialized = 3;  {Methods Init() or InitKey() were not called}

type
  ECipherException = class(Exception)
  public
    ErrorCode: Integer;
  end;

type
  (*)Ntlm = class (Tool)
    //class function
  end;(*)

  TCipherMode = (cmCTS, cmCBC, cmCFB, cmOFB, cmECB);
  TCipherClass = class of TCipher;

  TCipher = class(TPersistent)
  private
    FMode: TCipherMode;
    FHash: THash;
    FHashClass: THashClass;
    FKeySize: Integer;
    FBufSize: Integer;
    FUserSize: Integer;
    FBuffer: Pointer;
    FVector: Pointer;
    FFeedback: Pointer;
    FUser: Pointer;
    FFlags: Integer;
    function GetHash: THash;
    procedure SetHashClass(Value: THashClass);
    procedure InternalCodeStream(Source, Dest: TStream; DataSize: Integer; Encode: Boolean);
    procedure InternalCodeFile(const Source, Dest: String; Encode: Boolean);
  protected
    function GetFlag(Index: Integer): Boolean;
    procedure SetFlag(Index: Integer; Value: Boolean); virtual;
{used in method Init()}
    procedure InitBegin(var Size: Integer);
    procedure InitEnd(IVector: Pointer);
{must override}
    class procedure GetContext(var ABufSize, AKeySize, AUserSize: Integer); virtual; abstract;
    class function TestVector: Pointer; virtual; abstract;

{the encode function, must override}
    procedure Encode(Data: Pointer); virtual;
{the decode function, must override}
    procedure Decode(Data: Pointer); virtual;
{the individual Userdata}
    property User: Pointer read FUser;
    property UserSize: Integer read FUserSize;
{the Key is set from InitKey() and the Hash.DigestKey^ include the encrypted Hash-Key}
    property HasHashKey: Boolean index 0 read GetFlag write SetFlag;
  public
    destructor Destroy; override;
    class function NewInstance: TObject; override;
    class function MaxKeySize: Integer;
{performs a Test of correct work}
    class function SelfTest: Boolean;
{initialization form the Cipher}
    procedure Init(const Key; Size: Integer; IVector: Pointer); virtual; abstract;
    procedure InitKey(const Key: String; IVector: Pointer);
{reset the Feedbackregister with the actual IVector}
    procedure Done; virtual;
{protect the security Data's, Feedback, Buffer, Vector etc.}
    procedure Protect; virtual;
{en/decode a TStream, Source and Dest can be the same or Dest = nil
 when DataSize < 0 the Source.Size is used}
    procedure EncodeStream(const Source, Dest: TStream; DataSize: Integer);
    procedure DecodeStream(const Source, Dest: TStream; DataSize: Integer);
{en/decode a File}
    procedure EncodeFile(const Source, Dest: String);
    procedure DecodeFile(const Source, Dest: String);
{en/decode a Memory, Source and Dest can be the same}
    procedure EncodeBuffer(const Source; var Dest; DataSize: Integer);
    procedure DecodeBuffer(const Source; var Dest; DataSize: Integer);
{en/decode a String}
    function EncodeString(const Source: String): String;
    function DecodeString(const Source: String): String;
{the Cipher Mode = cmXXX}
    property Mode: TCipherMode read FMode write FMode;
{the Current Hash-Object, build a Hash from InitKey()}
    property Hash: THash read GetHash;
{the Class of the Hash-Object}
    property HashClass: THashClass read FHashClass write SetHashClass;
{the maximal KeySize and BufSize (Size of Feedback, Buffer and Vector}
    property KeySize: Integer read FKeySize;
    property BufSize: Integer read FBufSize;
{when the Key is set with InitKey() = HasHashKey and IncludeHashKey = True then
 En/Decodefile & En/Decodestream read, write at first the Key}
    property IncludeHashKey: Boolean index 8 read GetFlag write SetFlag;
{Init() was called}
    property Initialized: Boolean index 9 read GetFlag write SetFlag;
{the actual IVector, BufSize Bytes}
    property Vector: Pointer read FVector;
{the Feedback register, BufSize Bytes}
    property Feedback: Pointer read FFeedback;
  end;

  TCipher_1DES = class(TCipher)
  protected
    class procedure GetContext(var ABufSize, AKeySize, AUserSize: Integer); override;
    class function TestVector: Pointer; override;
    procedure Encode(Data: Pointer); override;
    procedure Decode(Data: Pointer); override;
    procedure MakeKey(const Data: array of Byte; Key: PInteger; Reverse: Boolean);
  public
    procedure Init(const Key; Size: Integer; IVector: Pointer); override;
  end;

procedure RaiseCipherException(const ErrorCode: Integer; const Msg: String);

implementation

resourcestring
  sInvalidKey      = 'Encryptionkey is invalid';
  sInvalidKeySize  = 'Length from Encryptionkey is invalid.'#13#10+
                     'Keysize for %s must be to %d-%d bytes';
  sNotInitialized  = '%s is not initialized call Init() or InitKey() before.';

type
  TCodeProc     = procedure(const Source; var Dest; DataSize: Integer) of object;

const
  maxBufSize    = 1024 * 4;  {Buffersize for File, Stream-Access}

procedure RaiseCipherException(const ErrorCode: Integer; const Msg: String);
var
  E: ECipherException;
begin
  E := ECipherException.Create(Msg);
  E.ErrorCode := ErrorCode;
  raise E;
end;

procedure XORBuffers(I1, I2: Pointer; Size: Integer; Dest: Pointer); assembler;
{begin
  while Size >= 4 do
  begin
    Dec(Size, 4);
    PInteger(Dest)^ := PInteger(I1)^ xor PInteger(I2)^;
    Inc(PInteger(Dest));
    Inc(PInteger(I1));
    Inc(PInteger(I2));
  end;
  while Size > 0 do
  begin
    Dec(Size);
    PByte(Dest)^ := PByte(I1)^ xor PByte(I2)^;
    Inc(PChar(Dest));
    Inc(PChar(I1));
    Inc(PChar(I2));
  end;
end;}

asm
       JCXZ  @@3
       PUSH  ESI
       PUSH  EDI
       MOV   ESI,EAX
       MOV   EDI,Dest
       TEST  ECX,1
       JZ    @@0
       DEC   ECX
       MOV   AL,[ESI + ECX]
       XOR   AL,[EDX + ECX]
       MOV   [EDI + ECX],AL
       JCXZ  @@2
@@0:   SHR   ECX,1
       TEST  ECX,1
       JZ    @@00
       DEC   ECX
       MOV   AX,[ESI + ECX * 2]
       XOR   AX,[EDX + ECX * 2]
       MOV   [EDI + ECX * 2],AX
       JCXZ  @@2
@@00:  SHR   ECX,1
@@1:   JCXZ  @@2
       DEC   ECX
       MOV   EAX,[ESI + ECX * 4]
       XOR   EAX,[EDX + ECX * 4]
       MOV   [EDI + ECX * 4],EAX
       JMP   @@1
@@2:   POP   EDI
       POP   ESI
@@3:
end;

function TCipher.GetFlag(Index: Integer): Boolean;
begin
  Result := FFlags and (1 shl Index) <> 0;
end;

procedure TCipher.SetFlag(Index: Integer; Value: Boolean);
begin
  Index := 1 shl Index;
  if Value then FFlags := FFlags or Index
    else FFlags := FFlags and not Index;
end;

procedure TCipher.InitBegin(var Size: Integer);
begin
  Initialized := False;
  Protect;
  if Size < 0 then Size := 0;
  if Size > KeySize then
    Size := KeySize;
end;

procedure TCipher.InitEnd(IVector: Pointer);
begin
  if IVector = nil then Encode(Vector)
    else Move(IVector^, Vector^, BufSize);
  Move(Vector^, Feedback^, BufSize);
  Initialized := True;
end;

procedure TCipher.Encode(Data: Pointer);
begin
end;

procedure TCipher.Decode(Data: Pointer);
begin
end;

destructor TCipher.Destroy;
begin
  Protect;
  ReallocMem(FVector, 0);
  ReallocMem(FFeedback, 0);
  ReallocMem(FBuffer, 0);
  ReallocMem(FUser, 0);
  FHash.Free;
  FHash := nil;
  inherited Destroy;
end;

class function TCipher.NewInstance: TObject;
begin
  Result := inherited NewInstance;
  if Result = nil then Exit;
  with TCipher(Result) do
  begin
    GetContext(FBufSize, FKeySize, FUserSize);
    GetMem(FVector, FBufSize);
    GetMem(FFeedback, FBufSize);
    GetMem(FBuffer, FBufSize);
    GetMem(FUser, FUserSize);
    Protect;
  end;
end;

class function TCipher.MaxKeySize: Integer;
var
  Dummy: Integer;
begin
  GetContext(Dummy, Result, Dummy);
end;

class function TCipher.SelfTest: Boolean;
var
  Data: array[0..63] of Char;
  Key: String;
begin
  Result       := InitTestIsOk; {have anonyme modified the testvectors ?}
{we will use the ClassName as Key :-)}
  Key          := ClassName;
  with Self.Create do
  try
    Mode := cmCTS;
    Init(PChar(Key)^, Length(Key), nil);
    EncodeBuffer(GetTestVector^, Data, 32);
    Result := Result and CompareMem(TestVector, @Data, 32);
    Done;
    DecodeBuffer(Data, Data, 32);
    Result := Result and CompareMem(GetTestVector, @Data, 32);
  finally
    Free;
  end;
  FillChar(Data, SizeOf(Data), 0);
end;

procedure TCipher.InitKey(const Key: String; IVector: Pointer);
var
  I: Integer;
begin
  Hash.Init;
  Hash.Calc(PChar(Key)^, Length(Key));
  Hash.Done;
  I := Hash.DigestKeySize;
  if I > FKeySize then I := FKeySize; {generaly will truncute to large Keys}
  Init(Hash.DigestKey^, I, IVector);
  EncodeBuffer(Hash.DigestKey^, Hash.DigestKey^, Hash.DigestKeySize);
  Done;
  HasHashKey := True;
end;

procedure TCipher.Done;
begin
  Move(FVector^, FFeedback^, FBufSize);
end;

procedure TCipher.Protect;
begin
  HasHashKey := False;
  Initialized := False;
  FillChar(FVector^, FBufSize, $FF);
  FillChar(FFeedback^, FBufSize, $FF);
  FillChar(FBuffer^, FBufSize, 0);
  FillChar(FUser^, FUserSize, 0);
end;

function TCipher.GetHash: THash;
begin
  if FHash = nil then
  begin
    if FHashClass = nil then FHashClass := THash_SHA1;
    FHash := FHashClass.Create;
  end;
  Result := FHash;
end;

procedure TCipher.SetHashClass(Value: THashClass);
begin
  if Value <> FHashClass then
  begin
    FHash.Free;
    FHash := nil;
    FHashClass := Value;
  end;
end;

procedure TCipher.InternalCodeStream(Source, Dest: TStream; DataSize: Integer; Encode: Boolean);
var
  Buf: PChar;
  Over: PChar;
  OverSize: Integer;
  SPos: Integer;
  DPos: Integer;
  Len: Integer;
  Proc: TCodeProc;
  Size: Integer;
begin
  if Source = nil then Exit;
  if Encode then Proc := EncodeBuffer else Proc := DecodeBuffer;
  if Dest = nil then Dest := Source;
  if DataSize < 0 then
  begin
    DataSize := Source.Size;
    Source.Position := 0;
  end;
  Buf := nil;
  Over := nil;
  OverSize := 0;
  Size := DataSize;
  DoProgress(Self, 0, Size);
  try
    Buf    := AllocMem(maxBufSize);
    DPos   := Dest.Position;
    SPos   := Source.Position;
    if IncludeHashKey and HasHashKey then
      if Encode then
      begin
        if Source = Dest then
        begin
          OverSize := Hash.DigestKeySize;
          Over := AllocMem(OverSize);
          OverSize := Source.Read(Over^, OverSize);
          SPos := Source.Position;
        end;
        Dest.Position := DPos;
        Dest.Write(Hash.DigestKey^, Hash.DigestKeySize);
        DPos := Dest.Position;
      end else
      begin
        OverSize := Hash.DigestKeySize;
        OverSize := Source.Read(Buf^, OverSize);
        if not CompareMem(Buf, Hash.DigestKey, Hash.DigestKeySize) then
          RaiseCipherException(errInvalidKey, sInvalidKey);
        SPos := Source.Position;
//        Dec(DataSize, OverSize);
      end;
    while DataSize > 0 do
    begin
      Source.Position := SPos;
      Len := DataSize;
      if Len > maxBufSize then Len := maxBufSize;
      if Over <> nil then
      begin
        if Len < OverSize then
        begin
          Move(Over^, Buf^, Len);
          Move(PByteArray(Over)[Len], Over^, OverSize - Len);
          Dec(OverSize, Len);
          OverSize := Source.Read(PByteArray(Over)[OverSize], Len) + OverSize;
        end else
        begin
          Move(Over^, Buf^, OverSize);
          Dec(Len, OverSize);
          Len := Source.Read(PChar(Buf + OverSize)^, Len) + OverSize;
          OverSize := Source.Read(Over^, OverSize);
        end;
      end else Len := Source.Read(Buf^, Len);
      SPos := Source.Position;
      if Len <= 0 then Break;
      Proc(Buf^, Buf^, Len);
      Dest.Position := DPos;
      Dest.Write(Buf^, Len);
      DPos := Dest.Position;
      Dec(DataSize, Len);
      DoProgress(Self, Size - DataSize, Size);
    end;
    if IncludeHashKey and HasHashKey and (Dest = Source) then
      if Encode and (Over <> nil) then
      begin
        while OverSize > 0 do
        begin
          Len := maxBufSize;
          Move(Over^, Buf^, OverSize);
          Dec(Len, OverSize);
          Source.Position := SPos;
          Len := Source.Read(PChar(Buf + OverSize)^, Len) + OverSize;
          OverSize := Source.Read(Over^, OverSize);
          SPos := Source.Position;
          Source.Position := DPos;
          Source.Write(Buf^, Len);
          DPos := Source.Position;
        end;
      end else
        if not Encode then
        begin
          repeat
            Source.Position := SPos;
            Len := Source.Read(Buf^, maxBufSize);
            SPos := Source.Position;
            Source.Position := DPos;
            Source.Write(Buf^, Len);
            DPos := Source.Position;
          until Len <= 0;
          Source.Size := Source.Position;
        end;
  finally
    DoProgress(Self, 0, 0);
    ReallocMem(Buf, 0);
    ReallocMem(Over, 0);
  end;
end;

procedure TCipher.InternalCodeFile(const Source, Dest: String; Encode: Boolean);
var
  S,D: TFileStream;
begin
  S := nil;
  D := nil;
  try
    if (AnsiCompareText(Source, Dest) <> 0) and (Trim(Dest) <> '') then
    begin
      S := TFileStream.Create(Source, fmOpenRead or fmShareDenyNone);
      if not FileExists(Dest) then D := TFileStream.Create(Dest, fmCreate)
        else D := TFileStream.Create(Dest, fmOpenReadWrite);
    end else
    begin
      S := TFileStream.Create(Source, fmOpenReadWrite);
      D := S;
    end;
    InternalCodeStream(S, D, -1, Encode);
  finally
    S.Free;
    if S <> D then
    begin
      D.Size := D.Position;
      D.Free;
    end;
  end;
end;

procedure TCipher.EncodeStream(const Source, Dest: TStream; DataSize: Integer);
begin
  InternalCodeStream(Source, Dest, DataSize, True);
end;

procedure TCipher.DecodeStream(const Source, Dest: TStream; DataSize: Integer);
begin
  InternalCodeStream(Source, Dest, DataSize, False);
end;

procedure TCipher.EncodeFile(const Source, Dest: String);
begin
  InternalCodeFile(Source, Dest, True);
end;

procedure TCipher.DecodeFile(const Source, Dest: String);
begin
  InternalCodeFile(Source, Dest, False);
end;

function TCipher.EncodeString(const Source: String): String;
begin
  SetLength(Result, Length(Source));
  EncodeBuffer(PChar(Source)^, PChar(Result)^, Length(Source));
end;

function TCipher.DecodeString(const Source: String): String;
begin
  SetLength(Result, Length(Source));
  DecodeBuffer(PChar(Source)^, PChar(Result)^, Length(Source));
end;

procedure TCipher.EncodeBuffer(const Source; var Dest; DataSize: Integer);
var
  S,D,F: PByte;
begin
  if not Initialized then
    RaiseCipherException(errNotInitialized, Format(sNotInitialized, [ClassName]));
  S := @Source;
  D := @Dest;
  case FMode of
    cmECB:
      begin
        if S <> D then Move(S^, D^, DataSize);
        while DataSize >= FBufSize do
        begin
          Encode(D);
          Inc(D, FBufSize);
          Dec(DataSize, FBufSize);
        end;
        if DataSize > 0 then
        begin
          Move(D^, FBuffer^, DataSize);
          Encode(FBuffer);
          Move(FBuffer^, D^, DataSize);
        end;
      end;
    cmCTS:
      begin
        while DataSize >= FBufSize do
        begin
          XORBuffers(S, FFeedback, FBufSize, D);
          Encode(D);
          XORBuffers(D, FFeedback, FBufSize, FFeedback);
{old Code here}
{          XORBuffers(S, F, FBufSize, D);
          Encode(D);
          F := D;}

          Inc(S, FBufSize);
          Inc(D, FBufSize);
          Dec(DataSize, FBufSize);
        end;
//        Move(F^, FFeedback^, FBufSize);
        if DataSize > 0 then
        begin
          Move(FFeedback^, FBuffer^, FBufSize);
          Encode(FBuffer);
          XORBuffers(S, FBuffer, DataSize, D);
          XORBuffers(FBuffer, FFeedback, FBufSize, FFeedback);
        end;
      end;
    cmCBC:
      begin
        F := FFeedback;
        while DataSize >= FBufSize do
        begin
          XORBuffers(S, F, FBufSize, D);
          Encode(D);
          F := D;
          Inc(S, FBufSize);
          Inc(D, FBufSize);
          Dec(DataSize, FBufSize);
        end;
        Move(F^, FFeedback^, FBufSize);
        if DataSize > 0 then
        begin
          Move(FFeedback^, FBuffer^, FBufSize);
          Encode(FBuffer);
          XORBuffers(S, FBuffer, DataSize, D);
        end;
      end;
    cmCFB:
      while DataSize > 0 do
      begin
        Move(FFeedback^, FBuffer^, FBufSize);
        Encode(FBuffer);
        D^ := S^ xor PByte(FBuffer)^;
        Move(PByteArray(FFeedback)[1], FFeedback^, FBufSize-1);
        PByteArray(FFeedback)[FBufSize-1] := D^;
//        SHIFTBuffers(FFeedback, PByteArray(D), FBufSize, 8);
        Inc(D);
        Inc(S);
        Dec(DataSize);
      end;
    cmOFB:
      while DataSize > 0 do
      begin
        Move(FFeedback^, FBuffer^, FBufSize);
        Encode(FBuffer);
        D^ := S^ xor PByte(FBuffer)^;
        Move(PByteArray(FFeedback)[1], FFeedback^, FBufSize-1);
        PByteArray(FFeedback)[FBufSize-1] := PByte(FBuffer)^;
//        SHIFTBuffers(FFeedback, PByteArray(D), FBufSize, 8);
        Inc(D);
        Inc(S);
        Dec(DataSize);
      end;
  end;
  FillChar(FBuffer^, FBufSize, 0);
end;

procedure TCipher.DecodeBuffer(const Source; var Dest; DataSize: Integer);
var
  S,D,F,B: PByte;
  K: Byte;
begin
  if not Initialized then
    RaiseCipherException(errNotInitialized, Format(sNotInitialized, [ClassName]));
  S := @Source;
  D := @Dest;
  case FMode of
    cmECB:
      begin
        if S <> D then Move(S^, D^, DataSize);
        while DataSize >= FBufSize do
        begin
          Decode(D);
          Inc(D, FBufSize);
          Dec(DataSize, FBufSize);
        end;
        if DataSize > 0 then
        begin
          Move(D^, FBuffer^, DataSize);
          Encode(FBuffer);
          Move(FBuffer^, D^, DataSize);
        end;
      end;
    cmCTS:
      begin
        if S <> D then Move(S^, D^, DataSize);
        F := FFeedback;
        B := FBuffer;
        while DataSize >= FBufSize do
        begin
          XORBuffers(D, F, FBufSize, B);
          Decode(D);
          XORBuffers(D, F, FBufSize, D);

{old code here}
{          Move(D^, B^, FBufSize);
          Decode(D);
          XORBuffers(F, D, FBufSize, D);}
          S := B;
          B := F;
          F := S;
          Inc(D, FBufSize);
          Dec(DataSize, FBufSize);
        end;
        if F <> FFeedback then Move(F^, FFeedback^, FBufSize);
        if DataSize > 0 then
        begin
          Move(FFeedback^, FBuffer^, FBufSize);
          Encode(FBuffer);
          XORBuffers(FBuffer, D, DataSize, D);
          XORBuffers(FBuffer, FFeedback, FBufSize, FFeedback);
        end;
      end;
    cmCBC:
      begin
        if S <> D then Move(S^, D^, DataSize);
        F := FFeedback;
        B := FBuffer;
        while DataSize >= FBufSize do
        begin
          Move(D^, B^, FBufSize);
          Decode(D);
          XORBuffers(F, D, FBufSize, D);
          S := B;
          B := F;
          F := S;
          Inc(D, FBufSize);
          Dec(DataSize, FBufSize);
        end;
        if F <> FFeedback then Move(F^, FFeedback^, FBufSize);
        if DataSize > 0 then
        begin
          Move(FFeedback^, FBuffer^, FBufSize);
          Encode(FBuffer);
          XORBuffers(D, FBuffer, DataSize, D);
        end;
      end;
    cmCFB:
      while DataSize > 0 do
      begin
        Move(FFeedback^, FBuffer^, FBufSize);
        Encode(FBuffer);
        Move(PByteArray(FFeedback)[1], FFeedback^, FBufSize-1);
        PByteArray(FFeedback)[FBufSize-1] := S^;
//        SHIFTBuffers(FFeedback, PByteArray(S), FBufSize, 8);
        D^ := S^ xor PByte(FBuffer)^;
        Inc(D);
        Inc(S);
        Dec(DataSize);
      end;
    cmOFB:
      while DataSize > 0 do
      begin
        Move(FFeedback^, FBuffer^, FBufSize);
        Encode(FBuffer);
        D^ := S^ xor PByte(FBuffer)^;
        Move(PByteArray(FFeedback)[1], FFeedback^, FBufSize-1);
        PByteArray(FFeedback)[FBufSize-1] := PByte(FBuffer)^;
//        SHIFTBuffers(FFeedback, PByteArray(D), FBufSize, 8);
        Inc(D);
        Inc(S);
        Dec(DataSize);
      end;
  end;
  FillChar(FBuffer^, FBufSize, 0);
end;

procedure DES_Func(Data: PIntArray; Key: PInteger); register;
var
  L,R,X,Y,I: LongWord;
begin
  L := SwapInteger(Data[0]);
  R := SwapInteger(Data[1]);

  X := (L shr  4 xor R) and $0F0F0F0F; R := R xor X; L := L xor X shl  4;
  X := (L shr 16 xor R) and $0000FFFF; R := R xor X; L := L xor X shl 16;
  X := (R shr  2 xor L) and $33333333; L := L xor X; R := R xor X shl  2;
  X := (R shr  8 xor L) and $00FF00FF; L := L xor X; R := R xor X shl  8;

  R := R shl 1 or R shr 31;
  X := (L xor R) and $AAAAAAAA;
  R := R xor X;
  L := L xor X;
  L := L shl 1 or L shr 31;

  for I := 0 to 7 do
  begin
    X := (R shl 28 or R shr 4) xor Key^; Inc(Key);
    Y := R xor Key^;                     Inc(Key);
    L := L xor (DES_Data[0, X        and $3F] or DES_Data[1, X shr  8 and $3F] or
                DES_Data[2, X shr 16 and $3F] or DES_Data[3, X shr 24 and $3F] or
                DES_Data[4, Y        and $3F] or DES_Data[5, Y shr  8 and $3F] or
                DES_Data[6, Y shr 16 and $3F] or DES_Data[7, Y shr 24 and $3F]);

    X := (L shl 28 or L shr 4) xor Key^; Inc(Key);
    Y := L xor Key^;                     Inc(Key);
    R := R xor (DES_Data[0, X        and $3F] or DES_Data[1, X shr  8 and $3F] or
                DES_Data[2, X shr 16 and $3F] or DES_Data[3, X shr 24 and $3F] or
                DES_Data[4, Y        and $3F] or DES_Data[5, Y shr  8 and $3F] or
                DES_Data[6, Y shr 16 and $3F] or DES_Data[7, Y shr 24 and $3F]);
  end;

  R := R shl 31 or R shr 1;
  X := (L xor R) and $AAAAAAAA;
  R := R xor X;
  L := L xor X;
  L := L shl 31 or L shr 1;

  X := (L shr  8 xor R) and $00FF00FF; R := R xor X; L := L xor X shl  8;
  X := (L shr  2 xor R) and $33333333; R := R xor X; L := L xor X shl  2;
  X := (R shr 16 xor L) and $0000FFFF; L := L xor X; R := R xor X shl 16;
  X := (R shr  4 xor L) and $0F0F0F0F; L := L xor X; R := R xor X shl  4;

  Data[0] := SwapInteger(R);
  Data[1] := SwapInteger(L);
end;

class procedure TCipher_1DES.GetContext(var ABufSize, AKeySize, AUserSize: Integer);
begin
  ABufSize := 8;
  AKeySize := 8;
  AUserSize := 32 * 4 * 2;
end;

class function TCipher_1DES.TestVector: Pointer;
asm
         MOV   EAX,OFFSET @Vector
         RET
@Vector: DB    0ADh,069h,042h,0BBh,0F6h,068h,020h,04Dh
         DB    053h,0CDh,0C7h,062h,013h,093h,098h,0C0h
         DB    030h,00Dh,085h,00Bh,0E2h,0AAh,072h,009h
         DB    06Fh,0DBh,05Fh,08Eh,0D3h,0E4h,0CFh,08Ah
end;

procedure TCipher_1DES.Encode(Data: Pointer);
begin
  DES_Func(Data, User);
end;

procedure TCipher_1DES.Decode(Data: Pointer);
begin
  DES_Func(Data, @PIntArray(User)[32]);
end;

procedure TCipher_1DES.MakeKey(const Data: array of Byte; Key: PInteger; Reverse: Boolean);
const
  ROT: array[0..15] of Byte = (1,2,4,6,8,10,12,14,15,17,19,21,23,25,27,28);
var
  I,J,L,M,N: LongWord;
  PC_M, PC_R: array[0..55] of Byte;
  K: array[0..31] of LongWord;
begin
  FillChar(K, SizeOf(K), 0);
  for I := 0 to 55 do
    if Data[DES_PC1[I] shr 3] and ($80 shr (DES_PC1[I] and $07)) <> 0 then PC_M[I] := 1
      else PC_M[I] := 0;
  for I := 0 to 15 do
  begin
    if Reverse then M := (15 - I) shl 1 else M := I shl 1;
    N := M + 1;
    for J := 0 to 27 do
    begin
      L := J + ROT[I];
      if L < 28 then PC_R[J] := PC_M[L] else PC_R[J] := PC_M[L - 28];
    end;
    for J := 28 to 55 do
    begin
      L := J + ROT[I];
      if L < 56 then PC_R[J] := PC_M[L] else PC_R[J] := PC_M[L - 28];
    end;
    L := $1000000;
    for J := 0 to 23 do
    begin
      L := L shr 1;
      if PC_R[DES_PC2[J     ]] <> 0 then K[M] := K[M] or L;
      if PC_R[DES_PC2[J + 24]] <> 0 then K[N] := K[N] or L;
    end;
  end;
  for I := 0 to 15 do
  begin
    M := I shl 1; N := M + 1;
    Key^ := K[M] and $00FC0000 shl  6 or
            K[M] and $00000FC0 shl 10 or
            K[N] and $00FC0000 shr 10 or
            K[N] and $00000FC0 shr  6;
    Inc(Key);
    Key^ := K[M] and $0003F000 shl 12 or
            K[M] and $0000003F shl 16 or
            K[N] and $0003F000 shr  4 or
            K[N] and $0000003F;
    Inc(Key);
  end;
end;

procedure TCipher_1DES.Init(const Key; Size: Integer; IVector: Pointer);
var
  K: array[0..7] of Byte;
begin
  InitBegin(Size);
  FillChar(K, SizeOf(K), 0);
  Move(Key, K, Size);
  MakeKey(K, User, False);
  MakeKey(K, @PIntArray(User)[32], True);
  FillChar(K, SizeOf(K), 0);
  InitEnd(IVector);
end;

end.
