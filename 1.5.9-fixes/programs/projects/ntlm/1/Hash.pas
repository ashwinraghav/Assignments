{Copyright:      Hagen Reddmann  mailto:HaReddmann@AOL.COM
 Author:         Hagen Reddmann
 Remarks:        freeware, but this Copyright must be included
 known Problems: none
 Version:        2.2,  Part I from Delphi Encryption Compendium
                 Delphi 2-4, designed and testet under D3
 Description:    Include Objects for calculating various Hash's (fingerprints)
                 used Hash-Algorithm:
                   MD4, MD5, SHA, SHA1, RipeMD (128 - 320),
                   Haval (128 - 256), Snefru, Square, Tiger,
                   Sapphire II (128 - 320)
                 used Checksum-Algo:
                   CRC32, XOR32bit, XOR16bit, CRC16-CCITT, CRC16-Standard

 Comments:       for Designer's, the Buffer from Method Transform must be readonly


Digest size, Result from DigestKeySize and Datasize for Digest
   MD4         16 byte   128 bit   4x Integer
   MD5         16 byte   128 bit   4x Integer
   SHA         20 byte   160 bit   5x Integer
   SHA1        20 byte   160 bit   5x Integer
   RMD128      16 byte   128 bit   4x Integer
   RMD160      20 byte   160 bit   5x Integer
   RMD256      32 byte   256 bit   8x Integer
   RMD320      40 byte   320 bit  10x Integer
   Haval256    32 byte   256 bit   8x Integer
   Haval224    28 byte   224 bit   7x Integer
   Haval192    24 byte   192 bit   6x Integer
   Haval160    20 byte   160 bit   5x Integer
   Haval128    16 byte   128 bit   4x Integer
   Sapphire320 40 byte   320 bit  10x Integer
   Sapphire288 36 byte   288 bit   9x Integer
   Sapphire256 32 byte   256 bit   8x Integer
   Sapphire224 28 byte   224 bit   7x Integer
   Sapphire192 24 byte   192 bit   6x Integer
   Sapphire160 20 byte   160 bit   5x Integer
   Sapphire128 16 byte   128 bit   4x Integer
   Snefru      32 byte   256 bit   8x Integer
   Square      16 byte   128 bit   4x Integer
   Tiger       24 byte   192 bit   6x Integer   in D4 used 64bit Arithmetic

   XOR16     2 byte    16 bit   1x Word
   XOR32     4 byte    32 bit   1x Integer
   CRC32     4 byte    32 bit   1x Integer
   CRC16     2 byte    16 bit   1x Word

Version 2.2
  added   SHA (or SHS pseudo for SHA),
          Sapphire II (128, 160, 192, 224, 256, 288, 320)
          Square, (Sapphire & Square demonstrate the using from a Cipher to make a hash)
          CRC16_CCITT, CRC16_Standard

          ProgressEvent

  bug fixes  CRC32 offset Problem, and in THash_CRC32.Done inverse the Result
             Base16ToStr Len updating

  changed  Trailorbyte from all THash_HavalXXX from $80 to $01

Version 2.1
  added   Snefru, RipeMD128, RipeMD256, RipeMD320, Tiger

  added   Rounds, from 3-5, for THaval_xxx
          3 Rounds 174 % faster than with 5 Rounds, PII 266 12.74 Mb/sec
          4 Rounds 121 % faster than with 5 Rounds, PII 266  8.88 mb/sec
          5 Rounds                                  PII 266  7.32 mb/sec


  added Self-Test support, Methods THash.SelfTest and THash.TestVector;

  speeded up   MD4       - 137 %
               MD5       - 126 %
               SHA1      - 134 % for <= 386   and 148 % for >= 486 CPU
               RipeMD160 - 140 %
               Haval     - 173 %
}


unit Hash;

interface

uses Classes;

type
{$IFNDEF UseInt64} //for Delphi 2-3
  LongWord  = Integer;
{$ENDIF}
  PByte     = ^Byte;
  PInteger  = ^LongWord;
  PWord     = ^Word;
  PIntArray = ^TIntArray;
  TIntArray = array[0..1023] of LongWord;

{Progress (gauge) for Hash and Cipher}
  TProgressEvent = procedure(Sender: TObject; Current, Maximal: Integer) of Object;

{all Hash Classes}
  THash_MD4             = class;
  THash_MD5             = class;
  THash_RipeMD128       = class;
  THash_RipeMD160       = class;
  THash_RipeMD256       = class;
  THash_RipeMD320       = class;
  THash_SHA             = class;
  THash_SHA1            = class;
  THash_Sapphire320     = class; {demonstrate the using from a Cipher to hashing}
  THash_Sapphire288     = class;
  THash_Sapphire256     = class;
  THash_Sapphire224     = class;
  THash_Sapphire192     = class;
  THash_Sapphire160     = class;
  THash_Sapphire128     = class;
{all Checksum Classes}
  THash_XOR16           = class;
  THash_XOR32           = class;
  THash_CRC16_CCITT     = class;
  Thash_CRC16_Standard  = class;
  THash_CRC32           = class;


 {the Base-Class of all Hashs}
  THashClass = class of THash;

  THash = class(TPersistent)
  private
    function GetDigestStr(Index: Integer): String;
  protected
    class function TestVector: Pointer; virtual; abstract;
  public
    destructor Destroy; override;
    procedure Init; virtual;
    procedure Calc(const Data; DataSize: Integer); virtual; abstract;
    procedure Done; virtual;
    function DigestKey: Pointer; virtual; abstract;

    class function DigestKeySize: Integer; virtual; abstract;
    class function CalcBuffer(Digest: Pointer; const Buffer; BufferSize: Integer): String;
    class function CalcStream(Digest: Pointer; const Stream: TStream; StreamSize: Integer): String;
    class function CalcString(Digest: Pointer; const Data: String): String;
    class function CalcFile(Digest: Pointer; const FileName: String): String;
{test the correct working}
    class function SelfTest: Boolean;

{give back the Digest binary in a String}
    property DigestKeyStr: String index -1 read GetDigestStr;
{give back the Default String Format from the Digest}
    property DigestString: String index  0 read GetDigestStr;
{give back a HEX-String form the Digest}
    property DigestBase16: String index 16 read GetDigestStr;
{give back a Base64-MIME String}
    property DigestBase64: String index 64 read GetDigestStr;
  end;

  THash_MD4 = class(THash)
  private
    FCount: LongWord;
    FBuffer: array[0..63] of Byte;
    FDigest: array[0..9] of LongWord;
  protected
    class function TestVector: Pointer; override;
    procedure Transform(Buffer: PIntArray); virtual;
  public
    class function DigestKeySize: Integer; override;
    procedure Init; override;
    procedure Done; override;
    procedure Calc(const Data; DataSize: Integer); override;
    function DigestKey: Pointer; override;
  end;

  THash_MD5 = class(THash_MD4)
  protected
    class function TestVector: Pointer; override;
    procedure Transform(Buffer: PIntArray); override;
  end;

  THash_RipeMD128 = class(THash_MD4)
  protected
    class function TestVector: Pointer; override;
    procedure Transform(Buffer: PIntArray); override;
  end;

  THash_RipeMD160 = class(THash_MD4)
  protected
    class function TestVector: Pointer; override;
    procedure Transform(Buffer: PIntArray); override;
  public
{DigestKey-Size 160 bit}
    class function DigestKeySize: Integer; override;
  end;

  THash_RipeMD256 = class(THash_MD4)
  protected
    class function TestVector: Pointer; override;
    procedure Transform(Buffer: PIntArray); override;
  public
{DigestKey-Size 256 bit}
    class function DigestKeySize: Integer; override;
    procedure Init; override;
  end;

  THash_RipeMD320 = class(THash_MD4)
  protected
    class function TestVector: Pointer; override;
    procedure Transform(Buffer: PIntArray); override;
  public
{DigestKey-Size 320 bit}
    class function DigestKeySize: Integer; override;
  end;

  THash_SHA = class(THash_RipeMD160)
  private
    FRotate: Boolean;
  protected
    class function TestVector: Pointer; override;
    procedure Transform(Buffer: PIntArray); override;
  public
    procedure Done; override;
  end;

  THash_SHA1 = class(THash_SHA)
  protected
    class function TestVector: Pointer; override;
  public
    class function NewInstance: TObject; override;
  end;

  THash_Sapphire320 = class(THash)
  private
    FCount: LongWord;
    FRotor: Byte;
    FRatchet: Byte;
    FAvalanche: Byte;
    FPlain: Byte;
    FCipher: Byte;
    FCards: array[0..255] of Byte;
    FDigest: array[0..9] of LongWord;
  protected
    class function TestVector: Pointer; override;
  public
    class function DigestKeySize: Integer; override;
    procedure Init; override;
    procedure Calc(const Data; DataSize: Integer); override;
    procedure Done; override;
    function DigestKey: Pointer; override;
  end;

  THash_Sapphire288 = class(THash_Sapphire320)
  protected
    class function TestVector: Pointer; override;
  public
    class function DigestKeySize: Integer; override;
  end;

  THash_Sapphire256 = class(THash_Sapphire320)
  protected
    class function TestVector: Pointer; override;
  public
    class function DigestKeySize: Integer; override;
  end;

  THash_Sapphire224 = class(THash_Sapphire320)
  protected
    class function TestVector: Pointer; override;
  public
    class function DigestKeySize: Integer; override;
  end;

  THash_Sapphire192 = class(THash_Sapphire320)
  protected
    class function TestVector: Pointer; override;
  public
    class function DigestKeySize: Integer; override;
  end;

  THash_Sapphire160 = class(THash_Sapphire320)
  protected
    class function TestVector: Pointer; override;
  public
    class function DigestKeySize: Integer; override;
  end;

  THash_Sapphire128 = class(THash_Sapphire320)
  protected
    class function TestVector: Pointer; override;
  public
    class function DigestKeySize: Integer; override;
  end;

  THash_XOR16 = class(THash)
  private
    FCRC: Word;
  protected
    class function TestVector: Pointer; override;
  public
    class function DigestKeySize: Integer; override;
    procedure Init; override;
    procedure Calc(const Data; DataSize: Integer); override;
    function DigestKey: Pointer; override;
  end;

  THash_XOR32 = class(THash)
  private
    FCRC: LongWord;
  protected
    class function TestVector: Pointer; override;
  public
    class function DigestKeySize: Integer; override;
    procedure Init; override;
    procedure Calc(const Data; DataSize: Integer); override;
    function DigestKey: Pointer; override;
  end;

  THash_CRC32 = class(THash_XOR32)
  private
  protected
    class function TestVector: Pointer; override;
  public
    procedure Init; override;
    procedure Calc(const Data; DataSize: Integer); override;
    procedure Done; override;
  end;

  THash_CRC16_CCITT = class(THash_XOR16)
  private
  protected
    class function TestVector: Pointer; override;
  public
    procedure Init; override;
    procedure Calc(const Data; DataSize: Integer); override;
  end;

  THash_CRC16_Standard = class(THash_XOR16)
  private
  protected
    class function TestVector: Pointer; override;
  public
    procedure Calc(const Data; DataSize: Integer); override;
  end;

{calculate CRC32 Checksum, CRC is default $FFFFFFFF, after calc you must inverse Result with NOT}
function CRC32(CRC: LongWord; Data: Pointer; DataSize: LongWord): LongWord;
function GetTestVector: PChar;
{String convert.}
function StrToBase64(Value: PChar; Len: Integer): String;
function Base64ToStr(Value: PChar; Len: Integer): String;
function StrToBase16(Value: PChar; Len: Integer): String;
function Base16ToStr(Value: PChar; Len: Integer): String;
{Utility funcs}
function ROL(Value: LongWord; Shift: Integer): LongWord;
function ROLADD(Value, Add: LongWord; Shift: Integer): LongWord;
function ROLSUB(Value, Sub: LongWord; Shift: Integer): LongWord;
function ROR(Value: LongWord; Shift: Integer): LongWord;
function RORADD(Value, Add: LongWord; Shift: Integer): LongWord;
function RORSUB(Value, Sub: LongWord; Shift: Integer): LongWord;
function SwapBits(Value: LongWord): LongWord;
function CPUType: Integer; {3 = 386, 4 = 486, 5 = Pentium, 6 > Pentium}
procedure DoProgress(Sender: TObject; Current, Maximal: Integer);

const
{change this to 16 - HEX, 64 - Base64 MIME or -1 - binary}
  DefaultDigestStringFormat : Integer = 64;
  InitTestIsOk              : Boolean = False;

{this is set to SwapInt for <= 386 and BSwapInt >= 486 CPU, don't modify}
  SwapInteger       : function(Value: LongWord): LongWord  = nil;
{Count of Integers Buffer}
  SwapIntegerBuffer : procedure(Source, Dest: Pointer; Count: Integer) = nil;
{Progress callback function, set this to your Progresscallback}
  Progress: TProgressEvent = nil;

implementation

uses SysUtils;

const
  maxBufSize = 1024 * 4;  {Buffersize for File, Stream-Access}
  FCPUType : Integer = 0;

function CPUType: Integer;
begin
  Result := FCPUType;
end;

{I am missing the INLINE Statement :-( }
function ROL(Value: LongWord; Shift: Integer): LongWord; assembler;
asm
       MOV   CL,DL
       ROL   EAX,CL
end;

function ROLADD(Value, Add: LongWord; Shift: Integer): LongWord; assembler;
asm
       ROL   EAX,CL
       ADD   EAX,EDX
end;

function ROLSUB(Value, Sub: LongWord; Shift: Integer): LongWord; assembler;
asm
       ROL   EAX,CL
       SUB   EAX,EDX
end;

function ROR(Value: LongWord; Shift: Integer): LongWord; assembler;
asm
       MOV   CL,DL
       ROR   EAX,CL
end;

function RORADD(Value, Add: LongWord; Shift: Integer): LongWord; assembler;
asm
       ROR  EAX,CL
       ADD  EAX,EDX
end;

function RORSUB(Value, Sub: LongWord; Shift: Integer): LongWord; assembler;
asm
       ROR  EAX,CL
       SUB  EAX,EDX
end;
{swap 4 Bytes Intel}
function SwapInt(Value: LongWord): LongWord; assembler; register;
asm
       XCHG  AH,AL
       ROL   EAX,16
       XCHG  AH,AL
end;

function BSwapInt(Value: LongWord): LongWord; assembler; register;
asm
       BSWAP  EAX
end;

procedure SwapIntBuf(Source,Dest: Pointer; Count: Integer); assembler; register;
asm
       JCXZ   @Exit
       PUSH   EBX
       SUB    EAX,4
       SUB    EDX,4
@@1:   MOV    EBX,[EAX + ECX * 4]
       XCHG   BL,BH
       ROL    EBX,16
       XCHG   BL,BH
       MOV    [EDX + ECX * 4],EBX
       LOOP   @@1
       POP    EBX
@Exit:
end;

procedure BSwapIntBuf(Source, Dest: Pointer; Count: Integer); assembler; register;
asm
       JCXZ   @Exit
       PUSH   EBX
       SUB    EAX,4
       SUB    EDX,4
@@1:   MOV    EBX,[EAX + ECX * 4]
       BSWAP  EBX
       MOV    [EDX + ECX * 4],EBX
       LOOP   @@1
       POP    EBX
@Exit:
end;
{reverse the bit order from a integer}
function SwapBits(Value: LongWord): LongWord;
asm
       CMP    FCPUType,3
       JLE    @@1
       BSWAP  EAX
       JMP    @@2
@@1:   XCHG   AH,AL
       ROL    EAX,16
       XCHG   AH,AL
@@2:   MOV    EDX,EAX
       AND    EAX,0AAAAAAAAh
       SHR    EAX,1
       AND    EDX,055555555h
       SHL    EDX,1
       OR     EAX,EDX
       MOV    EDX,EAX
       AND    EAX,0CCCCCCCCh
       SHR    EAX,2
       AND    EDX,033333333h
       SHL    EDX,2
       OR     EAX,EDX
       MOV    EDX,EAX
       AND    EAX,0F0F0F0F0h
       SHR    EAX,4
       AND    EDX,00F0F0F0Fh
       SHL    EDX,4
       OR     EAX,EDX
end;


procedure DoProgress(Sender: TObject; Current, Maximal: Integer);
begin
{saver access}
  try
    if (TMethod(Progress).Code <> nil) and
       ((TMethod(Progress).Data = nil) or
        (TObject(TMethod(Progress).Data) is TObject)) then
      Progress(Sender, Current, Maximal);
  except
    Progress := nil;
  end;
end;

function StrToBase64(Value: PChar; Len: Integer): String;
const
  Table: PChar = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/';
var
  B,J,L: Integer;
  S: PByte;
  D: PChar;
begin
  Result := '';
  if Value = nil then Exit;
  if Len < 0 then Len := StrLen(Value);
  L := Len;
  SetLength(Result, (L+2) div 3 * 4);
  D := PChar(Result);
  B := 0;
  S := PByte(Value);
  while L > 0 do
  begin
    for J := 1 to 3 do
    begin
      if L > 0 then
      begin
        B := B or S^;
        Inc(S);
        Dec(L);
      end;
      B := B shl 8;
    end;
    while B <> 0 do
    begin
      B := ROL(B, 6);
      D^ := Table[B and $3F];
      Inc(D);
      B := B and not $3F;
    end;
  end;
  SetLength(Result, D - PChar(Result));
  if Len mod 3 = 1 then Result := Result + '==' else
    if Len mod 3 = 2 then Result := Result + '=';
end;

function Base64ToStr(Value: PChar; Len: Integer): String;
var
  B,J: Integer;
  D: PChar;
  S: PByte;
begin
  Result := '';
  if Value = nil then Exit;
  if Len < 0 then Len := StrLen(Value);
  SetLength(Result, Len);
  if Len = 0 then Exit;
  Move(PChar(Value)^, PChar(Result)^, Len);
  while Len and 3 <> 0 do
  begin
    Result := Result + '=';
    Inc(Len);
  end;
  D := PChar(Result);
  S := PByte(Result);
  Len := Len div 4 * 3;
  while Len > 0 do
  begin
    B := 0;
    for J := 1 to 4 do
    begin
      if (S^ >= 97) and (S^ <= 122) then Inc(B, S^ - 71) else
        if (S^ >= 65) and (S^ <= 90) then Inc(B, S^ - 65) else
          if (S^ >= 48) and (S^ <= 57) then Inc(B, S^ + 4) else
            if S^ = 43 then Inc(B, 62) else
              if S^ <> 61 then Inc(B, 63) else Dec(Len);
      B := B shl 6;
      Inc(S);
    end;
    B := ROL(B, 2);
    for J := 1 to 3 do
    begin
      if Len <= 0 then Break;
      B := ROL(B, 8);
      D^ := Char(B);
      Inc(D);
      Dec(Len);
    end;
  end;
  SetLength(Result, D - PChar(Result));
end;

function StrToBase16(Value: PChar; Len: Integer): String;
const
  H: array[0..15] of Char = '0123456789ABCDEF';
var
  S: PByte;
  D: PChar;
begin
  Result := '';
  if Value = nil then Exit;
  if Len < 0 then Len := StrLen(Value);
  SetLength(Result, Len * 2);
  if Len = 0 then Exit;
  D := PChar(Result);
  S := PByte(Value);
  while Len > 0 do
  begin
    D^ := H[S^ shr  4]; Inc(D);
    D^ := H[S^ and $F]; Inc(D);
    Inc(S);
    Dec(Len);
  end;
end;

function Base16ToStr(Value: PChar; Len: Integer): String;
var
  D: PByte;
  V: Byte;
  S: PChar;
begin
  Result := '';
  if Value = nil then Exit;
  if Len < 0 then Len := StrLen(Value);
  SetLength(Result, (Len +1) div 2);
  D := PByte(Result);
  S := PChar(Value);
  while Len > 0 do
  begin
    V := Byte(UpCase(S^));
    Inc(S);
    if V > Byte('9') then D^ := V - Byte('A') + 10
      else D^ := V - Byte('0');
    V := Byte(UpCase(S^));
    Inc(S);
    D^ := D^ shl 4;
    if V > Byte('9') then D^ := D^ or (V - Byte('A') + 10)
      else D^ := D^ or (V - Byte('0'));
    Dec(Len, 2);
    Inc(D);
  end;
  SetLength(Result, PChar(D) - PChar(Result));
end;

function CRC32(CRC: LongWord; Data: Pointer; DataSize: LongWord): LongWord; assembler;
asm
         OR     EDX,EDX
         JE     @Exit
         JCXZ   @Exit
         PUSH   EBX
@Start:
         MOVZX  EBX,AL
         XOR    BL,[EDX]
         SHR    EAX,8
//ops, in Version 2.1     XOR  EAX,CS:[EBX + OFFSET @CRC32] :-)
         XOR    EAX,CS:[EBX * 4 + OFFSET @CRC32]
         INC    EDX
         LOOP   @Start
         POP    EBX
@Exit:   RET

@CRC32:  DD 000000000h, 077073096h, 0EE0E612Ch, 0990951BAh
         DD 0076DC419h, 0706AF48Fh, 0E963A535h, 09E6495A3h
         DD 00EDB8832h, 079DCB8A4h, 0E0D5E91Eh, 097D2D988h
         DD 009B64C2Bh, 07EB17CBDh, 0E7B82D07h, 090BF1D91h
         DD 01DB71064h, 06AB020F2h, 0F3B97148h, 084BE41DEh
         DD 01ADAD47Dh, 06DDDE4EBh, 0F4D4B551h, 083D385C7h
         DD 0136C9856h, 0646BA8C0h, 0FD62F97Ah, 08A65C9ECh
         DD 014015C4Fh, 063066CD9h, 0FA0F3D63h, 08D080DF5h
         DD 03B6E20C8h, 04C69105Eh, 0D56041E4h, 0A2677172h
         DD 03C03E4D1h, 04B04D447h, 0D20D85FDh, 0A50AB56Bh
         DD 035B5A8FAh, 042B2986Ch, 0DBBBC9D6h, 0ACBCF940h
         DD 032D86CE3h, 045DF5C75h, 0DCD60DCFh, 0ABD13D59h
         DD 026D930ACh, 051DE003Ah, 0C8D75180h, 0BFD06116h
         DD 021B4F4B5h, 056B3C423h, 0CFBA9599h, 0B8BDA50Fh
         DD 02802B89Eh, 05F058808h, 0C60CD9B2h, 0B10BE924h
         DD 02F6F7C87h, 058684C11h, 0C1611DABh, 0B6662D3Dh
         DD 076DC4190h, 001DB7106h, 098D220BCh, 0EFD5102Ah
         DD 071B18589h, 006B6B51Fh, 09FBFE4A5h, 0E8B8D433h
         DD 07807C9A2h, 00F00F934h, 09609A88Eh, 0E10E9818h
         DD 07F6A0DBBh, 0086D3D2Dh, 091646C97h, 0E6635C01h
         DD 06B6B51F4h, 01C6C6162h, 0856530D8h, 0F262004Eh
         DD 06C0695EDh, 01B01A57Bh, 08208F4C1h, 0F50FC457h
         DD 065B0D9C6h, 012B7E950h, 08BBEB8EAh, 0FCB9887Ch
         DD 062DD1DDFh, 015DA2D49h, 08CD37CF3h, 0FBD44C65h
         DD 04DB26158h, 03AB551CEh, 0A3BC0074h, 0D4BB30E2h
         DD 04ADFA541h, 03DD895D7h, 0A4D1C46Dh, 0D3D6F4FBh
         DD 04369E96Ah, 0346ED9FCh, 0AD678846h, 0DA60B8D0h
         DD 044042D73h, 033031DE5h, 0AA0A4C5Fh, 0DD0D7CC9h
         DD 05005713Ch, 0270241AAh, 0BE0B1010h, 0C90C2086h
         DD 05768B525h, 0206F85B3h, 0B966D409h, 0CE61E49Fh
         DD 05EDEF90Eh, 029D9C998h, 0B0D09822h, 0C7D7A8B4h
         DD 059B33D17h, 02EB40D81h, 0B7BD5C3Bh, 0C0BA6CADh
         DD 0EDB88320h, 09ABFB3B6h, 003B6E20Ch, 074B1D29Ah
         DD 0EAD54739h, 09DD277AFh, 004DB2615h, 073DC1683h
         DD 0E3630B12h, 094643B84h, 00D6D6A3Eh, 07A6A5AA8h
         DD 0E40ECF0Bh, 09309FF9Dh, 00A00AE27h, 07D079EB1h
         DD 0F00F9344h, 08708A3D2h, 01E01F268h, 06906C2FEh
         DD 0F762575Dh, 0806567CBh, 0196C3671h, 06E6B06E7h
         DD 0FED41B76h, 089D32BE0h, 010DA7A5Ah, 067DD4ACCh
         DD 0F9B9DF6Fh, 08EBEEFF9h, 017B7BE43h, 060B08ED5h
         DD 0D6D6A3E8h, 0A1D1937Eh, 038D8C2C4h, 04FDFF252h
         DD 0D1BB67F1h, 0A6BC5767h, 03FB506DDh, 048B2364Bh
         DD 0D80D2BDAh, 0AF0A1B4Ch, 036034AF6h, 041047A60h
         DD 0DF60EFC3h, 0A867DF55h, 0316E8EEFh, 04669BE79h
         DD 0CB61B38Ch, 0BC66831Ah, 0256FD2A0h, 05268E236h
         DD 0CC0C7795h, 0BB0B4703h, 0220216B9h, 05505262Fh
         DD 0C5BA3BBEh, 0B2BD0B28h, 02BB45A92h, 05CB36A04h
         DD 0C2D7FFA7h, 0B5D0CF31h, 02CD99E8Bh, 05BDEAE1Dh
         DD 09B64C2B0h, 0EC63F226h, 0756AA39Ch, 0026D930Ah
         DD 09C0906A9h, 0EB0E363Fh, 072076785h, 005005713h
         DD 095BF4A82h, 0E2B87A14h, 07BB12BAEh, 00CB61B38h
         DD 092D28E9Bh, 0E5D5BE0Dh, 07CDCEFB7h, 00BDBDF21h
         DD 086D3D2D4h, 0F1D4E242h, 068DDB3F8h, 01FDA836Eh
         DD 081BE16CDh, 0F6B9265Bh, 06FB077E1h, 018B74777h
         DD 088085AE6h, 0FF0F6A70h, 066063BCAh, 011010B5Ch
         DD 08F659EFFh, 0F862AE69h, 0616BFFD3h, 0166CCF45h
         DD 0A00AE278h, 0D70DD2EEh, 04E048354h, 03903B3C2h
         DD 0A7672661h, 0D06016F7h, 04969474Dh, 03E6E77DBh
         DD 0AED16A4Ah, 0D9D65ADCh, 040DF0B66h, 037D83BF0h
         DD 0A9BCAE53h, 0DEBB9EC5h, 047B2CF7Fh, 030B5FFE9h
         DD 0BDBDF21Ch, 0CABAC28Ah, 053B39330h, 024B4A3A6h
         DD 0BAD03605h, 0CDD70693h, 054DE5729h, 023D967BFh
         DD 0B3667A2Eh, 0C4614AB8h, 05D681B02h, 02A6F2B94h
         DD 0B40BBE37h, 0C30C8EA1h, 05A05DF1Bh, 02D02EF8Dh
         DD 074726F50h, 0736E6F69h, 0706F4320h, 067697279h
         DD 028207468h, 031202963h, 020393939h, 048207962h
         DD 06E656761h, 064655220h, 06E616D64h, 06FBBA36Eh
end;

{a Random generated Testvector 256bit - 32 Bytes, it's used for Testing the Hash}
function GetTestVector: PChar;
asm
         MOV   EAX,OFFSET @Vector
         RET
@Vector: DB    030h,044h,0EDh,06Eh,045h,0A4h,096h,0F5h
         DB    0F6h,035h,0A2h,0EBh,03Dh,01Ah,05Dh,0D6h
         DB    0CBh,01Dh,009h,082h,02Dh,0BDh,0F5h,060h
         DB    0C2h,0B8h,058h,0A1h,091h,0F9h,081h,0B1h
         DB    000h
end;

destructor THash.Destroy;
begin
  FillChar(DigestKey^, DigestKeySize, 0);
  inherited Destroy;
end;

procedure THash.Init;
begin
end;

procedure THash.Done;
begin
end;

function THash.GetDigestStr(Index: Integer): String;
begin
  if Index = 0 then Index := DefaultDigestStringFormat;
  case Index of
    16: Result := StrToBase16(PChar(DigestKey), DigestKeySize);
    64: Result := StrToBase64(PChar(DigestKey), DigestKeySize);
  else
    begin
      SetLength(Result, DigestKeySize);
      Move(DigestKey^, PChar(Result)^, DigestKeySize);
    end;
  end;
end;

class function THash.CalcStream(Digest: Pointer; const Stream: TStream; StreamSize: Integer): String;
var
  Buf: Pointer;
  BufSize: Integer;
  Size: Integer;
  H: THash;
begin
  H := Create;
  with H do
  try
    Buf := AllocMem(maxBufSize);
    Init;
    if StreamSize < 0 then
 {if Size < 0 then reset the Position, otherwise, calc with the specific
  Size and from the aktual Position in the Stream}
    begin
      Stream.Position := 0;
      StreamSize := Stream.Size;
    end;
    Size := StreamSize;
    DoProgress(H, 0, Size);
    repeat
      BufSize := StreamSize;
      if BufSize > maxBufSize then BufSize := maxBufSize;
      BufSize := Stream.Read(Buf^, BufSize);
      if BufSize <= 0 then Break;
      Calc(Buf^, BufSize);
      Dec(StreamSize, BufSize);
      DoProgress(H, Size - StreamSize, Size);
    until BufSize <= 0;
    Done;
    if Digest <> nil then Move(DigestKey^, Digest^, DigestKeySize);
    Result := DigestString;
  finally
    DoProgress(H, 0, 0);
    Free;
    ReallocMem(Buf, 0);
  end;
end;

class function THash.CalcString(Digest: Pointer; const Data: String): String;
begin
  with Self.Create do
  try
    Init;
    Calc(PChar(Data)^, Length(Data));
    Done;
    Result := DigestString;
    if Digest <> nil then Move(DigestKey^, Digest^, DigestKeySize);
  finally
    Free;
  end;
end;

class function THash.CalcFile(Digest: Pointer; const FileName: String): String;
var
  S: TFileStream;
begin
  S := TFileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
  try
    Result := CalcStream(Digest, S, -1);
  finally
    S.Free;
  end;
end;

class function THash.CalcBuffer(Digest: Pointer; const Buffer; BufferSize: Integer): String;
begin
  with Create do {create an Object from my Classtype}
  try
    Init;
    Calc(Buffer, BufferSize);
    Done;
    if Digest <> nil then Move(DigestKey^, Digest^, DigestKeySize);
    Result := DigestString;
  finally
    Free; {destroy it}
  end;
end;

class function THash.SelfTest: Boolean;
var
  Test: String;
begin
  SetLength(Test, DigestKeySize);
  CalcBuffer(PChar(Test), GetTestVector^, 32);
  Result := InitTestIsOk and CompareMem(PChar(Test), TestVector, DigestKeySize);
end;

class function THash_MD4.TestVector: Pointer; assembler;
asm
         MOV   EAX,OFFSET @Vector
         RET
@Vector: DB    025h,0EAh,0BFh,0CCh,08Ch,0C9h,06Fh,0D9h
         DB    02Dh,0CFh,07Eh,0BDh,07Fh,087h,07Ch,07Ch
end;

procedure THash_MD4.Transform(Buffer: PIntArray);
{calculate the Digest, fast}
var
  A, B, C, D: LongWord;
begin
  A := FDigest[0];
  B := FDigest[1];
  C := FDigest[2];
  D := FDigest[3];

  Inc(A, Buffer[ 0] + (B and C or not B and D)); A := A shl  3 or A shr 29;
  Inc(D, Buffer[ 1] + (A and B or not A and C)); D := D shl  7 or D shr 25;
  Inc(C, Buffer[ 2] + (D and A or not D and B)); C := C shl 11 or C shr 21;
  Inc(B, Buffer[ 3] + (C and D or not C and A)); B := B shl 19 or B shr 13;
  Inc(A, Buffer[ 4] + (B and C or not B and D)); A := A shl  3 or A shr 29;
  Inc(D, Buffer[ 5] + (A and B or not A and C)); D := D shl  7 or D shr 25;
  Inc(C, Buffer[ 6] + (D and A or not D and B)); C := C shl 11 or C shr 21;
  Inc(B, Buffer[ 7] + (C and D or not C and A)); B := B shl 19 or B shr 13;
  Inc(A, Buffer[ 8] + (B and C or not B and D)); A := A shl  3 or A shr 29;
  Inc(D, Buffer[ 9] + (A and B or not A and C)); D := D shl  7 or D shr 25;
  Inc(C, Buffer[10] + (D and A or not D and B)); C := C shl 11 or C shr 21;
  Inc(B, Buffer[11] + (C and D or not C and A)); B := B shl 19 or B shr 13;
  Inc(A, Buffer[12] + (B and C or not B and D)); A := A shl  3 or A shr 29;
  Inc(D, Buffer[13] + (A and B or not A and C)); D := D shl  7 or D shr 25;
  Inc(C, Buffer[14] + (D and A or not D and B)); C := C shl 11 or C shr 21;
  Inc(B, Buffer[15] + (C and D or not C and A)); B := B shl 19 or B shr 13;

  Inc(A, Buffer[ 0] + $5A827999 + (B and C or B and D or C and D)); A := A shl  3 or A shr 29;
  Inc(D, Buffer[ 4] + $5A827999 + (A and B or A and C or B and C)); D := D shl  5 or D shr 27;
  Inc(C, Buffer[ 8] + $5A827999 + (D and A or D and B or A and B)); C := C shl  9 or C shr 23;
  Inc(B, Buffer[12] + $5A827999 + (C and D or C and A or D and A)); B := B shl 13 or B shr 19;
  Inc(A, Buffer[ 1] + $5A827999 + (B and C or B and D or C and D)); A := A shl  3 or A shr 29;
  Inc(D, Buffer[ 5] + $5A827999 + (A and B or A and C or B and C)); D := D shl  5 or D shr 27;
  Inc(C, Buffer[ 9] + $5A827999 + (D and A or D and B or A and B)); C := C shl  9 or C shr 23;
  Inc(B, Buffer[13] + $5A827999 + (C and D or C and A or D and A)); B := B shl 13 or B shr 19;
  Inc(A, Buffer[ 2] + $5A827999 + (B and C or B and D or C and D)); A := A shl  3 or A shr 29;
  Inc(D, Buffer[ 6] + $5A827999 + (A and B or A and C or B and C)); D := D shl  5 or D shr 27;
  Inc(C, Buffer[10] + $5A827999 + (D and A or D and B or A and B)); C := C shl  9 or C shr 23;
  Inc(B, Buffer[14] + $5A827999 + (C and D or C and A or D and A)); B := B shl 13 or B shr 19;
  Inc(A, Buffer[ 3] + $5A827999 + (B and C or B and D or C and D)); A := A shl  3 or A shr 29;
  Inc(D, Buffer[ 7] + $5A827999 + (A and B or A and C or B and C)); D := D shl  5 or D shr 27;
  Inc(C, Buffer[11] + $5A827999 + (D and A or D and B or A and B)); C := C shl  9 or C shr 23;
  Inc(B, Buffer[15] + $5A827999 + (C and D or C and A or D and A)); B := B shl 13 or B shr 19;

  Inc(A, Buffer[ 0] + $6ED9EBA1 + (B xor C xor D)); A := A shl  3 or A shr 29;
  Inc(D, Buffer[ 8] + $6ED9EBA1 + (A xor B xor C)); D := D shl  9 or D shr 23;
  Inc(C, Buffer[ 4] + $6ED9EBA1 + (D xor A xor B)); C := C shl 11 or C shr 21;
  Inc(B, Buffer[12] + $6ED9EBA1 + (C xor D xor A)); B := B shl 15 or B shr 17;
  Inc(A, Buffer[ 2] + $6ED9EBA1 + (B xor C xor D)); A := A shl  3 or A shr 29;
  Inc(D, Buffer[10] + $6ED9EBA1 + (A xor B xor C)); D := D shl  9 or D shr 23;
  Inc(C, Buffer[ 6] + $6ED9EBA1 + (D xor A xor B)); C := C shl 11 or C shr 21;
  Inc(B, Buffer[14] + $6ED9EBA1 + (C xor D xor A)); B := B shl 15 or B shr 17;
  Inc(A, Buffer[ 1] + $6ED9EBA1 + (B xor C xor D)); A := A shl  3 or A shr 29;
  Inc(D, Buffer[ 9] + $6ED9EBA1 + (A xor B xor C)); D := D shl  9 or D shr 23;
  Inc(C, Buffer[ 5] + $6ED9EBA1 + (D xor A xor B)); C := C shl 11 or C shr 21;
  Inc(B, Buffer[13] + $6ED9EBA1 + (C xor D xor A)); B := B shl 15 or B shr 17;
  Inc(A, Buffer[ 3] + $6ED9EBA1 + (B xor C xor D)); A := A shl  3 or A shr 29;
  Inc(D, Buffer[11] + $6ED9EBA1 + (A xor B xor C)); D := D shl  9 or D shr 23;
  Inc(C, Buffer[ 7] + $6ED9EBA1 + (D xor A xor B)); C := C shl 11 or C shr 21;
  Inc(B, Buffer[15] + $6ED9EBA1 + (C xor D xor A)); B := B shl 15 or B shr 17;

  Inc(FDigest[0], A);
  Inc(FDigest[1], B);
  Inc(FDigest[2], C);
  Inc(FDigest[3], D);
end;

class function THash_MD4.DigestKeySize: Integer;
begin
  Result := 16;
end;

function THash_MD4.DigestKey: Pointer;
begin
  Result := @FDigest;
end;

procedure THash_MD4.Init;
begin
  FillChar(FBuffer, SizeOf(FBuffer), 0);
{all Descend from MD4 (MD4, SHA1, RipeMD128, RipeMD160, RipeMD256) use this Init-Key}
  FDigest[0] := $67452301;
  FDigest[1] := $EFCDAB89;
  FDigest[2] := $98BADCFE;
  FDigest[3] := $10325476;
  FDigest[4] := $C3D2E1F0;
{for RMD320}
  FDigest[5] := $76543210;
  FDigest[6] := $FEDCBA98;
  FDigest[7] := $89ABCDEF;
  FDigest[8] := $01234567;
  FDigest[9] := $3C2D1E0F;
  FCount := 0;
end;

procedure THash_MD4.Done;
var
  I: Integer;
  S: Comp;
begin
  I := FCount and $3F;
  FBuffer[I] := $80;
  Inc(I);
  if I > 64 - 8 then
  begin
    FillChar(FBuffer[I], 64 - I, 0);
    Transform(@FBuffer);
    I := 0;
  end;
  FillChar(FBuffer[I], 64 - I, 0);
  S := FCount * 8;
  Move(S, FBuffer[64 - 8], SizeOf(S));
  Transform(@FBuffer);
  FillChar(FBuffer, SizeOf(FBuffer), 0);
end;

procedure THash_MD4.Calc(const Data; DataSize: Integer);
var
  Index: Integer;
  P: PChar;
begin
  if DataSize <= 0 then Exit;
  Index := FCount and $3F;
  Inc(FCount, DataSize);
  if Index > 0 then
  begin
    if DataSize < 64 - Index then
    begin
      Move(Data, FBuffer[Index], DataSize);
      Exit;
    end;
    Move(Data, FBuffer[Index], 64 - Index);
    Transform(@FBuffer);
    Dec(DataSize, 64 - Index);
  end;
  P := @TByteArray(Data)[Index];
  Inc(Index, DataSize and not $3F);
  while DataSize >= 64 do
  begin
    Transform(Pointer(P));
    Inc(P, 64);
    Dec(DataSize, 64);
  end;
  Move(TByteArray(Data)[Index], FBuffer, DataSize);
end;

class function THash_MD5.TestVector: Pointer;
asm
         MOV   EAX,OFFSET @Vector
         RET
@Vector: DB    03Eh,0D8h,034h,08Ch,0D2h,0A4h,045h,0D6h
         DB    075h,05Dh,04Bh,0C9h,0FEh,0DCh,0C2h,0C6h
end;

procedure THash_MD5.Transform(Buffer: PIntArray);
var
  A, B, C, D: LongWord;
begin
  A := FDigest[0];
  B := FDigest[1];
  C := FDigest[2];
  D := FDigest[3];

  Inc(A, Buffer[ 0] + $D76AA478 + (D xor (B and (C xor D)))); A := A shl  7 or A shr 25 + B;
  Inc(D, Buffer[ 1] + $E8C7B756 + (C xor (A and (B xor C)))); D := D shl 12 or D shr 20 + A;
  Inc(C, Buffer[ 2] + $242070DB + (B xor (D and (A xor B)))); C := C shl 17 or C shr 15 + D;
  Inc(B, Buffer[ 3] + $C1BDCEEE + (A xor (C and (D xor A)))); B := B shl 22 or B shr 10 + C;
  Inc(A, Buffer[ 4] + $F57C0FAF + (D xor (B and (C xor D)))); A := A shl  7 or A shr 25 + B;
  Inc(D, Buffer[ 5] + $4787C62A + (C xor (A and (B xor C)))); D := D shl 12 or D shr 20 + A;
  Inc(C, Buffer[ 6] + $A8304613 + (B xor (D and (A xor B)))); C := C shl 17 or C shr 15 + D;
  Inc(B, Buffer[ 7] + $FD469501 + (A xor (C and (D xor A)))); B := B shl 22 or B shr 10 + C;
  Inc(A, Buffer[ 8] + $698098D8 + (D xor (B and (C xor D)))); A := A shl  7 or A shr 25 + B;
  Inc(D, Buffer[ 9] + $8B44F7AF + (C xor (A and (B xor C)))); D := D shl 12 or D shr 20 + A;
  Inc(C, Buffer[10] + $FFFF5BB1 + (B xor (D and (A xor B)))); C := C shl 17 or C shr 15 + D;
  Inc(B, Buffer[11] + $895CD7BE + (A xor (C and (D xor A)))); B := B shl 22 or B shr 10 + C;
  Inc(A, Buffer[12] + $6B901122 + (D xor (B and (C xor D)))); A := A shl  7 or A shr 25 + B;
  Inc(D, Buffer[13] + $FD987193 + (C xor (A and (B xor C)))); D := D shl 12 or D shr 20 + A;
  Inc(C, Buffer[14] + $A679438E + (B xor (D and (A xor B)))); C := C shl 17 or C shr 15 + D;
  Inc(B, Buffer[15] + $49B40821 + (A xor (C and (D xor A)))); B := B shl 22 or B shr 10 + C;

  Inc(A, Buffer[ 1] + $F61E2562 + (C xor (D and (B xor C)))); A := A shl  5 or A shr 27 + B;
  Inc(D, Buffer[ 6] + $C040B340 + (B xor (C and (A xor B)))); D := D shl  9 or D shr 23 + A;
  Inc(C, Buffer[11] + $265E5A51 + (A xor (B and (D xor A)))); C := C shl 14 or C shr 18 + D;
  Inc(B, Buffer[ 0] + $E9B6C7AA + (D xor (A and (C xor D)))); B := B shl 20 or B shr 12 + C;
  Inc(A, Buffer[ 5] + $D62F105D + (C xor (D and (B xor C)))); A := A shl  5 or A shr 27 + B;
  Inc(D, Buffer[10] + $02441453 + (B xor (C and (A xor B)))); D := D shl  9 or D shr 23 + A;
  Inc(C, Buffer[15] + $D8A1E681 + (A xor (B and (D xor A)))); C := C shl 14 or C shr 18 + D;
  Inc(B, Buffer[ 4] + $E7D3FBC8 + (D xor (A and (C xor D)))); B := B shl 20 or B shr 12 + C;
  Inc(A, Buffer[ 9] + $21E1CDE6 + (C xor (D and (B xor C)))); A := A shl  5 or A shr 27 + B;
  Inc(D, Buffer[14] + $C33707D6 + (B xor (C and (A xor B)))); D := D shl  9 or D shr 23 + A;
  Inc(C, Buffer[ 3] + $F4D50D87 + (A xor (B and (D xor A)))); C := C shl 14 or C shr 18 + D;
  Inc(B, Buffer[ 8] + $455A14ED + (D xor (A and (C xor D)))); B := B shl 20 or B shr 12 + C;
  Inc(A, Buffer[13] + $A9E3E905 + (C xor (D and (B xor C)))); A := A shl  5 or A shr 27 + B;
  Inc(D, Buffer[ 2] + $FCEFA3F8 + (B xor (C and (A xor B)))); D := D shl  9 or D shr 23 + A;
  Inc(C, Buffer[ 7] + $676F02D9 + (A xor (B and (D xor A)))); C := C shl 14 or C shr 18 + D;
  Inc(B, Buffer[12] + $8D2A4C8A + (D xor (A and (C xor D)))); B := B shl 20 or B shr 12 + C;

  Inc(A, Buffer[ 5] + $FFFA3942 + (B xor C xor D)); A := A shl  4 or A shr 28 + B;
  Inc(D, Buffer[ 8] + $8771F681 + (A xor B xor C)); D := D shl 11 or D shr 21 + A;
  Inc(C, Buffer[11] + $6D9D6122 + (D xor A xor B)); C := C shl 16 or C shr 16 + D;
  Inc(B, Buffer[14] + $FDE5380C + (C xor D xor A)); B := B shl 23 or B shr  9 + C;
  Inc(A, Buffer[ 1] + $A4BEEA44 + (B xor C xor D)); A := A shl  4 or A shr 28 + B;
  Inc(D, Buffer[ 4] + $4BDECFA9 + (A xor B xor C)); D := D shl 11 or D shr 21 + A;
  Inc(C, Buffer[ 7] + $F6BB4B60 + (D xor A xor B)); C := C shl 16 or C shr 16 + D;
  Inc(B, Buffer[10] + $BEBFBC70 + (C xor D xor A)); B := B shl 23 or B shr  9 + C;
  Inc(A, Buffer[13] + $289B7EC6 + (B xor C xor D)); A := A shl  4 or A shr 28 + B;
  Inc(D, Buffer[ 0] + $EAA127FA + (A xor B xor C)); D := D shl 11 or D shr 21 + A;
  Inc(C, Buffer[ 3] + $D4EF3085 + (D xor A xor B)); C := C shl 16 or C shr 16 + D;
  Inc(B, Buffer[ 6] + $04881D05 + (C xor D xor A)); B := B shl 23 or B shr  9 + C;
  Inc(A, Buffer[ 9] + $D9D4D039 + (B xor C xor D)); A := A shl  4 or A shr 28 + B;
  Inc(D, Buffer[12] + $E6DB99E5 + (A xor B xor C)); D := D shl 11 or D shr 21 + A;
  Inc(C, Buffer[15] + $1FA27CF8 + (D xor A xor B)); C := C shl 16 or C shr 16 + D;
  Inc(B, Buffer[ 2] + $C4AC5665 + (C xor D xor A)); B := B shl 23 or B shr  9 + C;

  Inc(A, Buffer[ 0] + $F4292244 + (C xor (B or not D))); A := A shl  6 or A shr 26 + B;
  Inc(D, Buffer[ 7] + $432AFF97 + (B xor (A or not C))); D := D shl 10 or D shr 22 + A;
  Inc(C, Buffer[14] + $AB9423A7 + (A xor (D or not B))); C := C shl 15 or C shr 17 + D;
  Inc(B, Buffer[ 5] + $FC93A039 + (D xor (C or not A))); B := B shl 21 or B shr 11 + C;
  Inc(A, Buffer[12] + $655B59C3 + (C xor (B or not D))); A := A shl  6 or A shr 26 + B;
  Inc(D, Buffer[ 3] + $8F0CCC92 + (B xor (A or not C))); D := D shl 10 or D shr 22 + A;
  Inc(C, Buffer[10] + $FFEFF47D + (A xor (D or not B))); C := C shl 15 or C shr 17 + D;
  Inc(B, Buffer[ 1] + $85845DD1 + (D xor (C or not A))); B := B shl 21 or B shr 11 + C;
  Inc(A, Buffer[ 8] + $6FA87E4F + (C xor (B or not D))); A := A shl  6 or A shr 26 + B;
  Inc(D, Buffer[15] + $FE2CE6E0 + (B xor (A or not C))); D := D shl 10 or D shr 22 + A;
  Inc(C, Buffer[ 6] + $A3014314 + (A xor (D or not B))); C := C shl 15 or C shr 17 + D;
  Inc(B, Buffer[13] + $4E0811A1 + (D xor (C or not A))); B := B shl 21 or B shr 11 + C;
  Inc(A, Buffer[ 4] + $F7537E82 + (C xor (B or not D))); A := A shl  6 or A shr 26 + B;
  Inc(D, Buffer[11] + $BD3AF235 + (B xor (A or not C))); D := D shl 10 or D shr 22 + A;
  Inc(C, Buffer[ 2] + $2AD7D2BB + (A xor (D or not B))); C := C shl 15 or C shr 17 + D;
  Inc(B, Buffer[ 9] + $EB86D391 + (D xor (C or not A))); B := B shl 21 or B shr 11 + C;

  Inc(FDigest[0], A);
  Inc(FDigest[1], B);
  Inc(FDigest[2], C);
  Inc(FDigest[3], D);
end;

class function THash_RipeMD128.TestVector: Pointer;
asm
         MOV   EAX,OFFSET @Vector
         RET
@Vector: DB    0CFh,0A0h,032h,0CFh,0D0h,08Fh,087h,03Ah
         DB    078h,0DFh,013h,0E7h,0EBh,0CDh,098h,00Fh
end;

procedure THash_RipeMD128.Transform(Buffer: PIntArray);
var
  A1, B1, C1, D1: LongWord;
  A2, B2, C2, D2: LongWord;
begin
  A1 := FDigest[0];
  B1 := FDigest[1];
  C1 := FDigest[2];
  D1 := FDigest[3];
  A2 := A1;
  B2 := B1;
  C2 := C1;
  D2 := D1;

  Inc(A1, B1 xor C1 xor D1 + Buffer[ 0]); A1 := A1 shl 11 or A1 shr 21;
  Inc(D1, A1 xor B1 xor C1 + Buffer[ 1]); D1 := D1 shl 14 or D1 shr 18;
  Inc(C1, D1 xor A1 xor B1 + Buffer[ 2]); C1 := C1 shl 15 or C1 shr 17;
  Inc(B1, C1 xor D1 xor A1 + Buffer[ 3]); B1 := B1 shl 12 or B1 shr 20;
  Inc(A1, B1 xor C1 xor D1 + Buffer[ 4]); A1 := A1 shl  5 or A1 shr 27;
  Inc(D1, A1 xor B1 xor C1 + Buffer[ 5]); D1 := D1 shl  8 or D1 shr 24;
  Inc(C1, D1 xor A1 xor B1 + Buffer[ 6]); C1 := C1 shl  7 or C1 shr 25;
  Inc(B1, C1 xor D1 xor A1 + Buffer[ 7]); B1 := B1 shl  9 or B1 shr 23;
  Inc(A1, B1 xor C1 xor D1 + Buffer[ 8]); A1 := A1 shl 11 or A1 shr 21;
  Inc(D1, A1 xor B1 xor C1 + Buffer[ 9]); D1 := D1 shl 13 or D1 shr 19;
  Inc(C1, D1 xor A1 xor B1 + Buffer[10]); C1 := C1 shl 14 or C1 shr 18;
  Inc(B1, C1 xor D1 xor A1 + Buffer[11]); B1 := B1 shl 15 or B1 shr 17;
  Inc(A1, B1 xor C1 xor D1 + Buffer[12]); A1 := A1 shl  6 or A1 shr 26;
  Inc(D1, A1 xor B1 xor C1 + Buffer[13]); D1 := D1 shl  7 or D1 shr 25;
  Inc(C1, D1 xor A1 xor B1 + Buffer[14]); C1 := C1 shl  9 or C1 shr 23;
  Inc(B1, C1 xor D1 xor A1 + Buffer[15]); B1 := B1 shl  8 or B1 shr 24;

  Inc(A1, B1 and C1 or not B1 and D1 + Buffer[ 7] + $5A827999); A1 := A1 shl  7 or A1 shr 25;
  Inc(D1, A1 and B1 or not A1 and C1 + Buffer[ 4] + $5A827999); D1 := D1 shl  6 or D1 shr 26;
  Inc(C1, D1 and A1 or not D1 and B1 + Buffer[13] + $5A827999); C1 := C1 shl  8 or C1 shr 24;
  Inc(B1, C1 and D1 or not C1 and A1 + Buffer[ 1] + $5A827999); B1 := B1 shl 13 or B1 shr 19;
  Inc(A1, B1 and C1 or not B1 and D1 + Buffer[10] + $5A827999); A1 := A1 shl 11 or A1 shr 21;
  Inc(D1, A1 and B1 or not A1 and C1 + Buffer[ 6] + $5A827999); D1 := D1 shl  9 or D1 shr 23;
  Inc(C1, D1 and A1 or not D1 and B1 + Buffer[15] + $5A827999); C1 := C1 shl  7 or C1 shr 25;
  Inc(B1, C1 and D1 or not C1 and A1 + Buffer[ 3] + $5A827999); B1 := B1 shl 15 or B1 shr 17;
  Inc(A1, B1 and C1 or not B1 and D1 + Buffer[12] + $5A827999); A1 := A1 shl  7 or A1 shr 25;
  Inc(D1, A1 and B1 or not A1 and C1 + Buffer[ 0] + $5A827999); D1 := D1 shl 12 or D1 shr 20;
  Inc(C1, D1 and A1 or not D1 and B1 + Buffer[ 9] + $5A827999); C1 := C1 shl 15 or C1 shr 17;
  Inc(B1, C1 and D1 or not C1 and A1 + Buffer[ 5] + $5A827999); B1 := B1 shl  9 or B1 shr 23;
  Inc(A1, B1 and C1 or not B1 and D1 + Buffer[ 2] + $5A827999); A1 := A1 shl 11 or A1 shr 21;
  Inc(D1, A1 and B1 or not A1 and C1 + Buffer[14] + $5A827999); D1 := D1 shl  7 or D1 shr 25;
  Inc(C1, D1 and A1 or not D1 and B1 + Buffer[11] + $5A827999); C1 := C1 shl 13 or C1 shr 19;
  Inc(B1, C1 and D1 or not C1 and A1 + Buffer[ 8] + $5A827999); B1 := B1 shl 12 or B1 shr 20;

  Inc(A1, (B1 or not C1) xor D1 + Buffer[ 3] + $6ED9EBA1); A1 := A1 shl 11 or A1 shr 21;
  Inc(D1, (A1 or not B1) xor C1 + Buffer[10] + $6ED9EBA1); D1 := D1 shl 13 or D1 shr 19;
  Inc(C1, (D1 or not A1) xor B1 + Buffer[14] + $6ED9EBA1); C1 := C1 shl  6 or C1 shr 26;
  Inc(B1, (C1 or not D1) xor A1 + Buffer[ 4] + $6ED9EBA1); B1 := B1 shl  7 or B1 shr 25;
  Inc(A1, (B1 or not C1) xor D1 + Buffer[ 9] + $6ED9EBA1); A1 := A1 shl 14 or A1 shr 18;
  Inc(D1, (A1 or not B1) xor C1 + Buffer[15] + $6ED9EBA1); D1 := D1 shl  9 or D1 shr 23;
  Inc(C1, (D1 or not A1) xor B1 + Buffer[ 8] + $6ED9EBA1); C1 := C1 shl 13 or C1 shr 19;
  Inc(B1, (C1 or not D1) xor A1 + Buffer[ 1] + $6ED9EBA1); B1 := B1 shl 15 or B1 shr 17;
  Inc(A1, (B1 or not C1) xor D1 + Buffer[ 2] + $6ED9EBA1); A1 := A1 shl 14 or A1 shr 18;
  Inc(D1, (A1 or not B1) xor C1 + Buffer[ 7] + $6ED9EBA1); D1 := D1 shl  8 or D1 shr 24;
  Inc(C1, (D1 or not A1) xor B1 + Buffer[ 0] + $6ED9EBA1); C1 := C1 shl 13 or C1 shr 19;
  Inc(B1, (C1 or not D1) xor A1 + Buffer[ 6] + $6ED9EBA1); B1 := B1 shl  6 or B1 shr 26;
  Inc(A1, (B1 or not C1) xor D1 + Buffer[13] + $6ED9EBA1); A1 := A1 shl  5 or A1 shr 27;
  Inc(D1, (A1 or not B1) xor C1 + Buffer[11] + $6ED9EBA1); D1 := D1 shl 12 or D1 shr 20;
  Inc(C1, (D1 or not A1) xor B1 + Buffer[ 5] + $6ED9EBA1); C1 := C1 shl  7 or C1 shr 25;
  Inc(B1, (C1 or not D1) xor A1 + Buffer[12] + $6ED9EBA1); B1 := B1 shl  5 or B1 shr 27;

  Inc(A1, B1 and D1 or C1 and not D1 + Buffer[ 1] + $8F1BBCDC); A1 := A1 shl 11 or A1 shr 21;
  Inc(D1, A1 and C1 or B1 and not C1 + Buffer[ 9] + $8F1BBCDC); D1 := D1 shl 12 or D1 shr 20;
  Inc(C1, D1 and B1 or A1 and not B1 + Buffer[11] + $8F1BBCDC); C1 := C1 shl 14 or C1 shr 18;
  Inc(B1, C1 and A1 or D1 and not A1 + Buffer[10] + $8F1BBCDC); B1 := B1 shl 15 or B1 shr 17;
  Inc(A1, B1 and D1 or C1 and not D1 + Buffer[ 0] + $8F1BBCDC); A1 := A1 shl 14 or A1 shr 18;
  Inc(D1, A1 and C1 or B1 and not C1 + Buffer[ 8] + $8F1BBCDC); D1 := D1 shl 15 or D1 shr 17;
  Inc(C1, D1 and B1 or A1 and not B1 + Buffer[12] + $8F1BBCDC); C1 := C1 shl  9 or C1 shr 23;
  Inc(B1, C1 and A1 or D1 and not A1 + Buffer[ 4] + $8F1BBCDC); B1 := B1 shl  8 or B1 shr 24;
  Inc(A1, B1 and D1 or C1 and not D1 + Buffer[13] + $8F1BBCDC); A1 := A1 shl  9 or A1 shr 23;
  Inc(D1, A1 and C1 or B1 and not C1 + Buffer[ 3] + $8F1BBCDC); D1 := D1 shl 14 or D1 shr 18;
  Inc(C1, D1 and B1 or A1 and not B1 + Buffer[ 7] + $8F1BBCDC); C1 := C1 shl  5 or C1 shr 27;
  Inc(B1, C1 and A1 or D1 and not A1 + Buffer[15] + $8F1BBCDC); B1 := B1 shl  6 or B1 shr 26;
  Inc(A1, B1 and D1 or C1 and not D1 + Buffer[14] + $8F1BBCDC); A1 := A1 shl  8 or A1 shr 24;
  Inc(D1, A1 and C1 or B1 and not C1 + Buffer[ 5] + $8F1BBCDC); D1 := D1 shl  6 or D1 shr 26;
  Inc(C1, D1 and B1 or A1 and not B1 + Buffer[ 6] + $8F1BBCDC); C1 := C1 shl  5 or C1 shr 27;
  Inc(B1, C1 and A1 or D1 and not A1 + Buffer[ 2] + $8F1BBCDC); B1 := B1 shl 12 or B1 shr 20;

  Inc(A2, B2 and D2 or C2 and not D2 + Buffer[ 5] + $50A28BE6); A2 := A2 shl  8 or A2 shr 24;
  Inc(D2, A2 and C2 or B2 and not C2 + Buffer[14] + $50A28BE6); D2 := D2 shl  9 or D2 shr 23;
  Inc(C2, D2 and B2 or A2 and not B2 + Buffer[ 7] + $50A28BE6); C2 := C2 shl  9 or C2 shr 23;
  Inc(B2, C2 and A2 or D2 and not A2 + Buffer[ 0] + $50A28BE6); B2 := B2 shl 11 or B2 shr 21;
  Inc(A2, B2 and D2 or C2 and not D2 + Buffer[ 9] + $50A28BE6); A2 := A2 shl 13 or A2 shr 19;
  Inc(D2, A2 and C2 or B2 and not C2 + Buffer[ 2] + $50A28BE6); D2 := D2 shl 15 or D2 shr 17;
  Inc(C2, D2 and B2 or A2 and not B2 + Buffer[11] + $50A28BE6); C2 := C2 shl 15 or C2 shr 17;
  Inc(B2, C2 and A2 or D2 and not A2 + Buffer[ 4] + $50A28BE6); B2 := B2 shl  5 or B2 shr 27;
  Inc(A2, B2 and D2 or C2 and not D2 + Buffer[13] + $50A28BE6); A2 := A2 shl  7 or A2 shr 25;
  Inc(D2, A2 and C2 or B2 and not C2 + Buffer[ 6] + $50A28BE6); D2 := D2 shl  7 or D2 shr 25;
  Inc(C2, D2 and B2 or A2 and not B2 + Buffer[15] + $50A28BE6); C2 := C2 shl  8 or C2 shr 24;
  Inc(B2, C2 and A2 or D2 and not A2 + Buffer[ 8] + $50A28BE6); B2 := B2 shl 11 or B2 shr 21;
  Inc(A2, B2 and D2 or C2 and not D2 + Buffer[ 1] + $50A28BE6); A2 := A2 shl 14 or A2 shr 18;
  Inc(D2, A2 and C2 or B2 and not C2 + Buffer[10] + $50A28BE6); D2 := D2 shl 14 or D2 shr 18;
  Inc(C2, D2 and B2 or A2 and not B2 + Buffer[ 3] + $50A28BE6); C2 := C2 shl 12 or C2 shr 20;
  Inc(B2, C2 and A2 or D2 and not A2 + Buffer[12] + $50A28BE6); B2 := B2 shl  6 or B2 shr 26;

  Inc(A2, (B2 or not C2) xor D2 + Buffer[ 6] + $5C4DD124); A2 := A2 shl  9 or A2 shr 23;
  Inc(D2, (A2 or not B2) xor C2 + Buffer[11] + $5C4DD124); D2 := D2 shl 13 or D2 shr 19;
  Inc(C2, (D2 or not A2) xor B2 + Buffer[ 3] + $5C4DD124); C2 := C2 shl 15 or C2 shr 17;
  Inc(B2, (C2 or not D2) xor A2 + Buffer[ 7] + $5C4DD124); B2 := B2 shl  7 or B2 shr 25;
  Inc(A2, (B2 or not C2) xor D2 + Buffer[ 0] + $5C4DD124); A2 := A2 shl 12 or A2 shr 20;
  Inc(D2, (A2 or not B2) xor C2 + Buffer[13] + $5C4DD124); D2 := D2 shl  8 or D2 shr 24;
  Inc(C2, (D2 or not A2) xor B2 + Buffer[ 5] + $5C4DD124); C2 := C2 shl  9 or C2 shr 23;
  Inc(B2, (C2 or not D2) xor A2 + Buffer[10] + $5C4DD124); B2 := B2 shl 11 or B2 shr 21;
  Inc(A2, (B2 or not C2) xor D2 + Buffer[14] + $5C4DD124); A2 := A2 shl  7 or A2 shr 25;
  Inc(D2, (A2 or not B2) xor C2 + Buffer[15] + $5C4DD124); D2 := D2 shl  7 or D2 shr 25;
  Inc(C2, (D2 or not A2) xor B2 + Buffer[ 8] + $5C4DD124); C2 := C2 shl 12 or C2 shr 20;
  Inc(B2, (C2 or not D2) xor A2 + Buffer[12] + $5C4DD124); B2 := B2 shl  7 or B2 shr 25;
  Inc(A2, (B2 or not C2) xor D2 + Buffer[ 4] + $5C4DD124); A2 := A2 shl  6 or A2 shr 26;
  Inc(D2, (A2 or not B2) xor C2 + Buffer[ 9] + $5C4DD124); D2 := D2 shl 15 or D2 shr 17;
  Inc(C2, (D2 or not A2) xor B2 + Buffer[ 1] + $5C4DD124); C2 := C2 shl 13 or C2 shr 19;
  Inc(B2, (C2 or not D2) xor A2 + Buffer[ 2] + $5C4DD124); B2 := B2 shl 11 or B2 shr 21;

  Inc(A2, B2 and C2 or not B2 and D2 + Buffer[15] + $6D703EF3); A2 := A2 shl  9 or A2 shr 23;
  Inc(D2, A2 and B2 or not A2 and C2 + Buffer[ 5] + $6D703EF3); D2 := D2 shl  7 or D2 shr 25;
  Inc(C2, D2 and A2 or not D2 and B2 + Buffer[ 1] + $6D703EF3); C2 := C2 shl 15 or C2 shr 17;
  Inc(B2, C2 and D2 or not C2 and A2 + Buffer[ 3] + $6D703EF3); B2 := B2 shl 11 or B2 shr 21;
  Inc(A2, B2 and C2 or not B2 and D2 + Buffer[ 7] + $6D703EF3); A2 := A2 shl  8 or A2 shr 24;
  Inc(D2, A2 and B2 or not A2 and C2 + Buffer[14] + $6D703EF3); D2 := D2 shl  6 or D2 shr 26;
  Inc(C2, D2 and A2 or not D2 and B2 + Buffer[ 6] + $6D703EF3); C2 := C2 shl  6 or C2 shr 26;
  Inc(B2, C2 and D2 or not C2 and A2 + Buffer[ 9] + $6D703EF3); B2 := B2 shl 14 or B2 shr 18;
  Inc(A2, B2 and C2 or not B2 and D2 + Buffer[11] + $6D703EF3); A2 := A2 shl 12 or A2 shr 20;
  Inc(D2, A2 and B2 or not A2 and C2 + Buffer[ 8] + $6D703EF3); D2 := D2 shl 13 or D2 shr 19;
  Inc(C2, D2 and A2 or not D2 and B2 + Buffer[12] + $6D703EF3); C2 := C2 shl  5 or C2 shr 27;
  Inc(B2, C2 and D2 or not C2 and A2 + Buffer[ 2] + $6D703EF3); B2 := B2 shl 14 or B2 shr 18;
  Inc(A2, B2 and C2 or not B2 and D2 + Buffer[10] + $6D703EF3); A2 := A2 shl 13 or A2 shr 19;
  Inc(D2, A2 and B2 or not A2 and C2 + Buffer[ 0] + $6D703EF3); D2 := D2 shl 13 or D2 shr 19;
  Inc(C2, D2 and A2 or not D2 and B2 + Buffer[ 4] + $6D703EF3); C2 := C2 shl  7 or C2 shr 25;
  Inc(B2, C2 and D2 or not C2 and A2 + Buffer[13] + $6D703EF3); B2 := B2 shl  5 or B2 shr 27;

  Inc(A2, B2 xor C2 xor D2 + Buffer[ 8]); A2 := A2 shl 15 or A2 shr 17;
  Inc(D2, A2 xor B2 xor C2 + Buffer[ 6]); D2 := D2 shl  5 or D2 shr 27;
  Inc(C2, D2 xor A2 xor B2 + Buffer[ 4]); C2 := C2 shl  8 or C2 shr 24;
  Inc(B2, C2 xor D2 xor A2 + Buffer[ 1]); B2 := B2 shl 11 or B2 shr 21;
  Inc(A2, B2 xor C2 xor D2 + Buffer[ 3]); A2 := A2 shl 14 or A2 shr 18;
  Inc(D2, A2 xor B2 xor C2 + Buffer[11]); D2 := D2 shl 14 or D2 shr 18;
  Inc(C2, D2 xor A2 xor B2 + Buffer[15]); C2 := C2 shl  6 or C2 shr 26;
  Inc(B2, C2 xor D2 xor A2 + Buffer[ 0]); B2 := B2 shl 14 or B2 shr 18;
  Inc(A2, B2 xor C2 xor D2 + Buffer[ 5]); A2 := A2 shl  6 or A2 shr 26;
  Inc(D2, A2 xor B2 xor C2 + Buffer[12]); D2 := D2 shl  9 or D2 shr 23;
  Inc(C2, D2 xor A2 xor B2 + Buffer[ 2]); C2 := C2 shl 12 or C2 shr 20;
  Inc(B2, C2 xor D2 xor A2 + Buffer[13]); B2 := B2 shl  9 or B2 shr 23;
  Inc(A2, B2 xor C2 xor D2 + Buffer[ 9]); A2 := A2 shl 12 or A2 shr 20;
  Inc(D2, A2 xor B2 xor C2 + Buffer[ 7]); D2 := D2 shl  5 or D2 shr 27;
  Inc(C2, D2 xor A2 xor B2 + Buffer[10]); C2 := C2 shl 15 or C2 shr 17;
  Inc(B2, C2 xor D2 xor A2 + Buffer[14]); B2 := B2 shl  8 or B2 shr 24;

  Inc(D2, C1 + FDigest[1]);
  FDigest[1] := FDigest[2] + D1 + A2;
  FDigest[2] := FDigest[3] + A1 + B2;
  FDigest[3] := FDIgest[0] + B1 + C2;
  FDigest[0] := D2;
end;

class function THash_RipeMD160.TestVector: Pointer;
asm
         MOV   EAX,OFFSET @Vector
         RET
@Vector: DB    019h,054h,0DEh,0BCh,01Bh,055h,035h,030h
         DB    008h,01Dh,09Bh,080h,070h,0A0h,0F2h,04Ah
         DB    09Dh,0F7h,034h,004h
end;

procedure THash_RipeMD160.Transform(Buffer: PIntArray);
var
  A1, B1, C1, D1, E1: LongWord;
  A, B, C, D, E: LongWord;
begin
  A := FDigest[0];
  B := FDigest[1];
  C := FDigest[2];
  D := FDigest[3];
  E := FDigest[4];

  Inc(A, Buffer[ 0] + (B xor C xor D)); A := A shl 11 or A shr 21 + E; C := C shl 10 or C shr 22;
  Inc(E, Buffer[ 1] + (A xor B xor C)); E := E shl 14 or E shr 18 + D; B := B shl 10 or B shr 22;
  Inc(D, Buffer[ 2] + (E xor A xor B)); D := D shl 15 or D shr 17 + C; A := A shl 10 or A shr 22;
  Inc(C, Buffer[ 3] + (D xor E xor A)); C := C shl 12 or C shr 20 + B; E := E shl 10 or E shr 22;
  Inc(B, Buffer[ 4] + (C xor D xor E)); B := B shl  5 or B shr 27 + A; D := D shl 10 or D shr 22;
  Inc(A, Buffer[ 5] + (B xor C xor D)); A := A shl  8 or A shr 24 + E; C := C shl 10 or C shr 22;
  Inc(E, Buffer[ 6] + (A xor B xor C)); E := E shl  7 or E shr 25 + D; B := B shl 10 or B shr 22;
  Inc(D, Buffer[ 7] + (E xor A xor B)); D := D shl  9 or D shr 23 + C; A := A shl 10 or A shr 22;
  Inc(C, Buffer[ 8] + (D xor E xor A)); C := C shl 11 or C shr 21 + B; E := E shl 10 or E shr 22;
  Inc(B, Buffer[ 9] + (C xor D xor E)); B := B shl 13 or B shr 19 + A; D := D shl 10 or D shr 22;
  Inc(A, Buffer[10] + (B xor C xor D)); A := A shl 14 or A shr 18 + E; C := C shl 10 or C shr 22;
  Inc(E, Buffer[11] + (A xor B xor C)); E := E shl 15 or E shr 17 + D; B := B shl 10 or B shr 22;
  Inc(D, Buffer[12] + (E xor A xor B)); D := D shl  6 or D shr 26 + C; A := A shl 10 or A shr 22;
  Inc(C, Buffer[13] + (D xor E xor A)); C := C shl  7 or C shr 25 + B; E := E shl 10 or E shr 22;
  Inc(B, Buffer[14] + (C xor D xor E)); B := B shl  9 or B shr 23 + A; D := D shl 10 or D shr 22;
  Inc(A, Buffer[15] + (B xor C xor D)); A := A shl  8 or A shr 24 + E; C := C shl 10 or C shr 22;

  Inc(E, Buffer[ 7] + $5A827999 + ((A and B) or (not A and C))); E := E shl  7 or E shr 25 + D; B := B shl 10 or B shr 22;
  Inc(D, Buffer[ 4] + $5A827999 + ((E and A) or (not E and B))); D := D shl  6 or D shr 26 + C; A := A shl 10 or A shr 22;
  Inc(C, Buffer[13] + $5A827999 + ((D and E) or (not D and A))); C := C shl  8 or C shr 24 + B; E := E shl 10 or E shr 22;
  Inc(B, Buffer[ 1] + $5A827999 + ((C and D) or (not C and E))); B := B shl 13 or B shr 19 + A; D := D shl 10 or D shr 22;
  Inc(A, Buffer[10] + $5A827999 + ((B and C) or (not B and D))); A := A shl 11 or A shr 21 + E; C := C shl 10 or C shr 22;
  Inc(E, Buffer[ 6] + $5A827999 + ((A and B) or (not A and C))); E := E shl  9 or E shr 23 + D; B := B shl 10 or B shr 22;
  Inc(D, Buffer[15] + $5A827999 + ((E and A) or (not E and B))); D := D shl  7 or D shr 25 + C; A := A shl 10 or A shr 22;
  Inc(C, Buffer[ 3] + $5A827999 + ((D and E) or (not D and A))); C := C shl 15 or C shr 17 + B; E := E shl 10 or E shr 22;
  Inc(B, Buffer[12] + $5A827999 + ((C and D) or (not C and E))); B := B shl  7 or B shr 25 + A; D := D shl 10 or D shr 22;
  Inc(A, Buffer[ 0] + $5A827999 + ((B and C) or (not B and D))); A := A shl 12 or A shr 20 + E; C := C shl 10 or C shr 22;
  Inc(E, Buffer[ 9] + $5A827999 + ((A and B) or (not A and C))); E := E shl 15 or E shr 17 + D; B := B shl 10 or B shr 22;
  Inc(D, Buffer[ 5] + $5A827999 + ((E and A) or (not E and B))); D := D shl  9 or D shr 23 + C; A := A shl 10 or A shr 22;
  Inc(C, Buffer[ 2] + $5A827999 + ((D and E) or (not D and A))); C := C shl 11 or C shr 21 + B; E := E shl 10 or E shr 22;
  Inc(B, Buffer[14] + $5A827999 + ((C and D) or (not C and E))); B := B shl  7 or B shr 25 + A; D := D shl 10 or D shr 22;
  Inc(A, Buffer[11] + $5A827999 + ((B and C) or (not B and D))); A := A shl 13 or A shr 19 + E; C := C shl 10 or C shr 22;
  Inc(E, Buffer[ 8] + $5A827999 + ((A and B) or (not A and C))); E := E shl 12 or E shr 20 + D; B := B shl 10 or B shr 22;

  Inc(D, Buffer[ 3] + $6ED9EBA1 + ((E or not A) xor B)); D := D shl 11 or D shr 21 + C; A := A shl 10 or A shr 22;
  Inc(C, Buffer[10] + $6ED9EBA1 + ((D or not E) xor A)); C := C shl 13 or C shr 19 + B; E := E shl 10 or E shr 22;
  Inc(B, Buffer[14] + $6ED9EBA1 + ((C or not D) xor E)); B := B shl  6 or B shr 26 + A; D := D shl 10 or D shr 22;
  Inc(A, Buffer[ 4] + $6ED9EBA1 + ((B or not C) xor D)); A := A shl  7 or A shr 25 + E; C := C shl 10 or C shr 22;
  Inc(E, Buffer[ 9] + $6ED9EBA1 + ((A or not B) xor C)); E := E shl 14 or E shr 18 + D; B := B shl 10 or B shr 22;
  Inc(D, Buffer[15] + $6ED9EBA1 + ((E or not A) xor B)); D := D shl  9 or D shr 23 + C; A := A shl 10 or A shr 22;
  Inc(C, Buffer[ 8] + $6ED9EBA1 + ((D or not E) xor A)); C := C shl 13 or C shr 19 + B; E := E shl 10 or E shr 22;
  Inc(B, Buffer[ 1] + $6ED9EBA1 + ((C or not D) xor E)); B := B shl 15 or B shr 17 + A; D := D shl 10 or D shr 22;
  Inc(A, Buffer[ 2] + $6ED9EBA1 + ((B or not C) xor D)); A := A shl 14 or A shr 18 + E; C := C shl 10 or C shr 22;
  Inc(E, Buffer[ 7] + $6ED9EBA1 + ((A or not B) xor C)); E := E shl  8 or E shr 24 + D; B := B shl 10 or B shr 22;
  Inc(D, Buffer[ 0] + $6ED9EBA1 + ((E or not A) xor B)); D := D shl 13 or D shr 19 + C; A := A shl 10 or A shr 22;
  Inc(C, Buffer[ 6] + $6ED9EBA1 + ((D or not E) xor A)); C := C shl  6 or C shr 26 + B; E := E shl 10 or E shr 22;
  Inc(B, Buffer[13] + $6ED9EBA1 + ((C or not D) xor E)); B := B shl  5 or B shr 27 + A; D := D shl 10 or D shr 22;
  Inc(A, Buffer[11] + $6ED9EBA1 + ((B or not C) xor D)); A := A shl 12 or A shr 20 + E; C := C shl 10 or C shr 22;
  Inc(E, Buffer[ 5] + $6ED9EBA1 + ((A or not B) xor C)); E := E shl  7 or E shr 25 + D; B := B shl 10 or B shr 22;
  Inc(D, Buffer[12] + $6ED9EBA1 + ((E or not A) xor B)); D := D shl  5 or D shr 27 + C; A := A shl 10 or A shr 22;

  Inc(C, Buffer[ 1] + $8F1BBCDC + ((D and A) or (E and not A))); C := C shl 11 or C shr 21 + B; E := E shl 10 or E shr 22;
  Inc(B, Buffer[ 9] + $8F1BBCDC + ((C and E) or (D and not E))); B := B shl 12 or B shr 20 + A; D := D shl 10 or D shr 22;
  Inc(A, Buffer[11] + $8F1BBCDC + ((B and D) or (C and not D))); A := A shl 14 or A shr 18 + E; C := C shl 10 or C shr 22;
  Inc(E, Buffer[10] + $8F1BBCDC + ((A and C) or (B and not C))); E := E shl 15 or E shr 17 + D; B := B shl 10 or B shr 22;
  Inc(D, Buffer[ 0] + $8F1BBCDC + ((E and B) or (A and not B))); D := D shl 14 or D shr 18 + C; A := A shl 10 or A shr 22;
  Inc(C, Buffer[ 8] + $8F1BBCDC + ((D and A) or (E and not A))); C := C shl 15 or C shr 17 + B; E := E shl 10 or E shr 22;
  Inc(B, Buffer[12] + $8F1BBCDC + ((C and E) or (D and not E))); B := B shl  9 or B shr 23 + A; D := D shl 10 or D shr 22;
  Inc(A, Buffer[ 4] + $8F1BBCDC + ((B and D) or (C and not D))); A := A shl  8 or A shr 24 + E; C := C shl 10 or C shr 22;
  Inc(E, Buffer[13] + $8F1BBCDC + ((A and C) or (B and not C))); E := E shl  9 or E shr 23 + D; B := B shl 10 or B shr 22;
  Inc(D, Buffer[ 3] + $8F1BBCDC + ((E and B) or (A and not B))); D := D shl 14 or D shr 18 + C; A := A shl 10 or A shr 22;
  Inc(C, Buffer[ 7] + $8F1BBCDC + ((D and A) or (E and not A))); C := C shl  5 or C shr 27 + B; E := E shl 10 or E shr 22;
  Inc(B, Buffer[15] + $8F1BBCDC + ((C and E) or (D and not E))); B := B shl  6 or B shr 26 + A; D := D shl 10 or D shr 22;
  Inc(A, Buffer[14] + $8F1BBCDC + ((B and D) or (C and not D))); A := A shl  8 or A shr 24 + E; C := C shl 10 or C shr 22;
  Inc(E, Buffer[ 5] + $8F1BBCDC + ((A and C) or (B and not C))); E := E shl  6 or E shr 26 + D; B := B shl 10 or B shr 22;
  Inc(D, Buffer[ 6] + $8F1BBCDC + ((E and B) or (A and not B))); D := D shl  5 or D shr 27 + C; A := A shl 10 or A shr 22;
  Inc(C, Buffer[ 2] + $8F1BBCDC + ((D and A) or (E and not A))); C := C shl 12 or C shr 20 + B; E := E shl 10 or E shr 22;

  Inc(B, Buffer[ 4] + $A953FD4E + (C xor (D or not E))); B := B shl  9 or B shr 23 + A; D := D shl 10 or D shr 22;
  Inc(A, Buffer[ 0] + $A953FD4E + (B xor (C or not D))); A := A shl 15 or A shr 17 + E; C := C shl 10 or C shr 22;
  Inc(E, Buffer[ 5] + $A953FD4E + (A xor (B or not C))); E := E shl  5 or E shr 27 + D; B := B shl 10 or B shr 22;
  Inc(D, Buffer[ 9] + $A953FD4E + (E xor (A or not B))); D := D shl 11 or D shr 21 + C; A := A shl 10 or A shr 22;
  Inc(C, Buffer[ 7] + $A953FD4E + (D xor (E or not A))); C := C shl  6 or C shr 26 + B; E := E shl 10 or E shr 22;
  Inc(B, Buffer[12] + $A953FD4E + (C xor (D or not E))); B := B shl  8 or B shr 24 + A; D := D shl 10 or D shr 22;
  Inc(A, Buffer[ 2] + $A953FD4E + (B xor (C or not D))); A := A shl 13 or A shr 19 + E; C := C shl 10 or C shr 22;
  Inc(E, Buffer[10] + $A953FD4E + (A xor (B or not C))); E := E shl 12 or E shr 20 + D; B := B shl 10 or B shr 22;
  Inc(D, Buffer[14] + $A953FD4E + (E xor (A or not B))); D := D shl  5 or D shr 27 + C; A := A shl 10 or A shr 22;
  Inc(C, Buffer[ 1] + $A953FD4E + (D xor (E or not A))); C := C shl 12 or C shr 20 + B; E := E shl 10 or E shr 22;
  Inc(B, Buffer[ 3] + $A953FD4E + (C xor (D or not E))); B := B shl 13 or B shr 19 + A; D := D shl 10 or D shr 22;
  Inc(A, Buffer[ 8] + $A953FD4E + (B xor (C or not D))); A := A shl 14 or A shr 18 + E; C := C shl 10 or C shr 22;
  Inc(E, Buffer[11] + $A953FD4E + (A xor (B or not C))); E := E shl 11 or E shr 21 + D; B := B shl 10 or B shr 22;
  Inc(D, Buffer[ 6] + $A953FD4E + (E xor (A or not B))); D := D shl  8 or D shr 24 + C; A := A shl 10 or A shr 22;
  Inc(C, Buffer[15] + $A953FD4E + (D xor (E or not A))); C := C shl  5 or C shr 27 + B; E := E shl 10 or E shr 22;
  Inc(B, Buffer[13] + $A953FD4E + (C xor (D or not E))); B := B shl  6 or B shr 26 + A; D := D shl 10 or D shr 22;

  A1 := A;
  B1 := B;
  C1 := C;
  D1 := D;
  E1 := E;

  A := FDigest[0];
  B := FDigest[1];
  C := FDigest[2];
  D := FDigest[3];
  E := FDigest[4];

  Inc(A, Buffer[ 5] + $50A28BE6 + (B xor (C or not D))); A := A shl  8 or A shr 24 + E; C := C shl 10 or C shr 22;
  Inc(E, Buffer[14] + $50A28BE6 + (A xor (B or not C))); E := E shl  9 or E shr 23 + D; B := B shl 10 or B shr 22;
  Inc(D, Buffer[ 7] + $50A28BE6 + (E xor (A or not B))); D := D shl  9 or D shr 23 + C; A := A shl 10 or A shr 22;
  Inc(C, Buffer[ 0] + $50A28BE6 + (D xor (E or not A))); C := C shl 11 or C shr 21 + B; E := E shl 10 or E shr 22;
  Inc(B, Buffer[ 9] + $50A28BE6 + (C xor (D or not E))); B := B shl 13 or B shr 19 + A; D := D shl 10 or D shr 22;
  Inc(A, Buffer[ 2] + $50A28BE6 + (B xor (C or not D))); A := A shl 15 or A shr 17 + E; C := C shl 10 or C shr 22;
  Inc(E, Buffer[11] + $50A28BE6 + (A xor (B or not C))); E := E shl 15 or E shr 17 + D; B := B shl 10 or B shr 22;
  Inc(D, Buffer[ 4] + $50A28BE6 + (E xor (A or not B))); D := D shl  5 or D shr 27 + C; A := A shl 10 or A shr 22;
  Inc(C, Buffer[13] + $50A28BE6 + (D xor (E or not A))); C := C shl  7 or C shr 25 + B; E := E shl 10 or E shr 22;
  Inc(B, Buffer[ 6] + $50A28BE6 + (C xor (D or not E))); B := B shl  7 or B shr 25 + A; D := D shl 10 or D shr 22;
  Inc(A, Buffer[15] + $50A28BE6 + (B xor (C or not D))); A := A shl  8 or A shr 24 + E; C := C shl 10 or C shr 22;
  Inc(E, Buffer[ 8] + $50A28BE6 + (A xor (B or not C))); E := E shl 11 or E shr 21 + D; B := B shl 10 or B shr 22;
  Inc(D, Buffer[ 1] + $50A28BE6 + (E xor (A or not B))); D := D shl 14 or D shr 18 + C; A := A shl 10 or A shr 22;
  Inc(C, Buffer[10] + $50A28BE6 + (D xor (E or not A))); C := C shl 14 or C shr 18 + B; E := E shl 10 or E shr 22;
  Inc(B, Buffer[ 3] + $50A28BE6 + (C xor (D or not E))); B := B shl 12 or B shr 20 + A; D := D shl 10 or D shr 22;
  Inc(A, Buffer[12] + $50A28BE6 + (B xor (C or not D))); A := A shl  6 or A shr 26 + E; C := C shl 10 or C shr 22;

  Inc(E, Buffer[ 6] + $5C4DD124 + ((A and C) or (B and not C))); E := E shl  9 or E shr 23 + D; B := B shl 10 or B shr 22;
  Inc(D, Buffer[11] + $5C4DD124 + ((E and B) or (A and not B))); D := D shl 13 or D shr 19 + C; A := A shl 10 or A shr 22;
  Inc(C, Buffer[ 3] + $5C4DD124 + ((D and A) or (E and not A))); C := C shl 15 or C shr 17 + B; E := E shl 10 or E shr 22;
  Inc(B, Buffer[ 7] + $5C4DD124 + ((C and E) or (D and not E))); B := B shl  7 or B shr 25 + A; D := D shl 10 or D shr 22;
  Inc(A, Buffer[ 0] + $5C4DD124 + ((B and D) or (C and not D))); A := A shl 12 or A shr 20 + E; C := C shl 10 or C shr 22;
  Inc(E, Buffer[13] + $5C4DD124 + ((A and C) or (B and not C))); E := E shl  8 or E shr 24 + D; B := B shl 10 or B shr 22;
  Inc(D, Buffer[ 5] + $5C4DD124 + ((E and B) or (A and not B))); D := D shl  9 or D shr 23 + C; A := A shl 10 or A shr 22;
  Inc(C, Buffer[10] + $5C4DD124 + ((D and A) or (E and not A))); C := C shl 11 or C shr 21 + B; E := E shl 10 or E shr 22;
  Inc(B, Buffer[14] + $5C4DD124 + ((C and E) or (D and not E))); B := B shl  7 or B shr 25 + A; D := D shl 10 or D shr 22;
  Inc(A, Buffer[15] + $5C4DD124 + ((B and D) or (C and not D))); A := A shl  7 or A shr 25 + E; C := C shl 10 or C shr 22;
  Inc(E, Buffer[ 8] + $5C4DD124 + ((A and C) or (B and not C))); E := E shl 12 or E shr 20 + D; B := B shl 10 or B shr 22;
  Inc(D, Buffer[12] + $5C4DD124 + ((E and B) or (A and not B))); D := D shl  7 or D shr 25 + C; A := A shl 10 or A shr 22;
  Inc(C, Buffer[ 4] + $5C4DD124 + ((D and A) or (E and not A))); C := C shl  6 or C shr 26 + B; E := E shl 10 or E shr 22;
  Inc(B, Buffer[ 9] + $5C4DD124 + ((C and E) or (D and not E))); B := B shl 15 or B shr 17 + A; D := D shl 10 or D shr 22;
  Inc(A, Buffer[ 1] + $5C4DD124 + ((B and D) or (C and not D))); A := A shl 13 or A shr 19 + E; C := C shl 10 or C shr 22;
  Inc(E, Buffer[ 2] + $5C4DD124 + ((A and C) or (B and not C))); E := E shl 11 or E shr 21 + D; B := B shl 10 or B shr 22;

  Inc(D, Buffer[15] + $6D703EF3 + ((E or not A) xor B)); D := D shl  9 or D shr 23 + C; A := A shl 10 or A shr 22;
  Inc(C, Buffer[ 5] + $6D703EF3 + ((D or not E) xor A)); C := C shl  7 or C shr 25 + B; E := E shl 10 or E shr 22;
  Inc(B, Buffer[ 1] + $6D703EF3 + ((C or not D) xor E)); B := B shl 15 or B shr 17 + A; D := D shl 10 or D shr 22;
  Inc(A, Buffer[ 3] + $6D703EF3 + ((B or not C) xor D)); A := A shl 11 or A shr 21 + E; C := C shl 10 or C shr 22;
  Inc(E, Buffer[ 7] + $6D703EF3 + ((A or not B) xor C)); E := E shl  8 or E shr 24 + D; B := B shl 10 or B shr 22;
  Inc(D, Buffer[14] + $6D703EF3 + ((E or not A) xor B)); D := D shl  6 or D shr 26 + C; A := A shl 10 or A shr 22;
  Inc(C, Buffer[ 6] + $6D703EF3 + ((D or not E) xor A)); C := C shl  6 or C shr 26 + B; E := E shl 10 or E shr 22;
  Inc(B, Buffer[ 9] + $6D703EF3 + ((C or not D) xor E)); B := B shl 14 or B shr 18 + A; D := D shl 10 or D shr 22;
  Inc(A, Buffer[11] + $6D703EF3 + ((B or not C) xor D)); A := A shl 12 or A shr 20 + E; C := C shl 10 or C shr 22;
  Inc(E, Buffer[ 8] + $6D703EF3 + ((A or not B) xor C)); E := E shl 13 or E shr 19 + D; B := B shl 10 or B shr 22;
  Inc(D, Buffer[12] + $6D703EF3 + ((E or not A) xor B)); D := D shl  5 or D shr 27 + C; A := A shl 10 or A shr 22;
  Inc(C, Buffer[ 2] + $6D703EF3 + ((D or not E) xor A)); C := C shl 14 or C shr 18 + B; E := E shl 10 or E shr 22;
  Inc(B, Buffer[10] + $6D703EF3 + ((C or not D) xor E)); B := B shl 13 or B shr 19 + A; D := D shl 10 or D shr 22;
  Inc(A, Buffer[ 0] + $6D703EF3 + ((B or not C) xor D)); A := A shl 13 or A shr 19 + E; C := C shl 10 or C shr 22;
  Inc(E, Buffer[ 4] + $6D703EF3 + ((A or not B) xor C)); E := E shl  7 or E shr 25 + D; B := B shl 10 or B shr 22;
  Inc(D, Buffer[13] + $6D703EF3 + ((E or not A) xor B)); D := D shl  5 or D shr 27 + C; A := A shl 10 or A shr 22;

  Inc(C, Buffer[ 8] + $7A6D76E9 + ((D and E) or (not D and A))); C := C shl 15 or C shr 17 + B; E := E shl 10 or E shr 22;
  Inc(B, Buffer[ 6] + $7A6D76E9 + ((C and D) or (not C and E))); B := B shl  5 or B shr 27 + A; D := D shl 10 or D shr 22;
  Inc(A, Buffer[ 4] + $7A6D76E9 + ((B and C) or (not B and D))); A := A shl  8 or A shr 24 + E; C := C shl 10 or C shr 22;
  Inc(E, Buffer[ 1] + $7A6D76E9 + ((A and B) or (not A and C))); E := E shl 11 or E shr 21 + D; B := B shl 10 or B shr 22;
  Inc(D, Buffer[ 3] + $7A6D76E9 + ((E and A) or (not E and B))); D := D shl 14 or D shr 18 + C; A := A shl 10 or A shr 22;
  Inc(C, Buffer[11] + $7A6D76E9 + ((D and E) or (not D and A))); C := C shl 14 or C shr 18 + B; E := E shl 10 or E shr 22;
  Inc(B, Buffer[15] + $7A6D76E9 + ((C and D) or (not C and E))); B := B shl  6 or B shr 26 + A; D := D shl 10 or D shr 22;
  Inc(A, Buffer[ 0] + $7A6D76E9 + ((B and C) or (not B and D))); A := A shl 14 or A shr 18 + E; C := C shl 10 or C shr 22;
  Inc(E, Buffer[ 5] + $7A6D76E9 + ((A and B) or (not A and C))); E := E shl  6 or E shr 26 + D; B := B shl 10 or B shr 22;
  Inc(D, Buffer[12] + $7A6D76E9 + ((E and A) or (not E and B))); D := D shl  9 or D shr 23 + C; A := A shl 10 or A shr 22;
  Inc(C, Buffer[ 2] + $7A6D76E9 + ((D and E) or (not D and A))); C := C shl 12 or C shr 20 + B; E := E shl 10 or E shr 22;
  Inc(B, Buffer[13] + $7A6D76E9 + ((C and D) or (not C and E))); B := B shl  9 or B shr 23 + A; D := D shl 10 or D shr 22;
  Inc(A, Buffer[ 9] + $7A6D76E9 + ((B and C) or (not B and D))); A := A shl 12 or A shr 20 + E; C := C shl 10 or C shr 22;
  Inc(E, Buffer[ 7] + $7A6D76E9 + ((A and B) or (not A and C))); E := E shl  5 or E shr 27 + D; B := B shl 10 or B shr 22;
  Inc(D, Buffer[10] + $7A6D76E9 + ((E and A) or (not E and B))); D := D shl 15 or D shr 17 + C; A := A shl 10 or A shr 22;
  Inc(C, Buffer[14] + $7A6D76E9 + ((D and E) or (not D and A))); C := C shl  8 or C shr 24 + B; E := E shl 10 or E shr 22;

  Inc(B, Buffer[12] + (C xor D xor E)); B := B shl  8 or B shr 24 + A; D := D shl 10 or D shr 22;
  Inc(A, Buffer[15] + (B xor C xor D)); A := A shl  5 or A shr 27 + E; C := C shl 10 or C shr 22;
  Inc(E, Buffer[10] + (A xor B xor C)); E := E shl 12 or E shr 20 + D; B := B shl 10 or B shr 22;
  Inc(D, Buffer[ 4] + (E xor A xor B)); D := D shl  9 or D shr 23 + C; A := A shl 10 or A shr 22;
  Inc(C, Buffer[ 1] + (D xor E xor A)); C := C shl 12 or C shr 20 + B; E := E shl 10 or E shr 22;
  Inc(B, Buffer[ 5] + (C xor D xor E)); B := B shl  5 or B shr 27 + A; D := D shl 10 or D shr 22;
  Inc(A, Buffer[ 8] + (B xor C xor D)); A := A shl 14 or A shr 18 + E; C := C shl 10 or C shr 22;
  Inc(E, Buffer[ 7] + (A xor B xor C)); E := E shl  6 or E shr 26 + D; B := B shl 10 or B shr 22;
  Inc(D, Buffer[ 6] + (E xor A xor B)); D := D shl  8 or D shr 24 + C; A := A shl 10 or A shr 22;
  Inc(C, Buffer[ 2] + (D xor E xor A)); C := C shl 13 or C shr 19 + B; E := E shl 10 or E shr 22;
  Inc(B, Buffer[13] + (C xor D xor E)); B := B shl  6 or B shr 26 + A; D := D shl 10 or D shr 22;
  Inc(A, Buffer[14] + (B xor C xor D)); A := A shl  5 or A shr 27 + E; C := C shl 10 or C shr 22;
  Inc(E, Buffer[ 0] + (A xor B xor C)); E := E shl 15 or E shr 17 + D; B := B shl 10 or B shr 22;
  Inc(D, Buffer[ 3] + (E xor A xor B)); D := D shl 13 or D shr 19 + C; A := A shl 10 or A shr 22;
  Inc(C, Buffer[ 9] + (D xor E xor A)); C := C shl 11 or C shr 21 + B; E := E shl 10 or E shr 22;
  Inc(B, Buffer[11] + (C xor D xor E)); B := B shl 11 or B shr 21 + A; D := D shl 10 or D shr 22;

  Inc(D, C1 + FDigest[1]);
  FDigest[1] := FDigest[2] + D1 + E;
  FDigest[2] := FDigest[3] + E1 + A;
  FDigest[3] := FDigest[4] + A1 + B;
  FDigest[4] := FDigest[0] + B1 + C;
  FDigest[0] := D;
end;

class function THash_RipeMD160.DigestKeySize: Integer;
begin
  Result := 20;
end;

class function THash_RipeMD256.TestVector: Pointer;
asm
         MOV   EAX,OFFSET @Vector
         RET
@Vector: DB    0C3h,0B1h,0D7h,0ACh,0A8h,09Ah,047h,07Ah
         DB    038h,0D3h,06Dh,039h,0EFh,000h,0FBh,045h
         DB    0FCh,04Eh,0C3h,01Ah,071h,021h,0DBh,09Eh
         DB    01Ch,076h,0C5h,0DEh,099h,088h,018h,0C2h
end;

procedure THash_RipeMD256.Transform(Buffer: PIntArray);
var
  A1, B1, C1, D1: LongWord;
  A2, B2, C2, D2: LongWord;
  T: LongWord;
begin
  A1 := FDigest[0];
  B1 := FDigest[1];
  C1 := FDigest[2];
  D1 := FDigest[3];
  A2 := FDigest[4];
  B2 := FDigest[5];
  C2 := FDigest[6];
  D2 := FDigest[7];

  Inc(A1, B1 xor C1 xor D1 + Buffer[ 0]); A1 := A1 shl 11 or A1 shr 21;
  Inc(D1, A1 xor B1 xor C1 + Buffer[ 1]); D1 := D1 shl 14 or D1 shr 18;
  Inc(C1, D1 xor A1 xor B1 + Buffer[ 2]); C1 := C1 shl 15 or C1 shr 17;
  Inc(B1, C1 xor D1 xor A1 + Buffer[ 3]); B1 := B1 shl 12 or B1 shr 20;
  Inc(A1, B1 xor C1 xor D1 + Buffer[ 4]); A1 := A1 shl  5 or A1 shr 27;
  Inc(D1, A1 xor B1 xor C1 + Buffer[ 5]); D1 := D1 shl  8 or D1 shr 24;
  Inc(C1, D1 xor A1 xor B1 + Buffer[ 6]); C1 := C1 shl  7 or C1 shr 25;
  Inc(B1, C1 xor D1 xor A1 + Buffer[ 7]); B1 := B1 shl  9 or B1 shr 23;
  Inc(A1, B1 xor C1 xor D1 + Buffer[ 8]); A1 := A1 shl 11 or A1 shr 21;
  Inc(D1, A1 xor B1 xor C1 + Buffer[ 9]); D1 := D1 shl 13 or D1 shr 19;
  Inc(C1, D1 xor A1 xor B1 + Buffer[10]); C1 := C1 shl 14 or C1 shr 18;
  Inc(B1, C1 xor D1 xor A1 + Buffer[11]); B1 := B1 shl 15 or B1 shr 17;
  Inc(A1, B1 xor C1 xor D1 + Buffer[12]); A1 := A1 shl  6 or A1 shr 26;
  Inc(D1, A1 xor B1 xor C1 + Buffer[13]); D1 := D1 shl  7 or D1 shr 25;
  Inc(C1, D1 xor A1 xor B1 + Buffer[14]); C1 := C1 shl  9 or C1 shr 23;
  Inc(B1, C1 xor D1 xor A1 + Buffer[15]); B1 := B1 shl  8 or B1 shr 24;

  Inc(A2, B2 and D2 or C2 and not D2 + Buffer[ 5] + $50A28BE6); A2 := A2 shl  8 or A2 shr 24;
  Inc(D2, A2 and C2 or B2 and not C2 + Buffer[14] + $50A28BE6); D2 := D2 shl  9 or D2 shr 23;
  Inc(C2, D2 and B2 or A2 and not B2 + Buffer[ 7] + $50A28BE6); C2 := C2 shl  9 or C2 shr 23;
  Inc(B2, C2 and A2 or D2 and not A2 + Buffer[ 0] + $50A28BE6); B2 := B2 shl 11 or B2 shr 21;
  Inc(A2, B2 and D2 or C2 and not D2 + Buffer[ 9] + $50A28BE6); A2 := A2 shl 13 or A2 shr 19;
  Inc(D2, A2 and C2 or B2 and not C2 + Buffer[ 2] + $50A28BE6); D2 := D2 shl 15 or D2 shr 17;
  Inc(C2, D2 and B2 or A2 and not B2 + Buffer[11] + $50A28BE6); C2 := C2 shl 15 or C2 shr 17;
  Inc(B2, C2 and A2 or D2 and not A2 + Buffer[ 4] + $50A28BE6); B2 := B2 shl  5 or B2 shr 27;
  Inc(A2, B2 and D2 or C2 and not D2 + Buffer[13] + $50A28BE6); A2 := A2 shl  7 or A2 shr 25;
  Inc(D2, A2 and C2 or B2 and not C2 + Buffer[ 6] + $50A28BE6); D2 := D2 shl  7 or D2 shr 25;
  Inc(C2, D2 and B2 or A2 and not B2 + Buffer[15] + $50A28BE6); C2 := C2 shl  8 or C2 shr 24;
  Inc(B2, C2 and A2 or D2 and not A2 + Buffer[ 8] + $50A28BE6); B2 := B2 shl 11 or B2 shr 21;
  Inc(A2, B2 and D2 or C2 and not D2 + Buffer[ 1] + $50A28BE6); A2 := A2 shl 14 or A2 shr 18;
  Inc(D2, A2 and C2 or B2 and not C2 + Buffer[10] + $50A28BE6); D2 := D2 shl 14 or D2 shr 18;
  Inc(C2, D2 and B2 or A2 and not B2 + Buffer[ 3] + $50A28BE6); C2 := C2 shl 12 or C2 shr 20;
  Inc(B2, C2 and A2 or D2 and not A2 + Buffer[12] + $50A28BE6); B2 := B2 shl  6 or B2 shr 26;

  T := A1; A1 := A2; A2 := T;

  Inc(A1, B1 and C1 or not B1 and D1 + Buffer[ 7] + $5A827999); A1 := A1 shl  7 or A1 shr 25;
  Inc(D1, A1 and B1 or not A1 and C1 + Buffer[ 4] + $5A827999); D1 := D1 shl  6 or D1 shr 26;
  Inc(C1, D1 and A1 or not D1 and B1 + Buffer[13] + $5A827999); C1 := C1 shl  8 or C1 shr 24;
  Inc(B1, C1 and D1 or not C1 and A1 + Buffer[ 1] + $5A827999); B1 := B1 shl 13 or B1 shr 19;
  Inc(A1, B1 and C1 or not B1 and D1 + Buffer[10] + $5A827999); A1 := A1 shl 11 or A1 shr 21;
  Inc(D1, A1 and B1 or not A1 and C1 + Buffer[ 6] + $5A827999); D1 := D1 shl  9 or D1 shr 23;
  Inc(C1, D1 and A1 or not D1 and B1 + Buffer[15] + $5A827999); C1 := C1 shl  7 or C1 shr 25;
  Inc(B1, C1 and D1 or not C1 and A1 + Buffer[ 3] + $5A827999); B1 := B1 shl 15 or B1 shr 17;
  Inc(A1, B1 and C1 or not B1 and D1 + Buffer[12] + $5A827999); A1 := A1 shl  7 or A1 shr 25;
  Inc(D1, A1 and B1 or not A1 and C1 + Buffer[ 0] + $5A827999); D1 := D1 shl 12 or D1 shr 20;
  Inc(C1, D1 and A1 or not D1 and B1 + Buffer[ 9] + $5A827999); C1 := C1 shl 15 or C1 shr 17;
  Inc(B1, C1 and D1 or not C1 and A1 + Buffer[ 5] + $5A827999); B1 := B1 shl  9 or B1 shr 23;
  Inc(A1, B1 and C1 or not B1 and D1 + Buffer[ 2] + $5A827999); A1 := A1 shl 11 or A1 shr 21;
  Inc(D1, A1 and B1 or not A1 and C1 + Buffer[14] + $5A827999); D1 := D1 shl  7 or D1 shr 25;
  Inc(C1, D1 and A1 or not D1 and B1 + Buffer[11] + $5A827999); C1 := C1 shl 13 or C1 shr 19;
  Inc(B1, C1 and D1 or not C1 and A1 + Buffer[ 8] + $5A827999); B1 := B1 shl 12 or B1 shr 20;

  Inc(A2, (B2 or not C2) xor D2 + Buffer[ 6] + $5C4DD124); A2 := A2 shl  9 or A2 shr 23;
  Inc(D2, (A2 or not B2) xor C2 + Buffer[11] + $5C4DD124); D2 := D2 shl 13 or D2 shr 19;
  Inc(C2, (D2 or not A2) xor B2 + Buffer[ 3] + $5C4DD124); C2 := C2 shl 15 or C2 shr 17;
  Inc(B2, (C2 or not D2) xor A2 + Buffer[ 7] + $5C4DD124); B2 := B2 shl  7 or B2 shr 25;
  Inc(A2, (B2 or not C2) xor D2 + Buffer[ 0] + $5C4DD124); A2 := A2 shl 12 or A2 shr 20;
  Inc(D2, (A2 or not B2) xor C2 + Buffer[13] + $5C4DD124); D2 := D2 shl  8 or D2 shr 24;
  Inc(C2, (D2 or not A2) xor B2 + Buffer[ 5] + $5C4DD124); C2 := C2 shl  9 or C2 shr 23;
  Inc(B2, (C2 or not D2) xor A2 + Buffer[10] + $5C4DD124); B2 := B2 shl 11 or B2 shr 21;
  Inc(A2, (B2 or not C2) xor D2 + Buffer[14] + $5C4DD124); A2 := A2 shl  7 or A2 shr 25;
  Inc(D2, (A2 or not B2) xor C2 + Buffer[15] + $5C4DD124); D2 := D2 shl  7 or D2 shr 25;
  Inc(C2, (D2 or not A2) xor B2 + Buffer[ 8] + $5C4DD124); C2 := C2 shl 12 or C2 shr 20;
  Inc(B2, (C2 or not D2) xor A2 + Buffer[12] + $5C4DD124); B2 := B2 shl  7 or B2 shr 25;
  Inc(A2, (B2 or not C2) xor D2 + Buffer[ 4] + $5C4DD124); A2 := A2 shl  6 or A2 shr 26;
  Inc(D2, (A2 or not B2) xor C2 + Buffer[ 9] + $5C4DD124); D2 := D2 shl 15 or D2 shr 17;
  Inc(C2, (D2 or not A2) xor B2 + Buffer[ 1] + $5C4DD124); C2 := C2 shl 13 or C2 shr 19;
  Inc(B2, (C2 or not D2) xor A2 + Buffer[ 2] + $5C4DD124); B2 := B2 shl 11 or B2 shr 21;

  T := B1; B1 := B2; B2 := T;

  Inc(A1, (B1 or not C1) xor D1 + Buffer[ 3] + $6ED9EBA1); A1 := A1 shl 11 or A1 shr 21;
  Inc(D1, (A1 or not B1) xor C1 + Buffer[10] + $6ED9EBA1); D1 := D1 shl 13 or D1 shr 19;
  Inc(C1, (D1 or not A1) xor B1 + Buffer[14] + $6ED9EBA1); C1 := C1 shl  6 or C1 shr 26;
  Inc(B1, (C1 or not D1) xor A1 + Buffer[ 4] + $6ED9EBA1); B1 := B1 shl  7 or B1 shr 25;
  Inc(A1, (B1 or not C1) xor D1 + Buffer[ 9] + $6ED9EBA1); A1 := A1 shl 14 or A1 shr 18;
  Inc(D1, (A1 or not B1) xor C1 + Buffer[15] + $6ED9EBA1); D1 := D1 shl  9 or D1 shr 23;
  Inc(C1, (D1 or not A1) xor B1 + Buffer[ 8] + $6ED9EBA1); C1 := C1 shl 13 or C1 shr 19;
  Inc(B1, (C1 or not D1) xor A1 + Buffer[ 1] + $6ED9EBA1); B1 := B1 shl 15 or B1 shr 17;
  Inc(A1, (B1 or not C1) xor D1 + Buffer[ 2] + $6ED9EBA1); A1 := A1 shl 14 or A1 shr 18;
  Inc(D1, (A1 or not B1) xor C1 + Buffer[ 7] + $6ED9EBA1); D1 := D1 shl  8 or D1 shr 24;
  Inc(C1, (D1 or not A1) xor B1 + Buffer[ 0] + $6ED9EBA1); C1 := C1 shl 13 or C1 shr 19;
  Inc(B1, (C1 or not D1) xor A1 + Buffer[ 6] + $6ED9EBA1); B1 := B1 shl  6 or B1 shr 26;
  Inc(A1, (B1 or not C1) xor D1 + Buffer[13] + $6ED9EBA1); A1 := A1 shl  5 or A1 shr 27;
  Inc(D1, (A1 or not B1) xor C1 + Buffer[11] + $6ED9EBA1); D1 := D1 shl 12 or D1 shr 20;
  Inc(C1, (D1 or not A1) xor B1 + Buffer[ 5] + $6ED9EBA1); C1 := C1 shl  7 or C1 shr 25;
  Inc(B1, (C1 or not D1) xor A1 + Buffer[12] + $6ED9EBA1); B1 := B1 shl  5 or B1 shr 27;

  Inc(A2, B2 and C2 or not B2 and D2 + Buffer[15] + $6D703EF3); A2 := A2 shl  9 or A2 shr 23;
  Inc(D2, A2 and B2 or not A2 and C2 + Buffer[ 5] + $6D703EF3); D2 := D2 shl  7 or D2 shr 25;
  Inc(C2, D2 and A2 or not D2 and B2 + Buffer[ 1] + $6D703EF3); C2 := C2 shl 15 or C2 shr 17;
  Inc(B2, C2 and D2 or not C2 and A2 + Buffer[ 3] + $6D703EF3); B2 := B2 shl 11 or B2 shr 21;
  Inc(A2, B2 and C2 or not B2 and D2 + Buffer[ 7] + $6D703EF3); A2 := A2 shl  8 or A2 shr 24;
  Inc(D2, A2 and B2 or not A2 and C2 + Buffer[14] + $6D703EF3); D2 := D2 shl  6 or D2 shr 26;
  Inc(C2, D2 and A2 or not D2 and B2 + Buffer[ 6] + $6D703EF3); C2 := C2 shl  6 or C2 shr 26;
  Inc(B2, C2 and D2 or not C2 and A2 + Buffer[ 9] + $6D703EF3); B2 := B2 shl 14 or B2 shr 18;
  Inc(A2, B2 and C2 or not B2 and D2 + Buffer[11] + $6D703EF3); A2 := A2 shl 12 or A2 shr 20;
  Inc(D2, A2 and B2 or not A2 and C2 + Buffer[ 8] + $6D703EF3); D2 := D2 shl 13 or D2 shr 19;
  Inc(C2, D2 and A2 or not D2 and B2 + Buffer[12] + $6D703EF3); C2 := C2 shl  5 or C2 shr 27;
  Inc(B2, C2 and D2 or not C2 and A2 + Buffer[ 2] + $6D703EF3); B2 := B2 shl 14 or B2 shr 18;
  Inc(A2, B2 and C2 or not B2 and D2 + Buffer[10] + $6D703EF3); A2 := A2 shl 13 or A2 shr 19;
  Inc(D2, A2 and B2 or not A2 and C2 + Buffer[ 0] + $6D703EF3); D2 := D2 shl 13 or D2 shr 19;
  Inc(C2, D2 and A2 or not D2 and B2 + Buffer[ 4] + $6D703EF3); C2 := C2 shl  7 or C2 shr 25;
  Inc(B2, C2 and D2 or not C2 and A2 + Buffer[13] + $6D703EF3); B2 := B2 shl  5 or B2 shr 27;

  T := C1; C1 := C2; C2 := T;

  Inc(A1, B1 and D1 or C1 and not D1 + Buffer[ 1] + $8F1BBCDC); A1 := A1 shl 11 or A1 shr 21;
  Inc(D1, A1 and C1 or B1 and not C1 + Buffer[ 9] + $8F1BBCDC); D1 := D1 shl 12 or D1 shr 20;
  Inc(C1, D1 and B1 or A1 and not B1 + Buffer[11] + $8F1BBCDC); C1 := C1 shl 14 or C1 shr 18;
  Inc(B1, C1 and A1 or D1 and not A1 + Buffer[10] + $8F1BBCDC); B1 := B1 shl 15 or B1 shr 17;
  Inc(A1, B1 and D1 or C1 and not D1 + Buffer[ 0] + $8F1BBCDC); A1 := A1 shl 14 or A1 shr 18;
  Inc(D1, A1 and C1 or B1 and not C1 + Buffer[ 8] + $8F1BBCDC); D1 := D1 shl 15 or D1 shr 17;
  Inc(C1, D1 and B1 or A1 and not B1 + Buffer[12] + $8F1BBCDC); C1 := C1 shl  9 or C1 shr 23;
  Inc(B1, C1 and A1 or D1 and not A1 + Buffer[ 4] + $8F1BBCDC); B1 := B1 shl  8 or B1 shr 24;
  Inc(A1, B1 and D1 or C1 and not D1 + Buffer[13] + $8F1BBCDC); A1 := A1 shl  9 or A1 shr 23;
  Inc(D1, A1 and C1 or B1 and not C1 + Buffer[ 3] + $8F1BBCDC); D1 := D1 shl 14 or D1 shr 18;
  Inc(C1, D1 and B1 or A1 and not B1 + Buffer[ 7] + $8F1BBCDC); C1 := C1 shl  5 or C1 shr 27;
  Inc(B1, C1 and A1 or D1 and not A1 + Buffer[15] + $8F1BBCDC); B1 := B1 shl  6 or B1 shr 26;
  Inc(A1, B1 and D1 or C1 and not D1 + Buffer[14] + $8F1BBCDC); A1 := A1 shl  8 or A1 shr 24;
  Inc(D1, A1 and C1 or B1 and not C1 + Buffer[ 5] + $8F1BBCDC); D1 := D1 shl  6 or D1 shr 26;
  Inc(C1, D1 and B1 or A1 and not B1 + Buffer[ 6] + $8F1BBCDC); C1 := C1 shl  5 or C1 shr 27;
  Inc(B1, C1 and A1 or D1 and not A1 + Buffer[ 2] + $8F1BBCDC); B1 := B1 shl 12 or B1 shr 20;

  Inc(A2, B2 xor C2 xor D2 + Buffer[ 8]); A2 := A2 shl 15 or A2 shr 17;
  Inc(D2, A2 xor B2 xor C2 + Buffer[ 6]); D2 := D2 shl  5 or D2 shr 27;
  Inc(C2, D2 xor A2 xor B2 + Buffer[ 4]); C2 := C2 shl  8 or C2 shr 24;
  Inc(B2, C2 xor D2 xor A2 + Buffer[ 1]); B2 := B2 shl 11 or B2 shr 21;
  Inc(A2, B2 xor C2 xor D2 + Buffer[ 3]); A2 := A2 shl 14 or A2 shr 18;
  Inc(D2, A2 xor B2 xor C2 + Buffer[11]); D2 := D2 shl 14 or D2 shr 18;
  Inc(C2, D2 xor A2 xor B2 + Buffer[15]); C2 := C2 shl  6 or C2 shr 26;
  Inc(B2, C2 xor D2 xor A2 + Buffer[ 0]); B2 := B2 shl 14 or B2 shr 18;
  Inc(A2, B2 xor C2 xor D2 + Buffer[ 5]); A2 := A2 shl  6 or A2 shr 26;
  Inc(D2, A2 xor B2 xor C2 + Buffer[12]); D2 := D2 shl  9 or D2 shr 23;
  Inc(C2, D2 xor A2 xor B2 + Buffer[ 2]); C2 := C2 shl 12 or C2 shr 20;
  Inc(B2, C2 xor D2 xor A2 + Buffer[13]); B2 := B2 shl  9 or B2 shr 23;
  Inc(A2, B2 xor C2 xor D2 + Buffer[ 9]); A2 := A2 shl 12 or A2 shr 20;
  Inc(D2, A2 xor B2 xor C2 + Buffer[ 7]); D2 := D2 shl  5 or D2 shr 27;
  Inc(C2, D2 xor A2 xor B2 + Buffer[10]); C2 := C2 shl 15 or C2 shr 17;
  Inc(B2, C2 xor D2 xor A2 + Buffer[14]); B2 := B2 shl  8 or B2 shr 24;

  T := D1; D1 := D2; D2 := T;

  Inc(FDigest[0], A1);
  Inc(FDigest[1], B1);
  Inc(FDigest[2], C1);
  Inc(FDigest[3], D1);
  Inc(FDigest[4], A2);
  Inc(FDigest[5], B2);
  Inc(FDigest[6], C2);
  Inc(FDigest[7], D2);
end;

class function THash_RipeMD256.DigestKeySize: Integer;
begin
  Result := 32;
end;

procedure THash_RipeMD256.Init;
begin
  inherited Init;
  FDigest[4] := $76543210;
  FDigest[5] := $FEDCBA98;
  FDigest[6] := $89ABCDEF;
  FDigest[7] := $01234567;
end;

class function THash_RipeMD320.TestVector: Pointer;
asm
         MOV   EAX,OFFSET @Vector
         RET
@Vector: DB    0B7h,0BDh,02Ch,075h,0B7h,013h,050h,091h
         DB    0E4h,067h,009h,046h,0F1h,041h,05Ah,048h
         DB    045h,0DFh,08Eh,007h,0BAh,067h,04Eh,0A9h
         DB    0FDh,066h,0EDh,001h,0D9h,06Fh,023h,020h
         DB    0B5h,011h,012h,0C5h,0A7h,041h,0A6h,05Ch
end;

procedure THash_RipeMD320.Transform(Buffer: PIntArray);
var
  A1, B1, C1, D1, E1: LongWord;
  A2, B2, C2, D2, E2: LongWord;
  T: LongWord;
begin
  A1 := FDigest[0];
  B1 := FDigest[1];
  C1 := FDigest[2];
  D1 := FDigest[3];
  E1 := FDigest[4];
  A2 := FDigest[5];
  B2 := FDigest[6];
  C2 := FDigest[7];
  D2 := FDigest[8];
  E2 := FDigest[9];

  Inc(A1, Buffer[ 0] + (B1 xor C1 xor D1)); A1 := A1 shl 11 or A1 shr 21 + E1; C1 := C1 shl 10 or C1 shr 22;
  Inc(E1, Buffer[ 1] + (A1 xor B1 xor C1)); E1 := E1 shl 14 or E1 shr 18 + D1; B1 := B1 shl 10 or B1 shr 22;
  Inc(D1, Buffer[ 2] + (E1 xor A1 xor B1)); D1 := D1 shl 15 or D1 shr 17 + C1; A1 := A1 shl 10 or A1 shr 22;
  Inc(C1, Buffer[ 3] + (D1 xor E1 xor A1)); C1 := C1 shl 12 or C1 shr 20 + B1; E1 := E1 shl 10 or E1 shr 22;
  Inc(B1, Buffer[ 4] + (C1 xor D1 xor E1)); B1 := B1 shl  5 or B1 shr 27 + A1; D1 := D1 shl 10 or D1 shr 22;
  Inc(A1, Buffer[ 5] + (B1 xor C1 xor D1)); A1 := A1 shl  8 or A1 shr 24 + E1; C1 := C1 shl 10 or C1 shr 22;
  Inc(E1, Buffer[ 6] + (A1 xor B1 xor C1)); E1 := E1 shl  7 or E1 shr 25 + D1; B1 := B1 shl 10 or B1 shr 22;
  Inc(D1, Buffer[ 7] + (E1 xor A1 xor B1)); D1 := D1 shl  9 or D1 shr 23 + C1; A1 := A1 shl 10 or A1 shr 22;
  Inc(C1, Buffer[ 8] + (D1 xor E1 xor A1)); C1 := C1 shl 11 or C1 shr 21 + B1; E1 := E1 shl 10 or E1 shr 22;
  Inc(B1, Buffer[ 9] + (C1 xor D1 xor E1)); B1 := B1 shl 13 or B1 shr 19 + A1; D1 := D1 shl 10 or D1 shr 22;
  Inc(A1, Buffer[10] + (B1 xor C1 xor D1)); A1 := A1 shl 14 or A1 shr 18 + E1; C1 := C1 shl 10 or C1 shr 22;
  Inc(E1, Buffer[11] + (A1 xor B1 xor C1)); E1 := E1 shl 15 or E1 shr 17 + D1; B1 := B1 shl 10 or B1 shr 22;
  Inc(D1, Buffer[12] + (E1 xor A1 xor B1)); D1 := D1 shl  6 or D1 shr 26 + C1; A1 := A1 shl 10 or A1 shr 22;
  Inc(C1, Buffer[13] + (D1 xor E1 xor A1)); C1 := C1 shl  7 or C1 shr 25 + B1; E1 := E1 shl 10 or E1 shr 22;
  Inc(B1, Buffer[14] + (C1 xor D1 xor E1)); B1 := B1 shl  9 or B1 shr 23 + A1; D1 := D1 shl 10 or D1 shr 22;
  Inc(A1, Buffer[15] + (B1 xor C1 xor D1)); A1 := A1 shl  8 or A1 shr 24 + E1; C1 := C1 shl 10 or C1 shr 22;

  Inc(A2, Buffer[ 5] + $50A28BE6 + (B2 xor (C2 or not D2))); A2 := A2 shl  8 or A2 shr 24 + E2; C2 := C2 shl 10 or C2 shr 22;
  Inc(E2, Buffer[14] + $50A28BE6 + (A2 xor (B2 or not C2))); E2 := E2 shl  9 or E2 shr 23 + D2; B2 := B2 shl 10 or B2 shr 22;
  Inc(D2, Buffer[ 7] + $50A28BE6 + (E2 xor (A2 or not B2))); D2 := D2 shl  9 or D2 shr 23 + C2; A2 := A2 shl 10 or A2 shr 22;
  Inc(C2, Buffer[ 0] + $50A28BE6 + (D2 xor (E2 or not A2))); C2 := C2 shl 11 or C2 shr 21 + B2; E2 := E2 shl 10 or E2 shr 22;
  Inc(B2, Buffer[ 9] + $50A28BE6 + (C2 xor (D2 or not E2))); B2 := B2 shl 13 or B2 shr 19 + A2; D2 := D2 shl 10 or D2 shr 22;
  Inc(A2, Buffer[ 2] + $50A28BE6 + (B2 xor (C2 or not D2))); A2 := A2 shl 15 or A2 shr 17 + E2; C2 := C2 shl 10 or C2 shr 22;
  Inc(E2, Buffer[11] + $50A28BE6 + (A2 xor (B2 or not C2))); E2 := E2 shl 15 or E2 shr 17 + D2; B2 := B2 shl 10 or B2 shr 22;
  Inc(D2, Buffer[ 4] + $50A28BE6 + (E2 xor (A2 or not B2))); D2 := D2 shl  5 or D2 shr 27 + C2; A2 := A2 shl 10 or A2 shr 22;
  Inc(C2, Buffer[13] + $50A28BE6 + (D2 xor (E2 or not A2))); C2 := C2 shl  7 or C2 shr 25 + B2; E2 := E2 shl 10 or E2 shr 22;
  Inc(B2, Buffer[ 6] + $50A28BE6 + (C2 xor (D2 or not E2))); B2 := B2 shl  7 or B2 shr 25 + A2; D2 := D2 shl 10 or D2 shr 22;
  Inc(A2, Buffer[15] + $50A28BE6 + (B2 xor (C2 or not D2))); A2 := A2 shl  8 or A2 shr 24 + E2; C2 := C2 shl 10 or C2 shr 22;
  Inc(E2, Buffer[ 8] + $50A28BE6 + (A2 xor (B2 or not C2))); E2 := E2 shl 11 or E2 shr 21 + D2; B2 := B2 shl 10 or B2 shr 22;
  Inc(D2, Buffer[ 1] + $50A28BE6 + (E2 xor (A2 or not B2))); D2 := D2 shl 14 or D2 shr 18 + C2; A2 := A2 shl 10 or A2 shr 22;
  Inc(C2, Buffer[10] + $50A28BE6 + (D2 xor (E2 or not A2))); C2 := C2 shl 14 or C2 shr 18 + B2; E2 := E2 shl 10 or E2 shr 22;
  Inc(B2, Buffer[ 3] + $50A28BE6 + (C2 xor (D2 or not E2))); B2 := B2 shl 12 or B2 shr 20 + A2; D2 := D2 shl 10 or D2 shr 22;
  Inc(A2, Buffer[12] + $50A28BE6 + (B2 xor (C2 or not D2))); A2 := A2 shl  6 or A2 shr 26 + E2; C2 := C2 shl 10 or C2 shr 22;

  T := A1; A1 := A2; A2 := T;

  Inc(E1, Buffer[ 7] + $5A827999 + ((A1 and B1) or (not A1 and C1))); E1 := E1 shl  7 or E1 shr 25 + D1; B1 := B1 shl 10 or B1 shr 22;
  Inc(D1, Buffer[ 4] + $5A827999 + ((E1 and A1) or (not E1 and B1))); D1 := D1 shl  6 or D1 shr 26 + C1; A1 := A1 shl 10 or A1 shr 22;
  Inc(C1, Buffer[13] + $5A827999 + ((D1 and E1) or (not D1 and A1))); C1 := C1 shl  8 or C1 shr 24 + B1; E1 := E1 shl 10 or E1 shr 22;
  Inc(B1, Buffer[ 1] + $5A827999 + ((C1 and D1) or (not C1 and E1))); B1 := B1 shl 13 or B1 shr 19 + A1; D1 := D1 shl 10 or D1 shr 22;
  Inc(A1, Buffer[10] + $5A827999 + ((B1 and C1) or (not B1 and D1))); A1 := A1 shl 11 or A1 shr 21 + E1; C1 := C1 shl 10 or C1 shr 22;
  Inc(E1, Buffer[ 6] + $5A827999 + ((A1 and B1) or (not A1 and C1))); E1 := E1 shl  9 or E1 shr 23 + D1; B1 := B1 shl 10 or B1 shr 22;
  Inc(D1, Buffer[15] + $5A827999 + ((E1 and A1) or (not E1 and B1))); D1 := D1 shl  7 or D1 shr 25 + C1; A1 := A1 shl 10 or A1 shr 22;
  Inc(C1, Buffer[ 3] + $5A827999 + ((D1 and E1) or (not D1 and A1))); C1 := C1 shl 15 or C1 shr 17 + B1; E1 := E1 shl 10 or E1 shr 22;
  Inc(B1, Buffer[12] + $5A827999 + ((C1 and D1) or (not C1 and E1))); B1 := B1 shl  7 or B1 shr 25 + A1; D1 := D1 shl 10 or D1 shr 22;
  Inc(A1, Buffer[ 0] + $5A827999 + ((B1 and C1) or (not B1 and D1))); A1 := A1 shl 12 or A1 shr 20 + E1; C1 := C1 shl 10 or C1 shr 22;
  Inc(E1, Buffer[ 9] + $5A827999 + ((A1 and B1) or (not A1 and C1))); E1 := E1 shl 15 or E1 shr 17 + D1; B1 := B1 shl 10 or B1 shr 22;
  Inc(D1, Buffer[ 5] + $5A827999 + ((E1 and A1) or (not E1 and B1))); D1 := D1 shl  9 or D1 shr 23 + C1; A1 := A1 shl 10 or A1 shr 22;
  Inc(C1, Buffer[ 2] + $5A827999 + ((D1 and E1) or (not D1 and A1))); C1 := C1 shl 11 or C1 shr 21 + B1; E1 := E1 shl 10 or E1 shr 22;
  Inc(B1, Buffer[14] + $5A827999 + ((C1 and D1) or (not C1 and E1))); B1 := B1 shl  7 or B1 shr 25 + A1; D1 := D1 shl 10 or D1 shr 22;
  Inc(A1, Buffer[11] + $5A827999 + ((B1 and C1) or (not B1 and D1))); A1 := A1 shl 13 or A1 shr 19 + E1; C1 := C1 shl 10 or C1 shr 22;
  Inc(E1, Buffer[ 8] + $5A827999 + ((A1 and B1) or (not A1 and C1))); E1 := E1 shl 12 or E1 shr 20 + D1; B1 := B1 shl 10 or B1 shr 22;

  Inc(E2, Buffer[ 6] + $5C4DD124 + ((A2 and C2) or (B2 and not C2))); E2 := E2 shl  9 or E2 shr 23 + D2; B2 := B2 shl 10 or B2 shr 22;
  Inc(D2, Buffer[11] + $5C4DD124 + ((E2 and B2) or (A2 and not B2))); D2 := D2 shl 13 or D2 shr 19 + C2; A2 := A2 shl 10 or A2 shr 22;
  Inc(C2, Buffer[ 3] + $5C4DD124 + ((D2 and A2) or (E2 and not A2))); C2 := C2 shl 15 or C2 shr 17 + B2; E2 := E2 shl 10 or E2 shr 22;
  Inc(B2, Buffer[ 7] + $5C4DD124 + ((C2 and E2) or (D2 and not E2))); B2 := B2 shl  7 or B2 shr 25 + A2; D2 := D2 shl 10 or D2 shr 22;
  Inc(A2, Buffer[ 0] + $5C4DD124 + ((B2 and D2) or (C2 and not D2))); A2 := A2 shl 12 or A2 shr 20 + E2; C2 := C2 shl 10 or C2 shr 22;
  Inc(E2, Buffer[13] + $5C4DD124 + ((A2 and C2) or (B2 and not C2))); E2 := E2 shl  8 or E2 shr 24 + D2; B2 := B2 shl 10 or B2 shr 22;
  Inc(D2, Buffer[ 5] + $5C4DD124 + ((E2 and B2) or (A2 and not B2))); D2 := D2 shl  9 or D2 shr 23 + C2; A2 := A2 shl 10 or A2 shr 22;
  Inc(C2, Buffer[10] + $5C4DD124 + ((D2 and A2) or (E2 and not A2))); C2 := C2 shl 11 or C2 shr 21 + B2; E2 := E2 shl 10 or E2 shr 22;
  Inc(B2, Buffer[14] + $5C4DD124 + ((C2 and E2) or (D2 and not E2))); B2 := B2 shl  7 or B2 shr 25 + A2; D2 := D2 shl 10 or D2 shr 22;
  Inc(A2, Buffer[15] + $5C4DD124 + ((B2 and D2) or (C2 and not D2))); A2 := A2 shl  7 or A2 shr 25 + E2; C2 := C2 shl 10 or C2 shr 22;
  Inc(E2, Buffer[ 8] + $5C4DD124 + ((A2 and C2) or (B2 and not C2))); E2 := E2 shl 12 or E2 shr 20 + D2; B2 := B2 shl 10 or B2 shr 22;
  Inc(D2, Buffer[12] + $5C4DD124 + ((E2 and B2) or (A2 and not B2))); D2 := D2 shl  7 or D2 shr 25 + C2; A2 := A2 shl 10 or A2 shr 22;
  Inc(C2, Buffer[ 4] + $5C4DD124 + ((D2 and A2) or (E2 and not A2))); C2 := C2 shl  6 or C2 shr 26 + B2; E2 := E2 shl 10 or E2 shr 22;
  Inc(B2, Buffer[ 9] + $5C4DD124 + ((C2 and E2) or (D2 and not E2))); B2 := B2 shl 15 or B2 shr 17 + A2; D2 := D2 shl 10 or D2 shr 22;
  Inc(A2, Buffer[ 1] + $5C4DD124 + ((B2 and D2) or (C2 and not D2))); A2 := A2 shl 13 or A2 shr 19 + E2; C2 := C2 shl 10 or C2 shr 22;
  Inc(E2, Buffer[ 2] + $5C4DD124 + ((A2 and C2) or (B2 and not C2))); E2 := E2 shl 11 or E2 shr 21 + D2; B2 := B2 shl 10 or B2 shr 22;

  T := B1; B1 := B2; B2 := T;

  Inc(D1, Buffer[ 3] + $6ED9EBA1 + ((E1 or not A1) xor B1)); D1 := D1 shl 11 or D1 shr 21 + C1; A1 := A1 shl 10 or A1 shr 22;
  Inc(C1, Buffer[10] + $6ED9EBA1 + ((D1 or not E1) xor A1)); C1 := C1 shl 13 or C1 shr 19 + B1; E1 := E1 shl 10 or E1 shr 22;
  Inc(B1, Buffer[14] + $6ED9EBA1 + ((C1 or not D1) xor E1)); B1 := B1 shl  6 or B1 shr 26 + A1; D1 := D1 shl 10 or D1 shr 22;
  Inc(A1, Buffer[ 4] + $6ED9EBA1 + ((B1 or not C1) xor D1)); A1 := A1 shl  7 or A1 shr 25 + E1; C1 := C1 shl 10 or C1 shr 22;
  Inc(E1, Buffer[ 9] + $6ED9EBA1 + ((A1 or not B1) xor C1)); E1 := E1 shl 14 or E1 shr 18 + D1; B1 := B1 shl 10 or B1 shr 22;
  Inc(D1, Buffer[15] + $6ED9EBA1 + ((E1 or not A1) xor B1)); D1 := D1 shl  9 or D1 shr 23 + C1; A1 := A1 shl 10 or A1 shr 22;
  Inc(C1, Buffer[ 8] + $6ED9EBA1 + ((D1 or not E1) xor A1)); C1 := C1 shl 13 or C1 shr 19 + B1; E1 := E1 shl 10 or E1 shr 22;
  Inc(B1, Buffer[ 1] + $6ED9EBA1 + ((C1 or not D1) xor E1)); B1 := B1 shl 15 or B1 shr 17 + A1; D1 := D1 shl 10 or D1 shr 22;
  Inc(A1, Buffer[ 2] + $6ED9EBA1 + ((B1 or not C1) xor D1)); A1 := A1 shl 14 or A1 shr 18 + E1; C1 := C1 shl 10 or C1 shr 22;
  Inc(E1, Buffer[ 7] + $6ED9EBA1 + ((A1 or not B1) xor C1)); E1 := E1 shl  8 or E1 shr 24 + D1; B1 := B1 shl 10 or B1 shr 22;
  Inc(D1, Buffer[ 0] + $6ED9EBA1 + ((E1 or not A1) xor B1)); D1 := D1 shl 13 or D1 shr 19 + C1; A1 := A1 shl 10 or A1 shr 22;
  Inc(C1, Buffer[ 6] + $6ED9EBA1 + ((D1 or not E1) xor A1)); C1 := C1 shl  6 or C1 shr 26 + B1; E1 := E1 shl 10 or E1 shr 22;
  Inc(B1, Buffer[13] + $6ED9EBA1 + ((C1 or not D1) xor E1)); B1 := B1 shl  5 or B1 shr 27 + A1; D1 := D1 shl 10 or D1 shr 22;
  Inc(A1, Buffer[11] + $6ED9EBA1 + ((B1 or not C1) xor D1)); A1 := A1 shl 12 or A1 shr 20 + E1; C1 := C1 shl 10 or C1 shr 22;
  Inc(E1, Buffer[ 5] + $6ED9EBA1 + ((A1 or not B1) xor C1)); E1 := E1 shl  7 or E1 shr 25 + D1; B1 := B1 shl 10 or B1 shr 22;
  Inc(D1, Buffer[12] + $6ED9EBA1 + ((E1 or not A1) xor B1)); D1 := D1 shl  5 or D1 shr 27 + C1; A1 := A1 shl 10 or A1 shr 22;

  Inc(D2, Buffer[15] + $6D703EF3 + ((E2 or not A2) xor B2)); D2 := D2 shl  9 or D2 shr 23 + C2; A2 := A2 shl 10 or A2 shr 22;
  Inc(C2, Buffer[ 5] + $6D703EF3 + ((D2 or not E2) xor A2)); C2 := C2 shl  7 or C2 shr 25 + B2; E2 := E2 shl 10 or E2 shr 22;
  Inc(B2, Buffer[ 1] + $6D703EF3 + ((C2 or not D2) xor E2)); B2 := B2 shl 15 or B2 shr 17 + A2; D2 := D2 shl 10 or D2 shr 22;
  Inc(A2, Buffer[ 3] + $6D703EF3 + ((B2 or not C2) xor D2)); A2 := A2 shl 11 or A2 shr 21 + E2; C2 := C2 shl 10 or C2 shr 22;
  Inc(E2, Buffer[ 7] + $6D703EF3 + ((A2 or not B2) xor C2)); E2 := E2 shl  8 or E2 shr 24 + D2; B2 := B2 shl 10 or B2 shr 22;
  Inc(D2, Buffer[14] + $6D703EF3 + ((E2 or not A2) xor B2)); D2 := D2 shl  6 or D2 shr 26 + C2; A2 := A2 shl 10 or A2 shr 22;
  Inc(C2, Buffer[ 6] + $6D703EF3 + ((D2 or not E2) xor A2)); C2 := C2 shl  6 or C2 shr 26 + B2; E2 := E2 shl 10 or E2 shr 22;
  Inc(B2, Buffer[ 9] + $6D703EF3 + ((C2 or not D2) xor E2)); B2 := B2 shl 14 or B2 shr 18 + A2; D2 := D2 shl 10 or D2 shr 22;
  Inc(A2, Buffer[11] + $6D703EF3 + ((B2 or not C2) xor D2)); A2 := A2 shl 12 or A2 shr 20 + E2; C2 := C2 shl 10 or C2 shr 22;
  Inc(E2, Buffer[ 8] + $6D703EF3 + ((A2 or not B2) xor C2)); E2 := E2 shl 13 or E2 shr 19 + D2; B2 := B2 shl 10 or B2 shr 22;
  Inc(D2, Buffer[12] + $6D703EF3 + ((E2 or not A2) xor B2)); D2 := D2 shl  5 or D2 shr 27 + C2; A2 := A2 shl 10 or A2 shr 22;
  Inc(C2, Buffer[ 2] + $6D703EF3 + ((D2 or not E2) xor A2)); C2 := C2 shl 14 or C2 shr 18 + B2; E2 := E2 shl 10 or E2 shr 22;
  Inc(B2, Buffer[10] + $6D703EF3 + ((C2 or not D2) xor E2)); B2 := B2 shl 13 or B2 shr 19 + A2; D2 := D2 shl 10 or D2 shr 22;
  Inc(A2, Buffer[ 0] + $6D703EF3 + ((B2 or not C2) xor D2)); A2 := A2 shl 13 or A2 shr 19 + E2; C2 := C2 shl 10 or C2 shr 22;
  Inc(E2, Buffer[ 4] + $6D703EF3 + ((A2 or not B2) xor C2)); E2 := E2 shl  7 or E2 shr 25 + D2; B2 := B2 shl 10 or B2 shr 22;
  Inc(D2, Buffer[13] + $6D703EF3 + ((E2 or not A2) xor B2)); D2 := D2 shl  5 or D2 shr 27 + C2; A2 := A2 shl 10 or A2 shr 22;

  T := C1; C1 := C2; C2 := T;

  Inc(C1, Buffer[ 1] + $8F1BBCDC + ((D1 and A1) or (E1 and not A1))); C1 := C1 shl 11 or C1 shr 21 + B1; E1 := E1 shl 10 or E1 shr 22;
  Inc(B1, Buffer[ 9] + $8F1BBCDC + ((C1 and E1) or (D1 and not E1))); B1 := B1 shl 12 or B1 shr 20 + A1; D1 := D1 shl 10 or D1 shr 22;
  Inc(A1, Buffer[11] + $8F1BBCDC + ((B1 and D1) or (C1 and not D1))); A1 := A1 shl 14 or A1 shr 18 + E1; C1 := C1 shl 10 or C1 shr 22;
  Inc(E1, Buffer[10] + $8F1BBCDC + ((A1 and C1) or (B1 and not C1))); E1 := E1 shl 15 or E1 shr 17 + D1; B1 := B1 shl 10 or B1 shr 22;
  Inc(D1, Buffer[ 0] + $8F1BBCDC + ((E1 and B1) or (A1 and not B1))); D1 := D1 shl 14 or D1 shr 18 + C1; A1 := A1 shl 10 or A1 shr 22;
  Inc(C1, Buffer[ 8] + $8F1BBCDC + ((D1 and A1) or (E1 and not A1))); C1 := C1 shl 15 or C1 shr 17 + B1; E1 := E1 shl 10 or E1 shr 22;
  Inc(B1, Buffer[12] + $8F1BBCDC + ((C1 and E1) or (D1 and not E1))); B1 := B1 shl  9 or B1 shr 23 + A1; D1 := D1 shl 10 or D1 shr 22;
  Inc(A1, Buffer[ 4] + $8F1BBCDC + ((B1 and D1) or (C1 and not D1))); A1 := A1 shl  8 or A1 shr 24 + E1; C1 := C1 shl 10 or C1 shr 22;
  Inc(E1, Buffer[13] + $8F1BBCDC + ((A1 and C1) or (B1 and not C1))); E1 := E1 shl  9 or E1 shr 23 + D1; B1 := B1 shl 10 or B1 shr 22;
  Inc(D1, Buffer[ 3] + $8F1BBCDC + ((E1 and B1) or (A1 and not B1))); D1 := D1 shl 14 or D1 shr 18 + C1; A1 := A1 shl 10 or A1 shr 22;
  Inc(C1, Buffer[ 7] + $8F1BBCDC + ((D1 and A1) or (E1 and not A1))); C1 := C1 shl  5 or C1 shr 27 + B1; E1 := E1 shl 10 or E1 shr 22;
  Inc(B1, Buffer[15] + $8F1BBCDC + ((C1 and E1) or (D1 and not E1))); B1 := B1 shl  6 or B1 shr 26 + A1; D1 := D1 shl 10 or D1 shr 22;
  Inc(A1, Buffer[14] + $8F1BBCDC + ((B1 and D1) or (C1 and not D1))); A1 := A1 shl  8 or A1 shr 24 + E1; C1 := C1 shl 10 or C1 shr 22;
  Inc(E1, Buffer[ 5] + $8F1BBCDC + ((A1 and C1) or (B1 and not C1))); E1 := E1 shl  6 or E1 shr 26 + D1; B1 := B1 shl 10 or B1 shr 22;
  Inc(D1, Buffer[ 6] + $8F1BBCDC + ((E1 and B1) or (A1 and not B1))); D1 := D1 shl  5 or D1 shr 27 + C1; A1 := A1 shl 10 or A1 shr 22;
  Inc(C1, Buffer[ 2] + $8F1BBCDC + ((D1 and A1) or (E1 and not A1))); C1 := C1 shl 12 or C1 shr 20 + B1; E1 := E1 shl 10 or E1 shr 22;

  Inc(C2, Buffer[ 8] + $7A6D76E9 + ((D2 and E2) or (not D2 and A2))); C2 := C2 shl 15 or C2 shr 17 + B2; E2 := E2 shl 10 or E2 shr 22;
  Inc(B2, Buffer[ 6] + $7A6D76E9 + ((C2 and D2) or (not C2 and E2))); B2 := B2 shl  5 or B2 shr 27 + A2; D2 := D2 shl 10 or D2 shr 22;
  Inc(A2, Buffer[ 4] + $7A6D76E9 + ((B2 and C2) or (not B2 and D2))); A2 := A2 shl  8 or A2 shr 24 + E2; C2 := C2 shl 10 or C2 shr 22;
  Inc(E2, Buffer[ 1] + $7A6D76E9 + ((A2 and B2) or (not A2 and C2))); E2 := E2 shl 11 or E2 shr 21 + D2; B2 := B2 shl 10 or B2 shr 22;
  Inc(D2, Buffer[ 3] + $7A6D76E9 + ((E2 and A2) or (not E2 and B2))); D2 := D2 shl 14 or D2 shr 18 + C2; A2 := A2 shl 10 or A2 shr 22;
  Inc(C2, Buffer[11] + $7A6D76E9 + ((D2 and E2) or (not D2 and A2))); C2 := C2 shl 14 or C2 shr 18 + B2; E2 := E2 shl 10 or E2 shr 22;
  Inc(B2, Buffer[15] + $7A6D76E9 + ((C2 and D2) or (not C2 and E2))); B2 := B2 shl  6 or B2 shr 26 + A2; D2 := D2 shl 10 or D2 shr 22;
  Inc(A2, Buffer[ 0] + $7A6D76E9 + ((B2 and C2) or (not B2 and D2))); A2 := A2 shl 14 or A2 shr 18 + E2; C2 := C2 shl 10 or C2 shr 22;
  Inc(E2, Buffer[ 5] + $7A6D76E9 + ((A2 and B2) or (not A2 and C2))); E2 := E2 shl  6 or E2 shr 26 + D2; B2 := B2 shl 10 or B2 shr 22;
  Inc(D2, Buffer[12] + $7A6D76E9 + ((E2 and A2) or (not E2 and B2))); D2 := D2 shl  9 or D2 shr 23 + C2; A2 := A2 shl 10 or A2 shr 22;
  Inc(C2, Buffer[ 2] + $7A6D76E9 + ((D2 and E2) or (not D2 and A2))); C2 := C2 shl 12 or C2 shr 20 + B2; E2 := E2 shl 10 or E2 shr 22;
  Inc(B2, Buffer[13] + $7A6D76E9 + ((C2 and D2) or (not C2 and E2))); B2 := B2 shl  9 or B2 shr 23 + A2; D2 := D2 shl 10 or D2 shr 22;
  Inc(A2, Buffer[ 9] + $7A6D76E9 + ((B2 and C2) or (not B2 and D2))); A2 := A2 shl 12 or A2 shr 20 + E2; C2 := C2 shl 10 or C2 shr 22;
  Inc(E2, Buffer[ 7] + $7A6D76E9 + ((A2 and B2) or (not A2 and C2))); E2 := E2 shl  5 or E2 shr 27 + D2; B2 := B2 shl 10 or B2 shr 22;
  Inc(D2, Buffer[10] + $7A6D76E9 + ((E2 and A2) or (not E2 and B2))); D2 := D2 shl 15 or D2 shr 17 + C2; A2 := A2 shl 10 or A2 shr 22;
  Inc(C2, Buffer[14] + $7A6D76E9 + ((D2 and E2) or (not D2 and A2))); C2 := C2 shl  8 or C2 shr 24 + B2; E2 := E2 shl 10 or E2 shr 22;

  T := D1; D1 := D2; D2 := T;

  Inc(B1, Buffer[ 4] + $A953FD4E + (C1 xor (D1 or not E1))); B1 := B1 shl  9 or B1 shr 23 + A1; D1 := D1 shl 10 or D1 shr 22;
  Inc(A1, Buffer[ 0] + $A953FD4E + (B1 xor (C1 or not D1))); A1 := A1 shl 15 or A1 shr 17 + E1; C1 := C1 shl 10 or C1 shr 22;
  Inc(E1, Buffer[ 5] + $A953FD4E + (A1 xor (B1 or not C1))); E1 := E1 shl  5 or E1 shr 27 + D1; B1 := B1 shl 10 or B1 shr 22;
  Inc(D1, Buffer[ 9] + $A953FD4E + (E1 xor (A1 or not B1))); D1 := D1 shl 11 or D1 shr 21 + C1; A1 := A1 shl 10 or A1 shr 22;
  Inc(C1, Buffer[ 7] + $A953FD4E + (D1 xor (E1 or not A1))); C1 := C1 shl  6 or C1 shr 26 + B1; E1 := E1 shl 10 or E1 shr 22;
  Inc(B1, Buffer[12] + $A953FD4E + (C1 xor (D1 or not E1))); B1 := B1 shl  8 or B1 shr 24 + A1; D1 := D1 shl 10 or D1 shr 22;
  Inc(A1, Buffer[ 2] + $A953FD4E + (B1 xor (C1 or not D1))); A1 := A1 shl 13 or A1 shr 19 + E1; C1 := C1 shl 10 or C1 shr 22;
  Inc(E1, Buffer[10] + $A953FD4E + (A1 xor (B1 or not C1))); E1 := E1 shl 12 or E1 shr 20 + D1; B1 := B1 shl 10 or B1 shr 22;
  Inc(D1, Buffer[14] + $A953FD4E + (E1 xor (A1 or not B1))); D1 := D1 shl  5 or D1 shr 27 + C1; A1 := A1 shl 10 or A1 shr 22;
  Inc(C1, Buffer[ 1] + $A953FD4E + (D1 xor (E1 or not A1))); C1 := C1 shl 12 or C1 shr 20 + B1; E1 := E1 shl 10 or E1 shr 22;
  Inc(B1, Buffer[ 3] + $A953FD4E + (C1 xor (D1 or not E1))); B1 := B1 shl 13 or B1 shr 19 + A1; D1 := D1 shl 10 or D1 shr 22;
  Inc(A1, Buffer[ 8] + $A953FD4E + (B1 xor (C1 or not D1))); A1 := A1 shl 14 or A1 shr 18 + E1; C1 := C1 shl 10 or C1 shr 22;
  Inc(E1, Buffer[11] + $A953FD4E + (A1 xor (B1 or not C1))); E1 := E1 shl 11 or E1 shr 21 + D1; B1 := B1 shl 10 or B1 shr 22;
  Inc(D1, Buffer[ 6] + $A953FD4E + (E1 xor (A1 or not B1))); D1 := D1 shl  8 or D1 shr 24 + C1; A1 := A1 shl 10 or A1 shr 22;
  Inc(C1, Buffer[15] + $A953FD4E + (D1 xor (E1 or not A1))); C1 := C1 shl  5 or C1 shr 27 + B1; E1 := E1 shl 10 or E1 shr 22;
  Inc(B1, Buffer[13] + $A953FD4E + (C1 xor (D1 or not E1))); B1 := B1 shl  6 or B1 shr 26 + A1; D1 := D1 shl 10 or D1 shr 22;

  Inc(B2, Buffer[12] + (C2 xor D2 xor E2)); B2 := B2 shl  8 or B2 shr 24 + A2; D2 := D2 shl 10 or D2 shr 22;
  Inc(A2, Buffer[15] + (B2 xor C2 xor D2)); A2 := A2 shl  5 or A2 shr 27 + E2; C2 := C2 shl 10 or C2 shr 22;
  Inc(E2, Buffer[10] + (A2 xor B2 xor C2)); E2 := E2 shl 12 or E2 shr 20 + D2; B2 := B2 shl 10 or B2 shr 22;
  Inc(D2, Buffer[ 4] + (E2 xor A2 xor B2)); D2 := D2 shl  9 or D2 shr 23 + C2; A2 := A2 shl 10 or A2 shr 22;
  Inc(C2, Buffer[ 1] + (D2 xor E2 xor A2)); C2 := C2 shl 12 or C2 shr 20 + B2; E2 := E2 shl 10 or E2 shr 22;
  Inc(B2, Buffer[ 5] + (C2 xor D2 xor E2)); B2 := B2 shl  5 or B2 shr 27 + A2; D2 := D2 shl 10 or D2 shr 22;
  Inc(A2, Buffer[ 8] + (B2 xor C2 xor D2)); A2 := A2 shl 14 or A2 shr 18 + E2; C2 := C2 shl 10 or C2 shr 22;
  Inc(E2, Buffer[ 7] + (A2 xor B2 xor C2)); E2 := E2 shl  6 or E2 shr 26 + D2; B2 := B2 shl 10 or B2 shr 22;
  Inc(D2, Buffer[ 6] + (E2 xor A2 xor B2)); D2 := D2 shl  8 or D2 shr 24 + C2; A2 := A2 shl 10 or A2 shr 22;
  Inc(C2, Buffer[ 2] + (D2 xor E2 xor A2)); C2 := C2 shl 13 or C2 shr 19 + B2; E2 := E2 shl 10 or E2 shr 22;
  Inc(B2, Buffer[13] + (C2 xor D2 xor E2)); B2 := B2 shl  6 or B2 shr 26 + A2; D2 := D2 shl 10 or D2 shr 22;
  Inc(A2, Buffer[14] + (B2 xor C2 xor D2)); A2 := A2 shl  5 or A2 shr 27 + E2; C2 := C2 shl 10 or C2 shr 22;
  Inc(E2, Buffer[ 0] + (A2 xor B2 xor C2)); E2 := E2 shl 15 or E2 shr 17 + D2; B2 := B2 shl 10 or B2 shr 22;
  Inc(D2, Buffer[ 3] + (E2 xor A2 xor B2)); D2 := D2 shl 13 or D2 shr 19 + C2; A2 := A2 shl 10 or A2 shr 22;
  Inc(C2, Buffer[ 9] + (D2 xor E2 xor A2)); C2 := C2 shl 11 or C2 shr 21 + B2; E2 := E2 shl 10 or E2 shr 22;
  Inc(B2, Buffer[11] + (C2 xor D2 xor E2)); B2 := B2 shl 11 or B2 shr 21 + A2; D2 := D2 shl 10 or D2 shr 22;

  T := E1; E1 := E2; E2 := T;

  Inc(FDigest[0], A1);
  Inc(FDigest[1], B1);
  Inc(FDigest[2], C1);
  Inc(FDigest[3], D1);
  Inc(FDigest[4], E1);
  Inc(FDigest[5], A2);
  Inc(FDigest[6], B2);
  Inc(FDigest[7], C2);
  Inc(FDigest[8], D2);
  Inc(FDigest[9], E2);
end;

class function THash_RipeMD320.DigestKeySize: Integer;
begin
  Result := 40;
end;

class function THash_SHA.TestVector: Pointer;
asm
         MOV   EAX,OFFSET @Vector
         RET
@Vector: DB    0DCh,01Fh,07Dh,07Ch,096h,0DDh,0C7h,0FCh
         DB    04Dh,00Ah,0F2h,0CCh,012h,0E7h,0F7h,066h
         DB    05Bh,0B1h,085h,0ACh
end;

procedure THash_SHA.Transform(Buffer: PIntArray);

  procedure AssignBuffer(S, D: Pointer; Rotate: Boolean); assembler;
{  for I := 0 to 15 do W[I] := SwapInteger(Buffer[I]);
  for i:= 16 to 79 do
--- SHA  Rotate = False ---
    W[i] :=     W[I-3] xor W[I-8] xor W[i-14] xor W[i-16]
--- SHA1 Rotate = True  ---
    W[i] := ROL(W[i-3] xor W[i-8] xor W[i-14] xor W[i-16], 1);
    }
  asm
     PUSH  EBX
     PUSH  ECX
     MOV   EBX,EAX
     XOR   ECX,ECX
     CMP   FCPUType,4
     JGE   @2
@1:  MOV   EAX,[EDX + ECX * 4]
     XCHG  AL,AH
     ROL   EAX,16
     XCHG  AL,AH
     MOV   [EBX],EAX
     ADD   EBX,4
     INC   ECX
     CMP   ECX,16
     JNZ   @1
     JMP   @@1

@2:  MOV   EAX,[EDX + ECX * 4]
     BSWAP EAX
     MOV   [EBX],EAX
     ADD   EBX,4
     INC   ECX
     CMP   ECX,16
     JNZ   @2
@@1:
     MOV   ECX,64
     POP   EDX
     CMP   DL,0
     JZ    @@3
@@2: MOV   EAX,[EBX -  3 * 4]
     XOR   EAX,[EBX -  8 * 4]
     XOR   EAX,[EBX - 14 * 4]
     XOR   EAX,[EBX - 16 * 4]
     ROL   EAX,1
     MOV   [EBX],EAX
     ADD   EBX,4
     LOOP  @@2
     JMP   @@4

@@3: MOV   EAX,[EBX -  3 * 4]
     XOR   EAX,[EBX -  8 * 4]
     XOR   EAX,[EBX - 14 * 4]
     XOR   EAX,[EBX - 16 * 4]
     MOV   [EBX],EAX
     ADD   EBX,4
     LOOP  @@3

@@4: POP   EBX
  end;

var
  A, B, C, D, E: LongWord;
  W: array[0..79] of LongWord;
begin
  AssignBuffer(@W, Buffer, FRotate);

  A := FDigest[0];
  B := FDigest[1];
  C := FDigest[2];
  D := FDigest[3];
  E := FDigest[4];

  Inc(E, (A shl 5 or A shr 27) + (D xor (B and (C xor D))) + W[ 0] + $5A827999); B := B shr 2 or B shl 30;
  Inc(D, (E shl 5 or E shr 27) + (C xor (A and (B xor C))) + W[ 1] + $5A827999); A := A shr 2 or A shl 30;
  Inc(C, (D shl 5 or D shr 27) + (B xor (E and (A xor B))) + W[ 2] + $5A827999); E := E shr 2 or E shl 30;
  Inc(B, (C shl 5 or C shr 27) + (A xor (D and (E xor A))) + W[ 3] + $5A827999); D := D shr 2 or D shl 30;
  Inc(A, (B shl 5 or B shr 27) + (E xor (C and (D xor E))) + W[ 4] + $5A827999); C := C shr 2 or C shl 30;
  Inc(E, (A shl 5 or A shr 27) + (D xor (B and (C xor D))) + W[ 5] + $5A827999); B := B shr 2 or B shl 30;
  Inc(D, (E shl 5 or E shr 27) + (C xor (A and (B xor C))) + W[ 6] + $5A827999); A := A shr 2 or A shl 30;
  Inc(C, (D shl 5 or D shr 27) + (B xor (E and (A xor B))) + W[ 7] + $5A827999); E := E shr 2 or E shl 30;
  Inc(B, (C shl 5 or C shr 27) + (A xor (D and (E xor A))) + W[ 8] + $5A827999); D := D shr 2 or D shl 30;
  Inc(A, (B shl 5 or B shr 27) + (E xor (C and (D xor E))) + W[ 9] + $5A827999); C := C shr 2 or C shl 30;
  Inc(E, (A shl 5 or A shr 27) + (D xor (B and (C xor D))) + W[10] + $5A827999); B := B shr 2 or B shl 30;
  Inc(D, (E shl 5 or E shr 27) + (C xor (A and (B xor C))) + W[11] + $5A827999); A := A shr 2 or A shl 30;
  Inc(C, (D shl 5 or D shr 27) + (B xor (E and (A xor B))) + W[12] + $5A827999); E := E shr 2 or E shl 30;
  Inc(B, (C shl 5 or C shr 27) + (A xor (D and (E xor A))) + W[13] + $5A827999); D := D shr 2 or D shl 30;
  Inc(A, (B shl 5 or B shr 27) + (E xor (C and (D xor E))) + W[14] + $5A827999); C := C shr 2 or C shl 30;
  Inc(E, (A shl 5 or A shr 27) + (D xor (B and (C xor D))) + W[15] + $5A827999); B := B shr 2 or B shl 30;
  Inc(D, (E shl 5 or E shr 27) + (C xor (A and (B xor C))) + W[16] + $5A827999); A := A shr 2 or A shl 30;
  Inc(C, (D shl 5 or D shr 27) + (B xor (E and (A xor B))) + W[17] + $5A827999); E := E shr 2 or E shl 30;
  Inc(B, (C shl 5 or C shr 27) + (A xor (D and (E xor A))) + W[18] + $5A827999); D := D shr 2 or D shl 30;
  Inc(A, (B shl 5 or B shr 27) + (E xor (C and (D xor E))) + W[19] + $5A827999); C := C shr 2 or C shl 30;

  Inc(E, (A shl 5 or A shr 27) + (D xor B xor C) + W[20] + $6ED9EBA1); B := B shr 2 or B shl 30;
  Inc(D, (E shl 5 or E shr 27) + (C xor A xor B) + W[21] + $6ED9EBA1); A := A shr 2 or A shl 30;
  Inc(C, (D shl 5 or D shr 27) + (B xor E xor A) + W[22] + $6ED9EBA1); E := E shr 2 or E shl 30;
  Inc(B, (C shl 5 or C shr 27) + (A xor D xor E) + W[23] + $6ED9EBA1); D := D shr 2 or D shl 30;
  Inc(A, (B shl 5 or B shr 27) + (E xor C xor D) + W[24] + $6ED9EBA1); C := C shr 2 or C shl 30;
  Inc(E, (A shl 5 or A shr 27) + (D xor B xor C) + W[25] + $6ED9EBA1); B := B shr 2 or B shl 30;
  Inc(D, (E shl 5 or E shr 27) + (C xor A xor B) + W[26] + $6ED9EBA1); A := A shr 2 or A shl 30;
  Inc(C, (D shl 5 or D shr 27) + (B xor E xor A) + W[27] + $6ED9EBA1); E := E shr 2 or E shl 30;
  Inc(B, (C shl 5 or C shr 27) + (A xor D xor E) + W[28] + $6ED9EBA1); D := D shr 2 or D shl 30;
  Inc(A, (B shl 5 or B shr 27) + (E xor C xor D) + W[29] + $6ED9EBA1); C := C shr 2 or C shl 30;
  Inc(E, (A shl 5 or A shr 27) + (D xor B xor C) + W[30] + $6ED9EBA1); B := B shr 2 or B shl 30;
  Inc(D, (E shl 5 or E shr 27) + (C xor A xor B) + W[31] + $6ED9EBA1); A := A shr 2 or A shl 30;
  Inc(C, (D shl 5 or D shr 27) + (B xor E xor A) + W[32] + $6ED9EBA1); E := E shr 2 or E shl 30;
  Inc(B, (C shl 5 or C shr 27) + (A xor D xor E) + W[33] + $6ED9EBA1); D := D shr 2 or D shl 30;
  Inc(A, (B shl 5 or B shr 27) + (E xor C xor D) + W[34] + $6ED9EBA1); C := C shr 2 or C shl 30;
  Inc(E, (A shl 5 or A shr 27) + (D xor B xor C) + W[35] + $6ED9EBA1); B := B shr 2 or B shl 30;
  Inc(D, (E shl 5 or E shr 27) + (C xor A xor B) + W[36] + $6ED9EBA1); A := A shr 2 or A shl 30;
  Inc(C, (D shl 5 or D shr 27) + (B xor E xor A) + W[37] + $6ED9EBA1); E := E shr 2 or E shl 30;
  Inc(B, (C shl 5 or C shr 27) + (A xor D xor E) + W[38] + $6ED9EBA1); D := D shr 2 or D shl 30;
  Inc(A, (B shl 5 or B shr 27) + (E xor C xor D) + W[39] + $6ED9EBA1); C := C shr 2 or C shl 30;

  Inc(E, (A shl 5 or A shr 27) + ((B and C) or (D and (B or C))) + W[40] + $8F1BBCDC); B := B shr 2 or B shl 30;
  Inc(D, (E shl 5 or E shr 27) + ((A and B) or (C and (A or B))) + W[41] + $8F1BBCDC); A := A shr 2 or A shl 30;
  Inc(C, (D shl 5 or D shr 27) + ((E and A) or (B and (E or A))) + W[42] + $8F1BBCDC); E := E shr 2 or E shl 30;
  Inc(B, (C shl 5 or C shr 27) + ((D and E) or (A and (D or E))) + W[43] + $8F1BBCDC); D := D shr 2 or D shl 30;
  Inc(A, (B shl 5 or B shr 27) + ((C and D) or (E and (C or D))) + W[44] + $8F1BBCDC); C := C shr 2 or C shl 30;
  Inc(E, (A shl 5 or A shr 27) + ((B and C) or (D and (B or C))) + W[45] + $8F1BBCDC); B := B shr 2 or B shl 30;
  Inc(D, (E shl 5 or E shr 27) + ((A and B) or (C and (A or B))) + W[46] + $8F1BBCDC); A := A shr 2 or A shl 30;
  Inc(C, (D shl 5 or D shr 27) + ((E and A) or (B and (E or A))) + W[47] + $8F1BBCDC); E := E shr 2 or E shl 30;
  Inc(B, (C shl 5 or C shr 27) + ((D and E) or (A and (D or E))) + W[48] + $8F1BBCDC); D := D shr 2 or D shl 30;
  Inc(A, (B shl 5 or B shr 27) + ((C and D) or (E and (C or D))) + W[49] + $8F1BBCDC); C := C shr 2 or C shl 30;
  Inc(E, (A shl 5 or A shr 27) + ((B and C) or (D and (B or C))) + W[50] + $8F1BBCDC); B := B shr 2 or B shl 30;
  Inc(D, (E shl 5 or E shr 27) + ((A and B) or (C and (A or B))) + W[51] + $8F1BBCDC); A := A shr 2 or A shl 30;
  Inc(C, (D shl 5 or D shr 27) + ((E and A) or (B and (E or A))) + W[52] + $8F1BBCDC); E := E shr 2 or E shl 30;
  Inc(B, (C shl 5 or C shr 27) + ((D and E) or (A and (D or E))) + W[53] + $8F1BBCDC); D := D shr 2 or D shl 30;
  Inc(A, (B shl 5 or B shr 27) + ((C and D) or (E and (C or D))) + W[54] + $8F1BBCDC); C := C shr 2 or C shl 30;
  Inc(E, (A shl 5 or A shr 27) + ((B and C) or (D and (B or C))) + W[55] + $8F1BBCDC); B := B shr 2 or B shl 30;
  Inc(D, (E shl 5 or E shr 27) + ((A and B) or (C and (A or B))) + W[56] + $8F1BBCDC); A := A shr 2 or A shl 30;
  Inc(C, (D shl 5 or D shr 27) + ((E and A) or (B and (E or A))) + W[57] + $8F1BBCDC); E := E shr 2 or E shl 30;
  Inc(B, (C shl 5 or C shr 27) + ((D and E) or (A and (D or E))) + W[58] + $8F1BBCDC); D := D shr 2 or D shl 30;
  Inc(A, (B shl 5 or B shr 27) + ((C and D) or (E and (C or D))) + W[59] + $8F1BBCDC); C := C shr 2 or C shl 30;

  Inc(E, (A shl 5 or A shr 27) + (D xor B xor C) + W[60] + $CA62C1D6); B := B shr 2 or B shl 30;
  Inc(D, (E shl 5 or E shr 27) + (C xor A xor B) + W[61] + $CA62C1D6); A := A shr 2 or A shl 30;
  Inc(C, (D shl 5 or D shr 27) + (B xor E xor A) + W[62] + $CA62C1D6); E := E shr 2 or E shl 30;
  Inc(B, (C shl 5 or C shr 27) + (A xor D xor E) + W[63] + $CA62C1D6); D := D shr 2 or D shl 30;
  Inc(A, (B shl 5 or B shr 27) + (E xor C xor D) + W[64] + $CA62C1D6); C := C shr 2 or C shl 30;
  Inc(E, (A shl 5 or A shr 27) + (D xor B xor C) + W[65] + $CA62C1D6); B := B shr 2 or B shl 30;
  Inc(D, (E shl 5 or E shr 27) + (C xor A xor B) + W[66] + $CA62C1D6); A := A shr 2 or A shl 30;
  Inc(C, (D shl 5 or D shr 27) + (B xor E xor A) + W[67] + $CA62C1D6); E := E shr 2 or E shl 30;
  Inc(B, (C shl 5 or C shr 27) + (A xor D xor E) + W[68] + $CA62C1D6); D := D shr 2 or D shl 30;
  Inc(A, (B shl 5 or B shr 27) + (E xor C xor D) + W[69] + $CA62C1D6); C := C shr 2 or C shl 30;
  Inc(E, (A shl 5 or A shr 27) + (D xor B xor C) + W[70] + $CA62C1D6); B := B shr 2 or B shl 30;
  Inc(D, (E shl 5 or E shr 27) + (C xor A xor B) + W[71] + $CA62C1D6); A := A shr 2 or A shl 30;
  Inc(C, (D shl 5 or D shr 27) + (B xor E xor A) + W[72] + $CA62C1D6); E := E shr 2 or E shl 30;
  Inc(B, (C shl 5 or C shr 27) + (A xor D xor E) + W[73] + $CA62C1D6); D := D shr 2 or D shl 30;
  Inc(A, (B shl 5 or B shr 27) + (E xor C xor D) + W[74] + $CA62C1D6); C := C shr 2 or C shl 30;
  Inc(E, (A shl 5 or A shr 27) + (D xor B xor C) + W[75] + $CA62C1D6); B := B shr 2 or B shl 30;
  Inc(D, (E shl 5 or E shr 27) + (C xor A xor B) + W[76] + $CA62C1D6); A := A shr 2 or A shl 30;
  Inc(C, (D shl 5 or D shr 27) + (B xor E xor A) + W[77] + $CA62C1D6); E := E shr 2 or E shl 30;
  Inc(B, (C shl 5 or C shr 27) + (A xor D xor E) + W[78] + $CA62C1D6); D := D shr 2 or D shl 30;
  Inc(A, (B shl 5 or B shr 27) + (E xor C xor D) + W[79] + $CA62C1D6); C := C shr 2 or C shl 30;

  Inc(FDigest[0], A);
  Inc(FDigest[1], B);
  Inc(FDigest[2], C);
  Inc(FDigest[3], D);
  Inc(FDigest[4], E);
end;

procedure THash_SHA.Done;
var
  I: Integer;
  S: Comp;
begin
  I := FCount mod 64;
  FBuffer[I] := $80;
  Inc(I);
  if I > 64 - 8 then
  begin
    FillChar(FBuffer[I], 64 - I, 0);
    Transform(@FBuffer);
    I := 0;
  end;
  FillChar(FBuffer[I], 64 - I, 0);
  S := FCount * 8;
  for I := 0 to 7 do FBuffer[63 - I] := PByteArray(@S)^[I];
  Transform(@FBuffer);
  FillChar(FBuffer, SizeOf(FBuffer), 0);
{and here the MAC}
  SwapIntegerBuffer(@FDigest, @FDigest, 5);
end;

class function THash_SHA1.TestVector: Pointer;
asm
         MOV   EAX,OFFSET @Vector
         RET
@Vector: DB    09Ah,001h,02Eh,063h,096h,02Ah,092h,0EBh
         DB    0D8h,02Eh,0F0h,0BCh,01Ch,0A4h,051h,06Ah
         DB    008h,069h,02Eh,068h
end;

class function THash_SHA1.NewInstance: TObject;
begin
  Result := inherited NewInstance;
  if Result <> nil then THash_SHA1(Result).FRotate := True;
end;

{3 Rounds}
class function THash_Sapphire320.TestVector: Pointer;
asm
         MOV   EAX,OFFSET @Vector
         RET
@Vector: DB    0D8h,0EDh,0A5h,074h,099h,048h,0ACh,00Bh
         DB    000h,090h,0CDh,087h,061h,0F2h,018h,0DBh
         DB    0A4h,01Dh,0CBh,073h,0E4h,033h,099h,0DDh
         DB    037h,012h,006h,093h,097h,033h,095h,000h
         DB    04Ah,0CFh,086h,0DBh,023h,06Dh,0E7h,06Bh
end;

procedure THash_Sapphire320.Calc(const Data; DataSize: Integer);
var
  C: Integer;
  T: Byte;
  D: PByte;
begin
  Inc(FCount, DataSize);
  D := @Data;
  C := FCipher;
  while DataSize > 0 do
  begin
    Dec(DataSize);
    Inc(FRatchet, FCards[FRotor]);
    Inc(FRotor);
    T := FCards[C];
    FCards[C] := FCards[FRatchet];
    FCards[FRatchet] := FCards[FPlain];
    FCards[FPlain] := FCards[FRotor];
    FCards[FRotor] := T;
    Inc(FAvalanche, FCards[T]);
    T := FCards[FPlain] + FCards[C] + FCards[FAvalanche];
    FPlain := D^; Inc(D);
    C := FPlain xor FCards[FCards[T]] xor
                    FCards[(FCards[FRatchet] + FCards[FRotor]) and $FF];
  end;
  FCipher := C;
end;

class function THash_Sapphire320.DigestKeySize: Integer;
begin
  Result := 40;
end;

procedure THash_Sapphire320.Init;
var
  I: Integer;
begin
  FillChar(FDigest, SizeOf(FDigest), 0);
  FCount := 0;
  FRotor := 1;
  FRatchet := 3;
  FAvalanche := 5;
  FPlain := 7;
  FCipher := 11;
  for I := 0 to 255 do FCards[I] := 255 - I;
end;

procedure THash_Sapphire320.Done;
{final sequence, I have here changed the original Code by adding the
 Trailor, Version, Digestsize, Init-values and Size in Bits,
 I mean this changes make a uniquer Fingerprint} 
var
  I,J: Byte;
  B: array[0..39] of Byte;
  S: Comp;
begin
  J := DigestKeySize;
  for I := 255 downto 0 do
  begin
    J := J xor I;
    Calc(J, 1);
  end;
  FillChar(B, SizeOf(B), 0);
  B[0] := $80;     {Trailorbyte}
  B[1] := $02;     {Version}
  PInteger(@B[2])^ := DigestKeySize shl 8;  {Digestsize in bits}
  B[4] :=   1;     {Init from FRotor}
  B[5] :=   3;     {Init from FRatchet}
  B[6] :=   5;     {Init from FAvalanche}
  B[7] :=   7;     {Init from FPlain}
  B[8] :=  11;     {Init from FCipher};
  S := FCount * 8; {Size in Bits}
  Move(S, B[9], SizeOf(S));
  for I := 0 to DigestKeySize-1 do
  begin
    Calc(B[I], 1);
    PByteArray(@FDigest)[I] := PByteArray(@FDigest)[I] xor FCipher;
  end;
end;

function THash_Sapphire320.DigestKey: Pointer;
begin
  Result := @FDigest;
end;

class function THash_Sapphire288.TestVector: Pointer;
asm
         MOV   EAX,OFFSET @Vector
         RET
@Vector: DB    040h,0F6h,05Ah,037h,0D8h,0CBh,011h,065h
         DB    0DCh,0FBh,0DEh,063h,02Bh,02Ch,0BEh,09Ch
         DB    02Bh,0B7h,0F3h,0EDh,022h,0D1h,0EBh,0EFh
         DB    00Fh,0A1h,088h,0EDh,023h,0F0h,07Fh,09Ch
         DB    07Eh,061h,037h,040h
end;

class function THash_Sapphire288.DigestKeySize: Integer;
begin
  Result := 36;
end;

class function THash_Sapphire256.TestVector: Pointer;
asm
         MOV   EAX,OFFSET @Vector
         RET
@Vector: DB    0E7h,0C8h,02Bh,06Dh,025h,08Ah,0FDh,0E0h
         DB    034h,042h,093h,0A3h,0E2h,0E5h,028h,096h
         DB    0A5h,046h,050h,06Bh,013h,093h,086h,01Bh
         DB    040h,0C6h,093h,04Bh,051h,0FAh,066h,0DDh
end;

class function THash_Sapphire256.DigestKeySize: Integer;
begin
  Result := 32;
end;

class function THash_Sapphire224.TestVector: Pointer;
asm
         MOV   EAX,OFFSET @Vector
         RET
@Vector: DB    08Fh,03Eh,0C5h,057h,092h,031h,06Ch,0DFh
         DB    0CAh,040h,018h,03Ah,086h,008h,008h,099h
         DB    034h,0FDh,0EBh,021h,0ABh,0A4h,052h,00Fh
         DB    040h,017h,0D3h,0D0h
end;

class function THash_Sapphire224.DigestKeySize: Integer;
begin
  Result := 28;
end;

class function THash_Sapphire192.TestVector: Pointer;
asm
         MOV   EAX,OFFSET @Vector
         RET
@Vector: DB    095h,041h,03Fh,0E0h,02Bh,052h,07Eh,050h
         DB    0B5h,0FFh,057h,0EBh,076h,03Dh,032h,051h
         DB    029h,0EBh,079h,06Fh,04Ah,064h,0FBh,017h
end;

class function THash_Sapphire192.DigestKeySize: Integer;
begin
  Result := 24;
end;

class function THash_Sapphire160.TestVector: Pointer;
asm
         MOV   EAX,OFFSET @Vector
         RET
@Vector: DB    073h,0D9h,01Ah,0CEh,0B1h,075h,0FDh,004h
         DB    090h,07Ah,097h,041h,03Ch,020h,00Fh,053h
         DB    02Ch,022h,0B3h,08Ch
end;

class function THash_Sapphire160.DigestKeySize: Integer;
begin
  Result := 20;
end;

class function THash_Sapphire128.TestVector: Pointer;
asm
         MOV   EAX,OFFSET @Vector
         RET
@Vector: DB    047h,0AEh,016h,013h,0C9h,03Ah,094h,02Dh
         DB    026h,074h,08Dh,062h,043h,026h,088h,0D8h
end;

class function THash_Sapphire128.DigestKeySize: Integer;
begin
  Result := 16;
end;

class function THash_XOR16.TestVector: Pointer;
asm
         MOV   EAX,OFFSET @Vector
         RET
@Vector: DB    079h,0E8h
end;

class function THash_XOR16.DigestKeySize: Integer;
begin
  Result := 2;
end;

procedure THash_XOR16.Init;
begin
  FCRC := 0;
end;

procedure THash_XOR16.Calc(const Data; DataSize: Integer); assembler; register;
asm
         JECXZ   @Exit
         PUSH    EAX
         MOV     AX,[EAX].THash_XOR16.FCRC
@@1:     ROL     AX,5
         XOR     AL,[EDX]
         INC     EDX
         LOOP    @@1
         POP     EDX
         MOV     [EDX].THash_XOR16.FCRC,AX
@Exit:
end;

function THash_XOR16.DigestKey: Pointer;
begin
  Result := @FCRC;
end;

class function THash_XOR32.TestVector: Pointer;
asm
         MOV   EAX,OFFSET @Vector
         RET
@Vector: DB    08Dh,0ADh,089h,07Fh
end;

class function THash_XOR32.DigestKeySize: Integer;
begin
  Result := 4;
end;

procedure THash_XOR32.Init;
begin
  FCRC := 0;
end;

procedure THash_XOR32.Calc(const Data; DataSize: Integer); assembler; register;
asm
         JECXZ   @Exit
         PUSH    EAX
         MOV     EAX,[EAX].THash_XOR32.FCRC
         TEST    ECX,1
         JE      @@1
         XOR     AX,[EDX]
         INC     EDX
@@1:     SHR     ECX,1
         JECXZ   @@3
@@2:     ROL     EAX,5
         XOR     AX,[EDX]
         ADD     EDX,2
         LOOP    @@2
@@3:     POP     EDX
         MOV     [EDX].THash_XOR32.FCRC,EAX
@Exit:
end;

function THash_XOR32.DigestKey: Pointer;
begin
  Result := @FCRC;
end;

class function THash_CRC32.TestVector: Pointer;
asm
         MOV   EAX,OFFSET @Vector
         RET
@Vector: DB    058h,0EEh,01Fh,031h
end;

procedure THash_CRC32.Init;
begin
  FCRC := $FFFFFFFF;
end;

procedure THash_CRC32.Calc(const Data; DataSize: Integer); assembler; register;
asm
         PUSH   EAX
         MOV    EAX,[EAX].THash_CRC32.FCRC
         CALL   CRC32
         POP    EDX
         MOV    [EDX].THash_CRC32.FCRC,EAX
end;

procedure THash_CRC32.Done;
begin
  FCRC := not FCRC;
end;

class function THash_CRC16_CCITT.TestVector: Pointer;
asm
         MOV   EAX,OFFSET @Vector
         RET
@Vector: DB    0B0h,0D1h
end;

procedure THash_CRC16_CCITT.Init;
begin
  FCRC := $FFFF;
end;

procedure THash_CRC16_CCITT.Calc(const Data; DataSize: Integer);
asm
         OR     EDX,EDX
         JE     @Exit
         JCXZ   @Exit
         PUSH   EAX
         MOV    AX,[EAX].THash_CRC16_CCITT.FCRC
         PUSH   EBX
         XOR    EBX,EBX
@Start:
         MOV    BL,AH
         SHL    AX,8
         MOV    AL,[EDX]
         XOR    AX,CS:[EBX * 2 + OFFSET @CRC16]
         INC    EDX
         LOOP   @Start

         POP    EBX
         POP    EDX
         MOV    [EDX].THash_CRC16_CCITT.FCRC,AX
@Exit:   RET

@CRC16:  DW     00000h, 01021h, 02042h, 03063h, 04084h, 050A5h, 060C6h, 070E7h
         DW     08108h, 09129h, 0A14Ah, 0B16Bh, 0C18Ch, 0D1ADh, 0E1CEh, 0F1EFh
         DW     01231h, 00210h, 03273h, 02252h, 052B5h, 04294h, 072F7h, 062D6h
         DW     09339h, 08318h, 0B37Bh, 0A35Ah, 0D3BDh, 0C39Ch, 0F3FFh, 0E3DEh
         DW     02462h, 03443h, 00420h, 01401h, 064E6h, 074C7h, 044A4h, 05485h
         DW     0A56Ah, 0B54Bh, 08528h, 09509h, 0E5EEh, 0F5CFh, 0C5ACh, 0D58Dh
         DW     03653h, 02672h, 01611h, 00630h, 076D7h, 066F6h, 05695h, 046B4h
         DW     0B75Bh, 0A77Ah, 09719h, 08738h, 0F7DFh, 0E7FEh, 0D79Dh, 0C7BCh
         DW     048C4h, 058E5h, 06886h, 078A7h, 00840h, 01861h, 02802h, 03823h
         DW     0C9CCh, 0D9EDh, 0E98Eh, 0F9AFh, 08948h, 09969h, 0A90Ah, 0B92Bh
         DW     05AF5h, 04AD4h, 07AB7h, 06A96h, 01A71h, 00A50h, 03A33h, 02A12h
         DW     0DBFDh, 0CBDCh, 0FBBFh, 0EB9Eh, 09B79h, 08B58h, 0BB3Bh, 0AB1Ah
         DW     06CA6h, 07C87h, 04CE4h, 05CC5h, 02C22h, 03C03h, 00C60h, 01C41h
         DW     0EDAEh, 0FD8Fh, 0CDECh, 0DDCDh, 0AD2Ah, 0BD0Bh, 08D68h, 09D49h
         DW     07E97h, 06EB6h, 05ED5h, 04EF4h, 03E13h, 02E32h, 01E51h, 00E70h
         DW     0FF9Fh, 0EFBEh, 0DFDDh, 0CFFCh, 0BF1Bh, 0AF3Ah, 09F59h, 08F78h
         DW     09188h, 081A9h, 0B1CAh, 0A1EBh, 0D10Ch, 0C12Dh, 0F14Eh, 0E16Fh
         DW     01080h, 000A1h, 030C2h, 020E3h, 05004h, 04025h, 07046h, 06067h
         DW     083B9h, 09398h, 0A3FBh, 0B3DAh, 0C33Dh, 0D31Ch, 0E37Fh, 0F35Eh
         DW     002B1h, 01290h, 022F3h, 032D2h, 04235h, 05214h, 06277h, 07256h
         DW     0B5EAh, 0A5CBh, 095A8h, 08589h, 0F56Eh, 0E54Fh, 0D52Ch, 0C50Dh
         DW     034E2h, 024C3h, 014A0h, 00481h, 07466h, 06447h, 05424h, 04405h
         DW     0A7DBh, 0B7FAh, 08799h, 097B8h, 0E75Fh, 0F77Eh, 0C71Dh, 0D73Ch
         DW     026D3h, 036F2h, 00691h, 016B0h, 06657h, 07676h, 04615h, 05634h
         DW     0D94Ch, 0C96Dh, 0F90Eh, 0E92Fh, 099C8h, 089E9h, 0B98Ah, 0A9ABh
         DW     05844h, 04865h, 07806h, 06827h, 018C0h, 008E1h, 03882h, 028A3h
         DW     0CB7Dh, 0DB5Ch, 0EB3Fh, 0FB1Eh, 08BF9h, 09BD8h, 0ABBBh, 0BB9Ah
         DW     04A75h, 05A54h, 06A37h, 07A16h, 00AF1h, 01AD0h, 02AB3h, 03A92h
         DW     0FD2Eh, 0ED0Fh, 0DD6Ch, 0CD4Dh, 0BDAAh, 0AD8Bh, 09DE8h, 08DC9h
         DW     07C26h, 06C07h, 05C64h, 04C45h, 03CA2h, 02C83h, 01CE0h, 00CC1h
         DW     0EF1Fh, 0FF3Eh, 0CF5Dh, 0DF7Ch, 0AF9Bh, 0BFBAh, 08FD9h, 09FF8h
         DW     06E17h, 07E36h, 04E55h, 05E74h, 02E93h, 03EB2h, 00ED1h, 01EF0h
end;

class function THash_CRC16_Standard.TestVector: Pointer;
asm
         MOV   EAX,OFFSET @Vector
         RET
@Vector: DB    0EDh,075h
end;

procedure THash_CRC16_Standard.Calc(const Data; DataSize: Integer);
asm
         OR     EDX,EDX
         JE     @Exit
         JCXZ   @Exit
         PUSH   EAX
         MOV    AX,[EAX].THash_CRC16_Standard.FCRC
         PUSH   EBX
         XOR    EBX,EBX
@Start:
         MOV    BL,[EDX]
         XOR    BL,AL
         SHR    AX,8
         XOR    AX,CS:[EBX * 2 + OFFSET @CRC16]
         INC    EDX
         LOOP   @Start

         POP    EBX
         POP    EDX
         MOV    [EDX].THash_CRC16_Standard.FCRC,AX
@Exit:   RET

@CRC16:  DW     00000h, 0C0C1h, 0C181h, 00140h, 0C301h, 003C0h, 00280h, 0C241h
         DW     0C601h, 006C0h, 00780h, 0C741h, 00500h, 0C5C1h, 0C481h, 00440h
         DW     0CC01h, 00CC0h, 00D80h, 0CD41h, 00F00h, 0CFC1h, 0CE81h, 00E40h
         DW     00A00h, 0CAC1h, 0CB81h, 00B40h, 0C901h, 009C0h, 00880h, 0C841h
         DW     0D801h, 018C0h, 01980h, 0D941h, 01B00h, 0DBC1h, 0DA81h, 01A40h
         DW     01E00h, 0DEC1h, 0DF81h, 01F40h, 0DD01h, 01DC0h, 01C80h, 0DC41h
         DW     01400h, 0D4C1h, 0D581h, 01540h, 0D701h, 017C0h, 01680h, 0D641h
         DW     0D201h, 012C0h, 01380h, 0D341h, 01100h, 0D1C1h, 0D081h, 01040h
         DW     0F001h, 030C0h, 03180h, 0F141h, 03300h, 0F3C1h, 0F281h, 03240h
         DW     03600h, 0F6C1h, 0F781h, 03740h, 0F501h, 035C0h, 03480h, 0F441h
         DW     03C00h, 0FCC1h, 0FD81h, 03D40h, 0FF01h, 03FC0h, 03E80h, 0FE41h
         DW     0FA01h, 03AC0h, 03B80h, 0FB41h, 03900h, 0F9C1h, 0F881h, 03840h
         DW     02800h, 0E8C1h, 0E981h, 02940h, 0EB01h, 02BC0h, 02A80h, 0EA41h
         DW     0EE01h, 02EC0h, 02F80h, 0EF41h, 02D00h, 0EDC1h, 0EC81h, 02C40h
         DW     0E401h, 024C0h, 02580h, 0E541h, 02700h, 0E7C1h, 0E681h, 02640h
         DW     02200h, 0E2C1h, 0E381h, 02340h, 0E101h, 021C0h, 02080h, 0E041h
         DW     0A001h, 060C0h, 06180h, 0A141h, 06300h, 0A3C1h, 0A281h, 06240h
         DW     06600h, 0A6C1h, 0A781h, 06740h, 0A501h, 065C0h, 06480h, 0A441h
         DW     06C00h, 0ACC1h, 0AD81h, 06D40h, 0AF01h, 06FC0h, 06E80h, 0AE41h
         DW     0AA01h, 06AC0h, 06B80h, 0AB41h, 06900h, 0A9C1h, 0A881h, 06840h
         DW     07800h, 0B8C1h, 0B981h, 07940h, 0BB01h, 07BC0h, 07A80h, 0BA41h
         DW     0BE01h, 07EC0h, 07F80h, 0BF41h, 07D00h, 0BDC1h, 0BC81h, 07C40h
         DW     0B401h, 074C0h, 07580h, 0B541h, 07700h, 0B7C1h, 0B681h, 07640h
         DW     07200h, 0B2C1h, 0B381h, 07340h, 0B101h, 071C0h, 07080h, 0B041h
         DW     05000h, 090C1h, 09181h, 05140h, 09301h, 053C0h, 05280h, 09241h
         DW     09601h, 056C0h, 05780h, 09741h, 05500h, 095C1h, 09481h, 05440h
         DW     09C01h, 05CC0h, 05D80h, 09D41h, 05F00h, 09FC1h, 09E81h, 05E40h
         DW     05A00h, 09AC1h, 09B81h, 05B40h, 09901h, 059C0h, 05880h, 09841h
         DW     08801h, 048C0h, 04980h, 08941h, 04B00h, 08BC1h, 08A81h, 04A40h
         DW     04E00h, 08EC1h, 08F81h, 04F40h, 08D01h, 04DC0h, 04C80h, 08C41h
         DW     04400h, 084C1h, 08581h, 04540h, 08701h, 047C0h, 04680h, 08641h
         DW     08201h, 042C0h, 04380h, 08341h, 04100h, 081C1h, 08081h, 04040h
end;
{get the CPU Type from your system}
function GetCPUType: Integer; assembler;
asm
         PUSH   EBX
         PUSH   ECX
         PUSH   EDX
         MOV    EBX,ESP
         AND    ESP,0FFFFFFFCh
         PUSHFD
         PUSHFD
         POP    EAX
         MOV    ECX,EAX
         XOR    EAX,40000h
         PUSH   EAX
         POPFD
         PUSHFD
         POP    EAX
         XOR    EAX,ECX
         MOV    EAX,3
         JE     @Exit
         PUSHFD
         POP    EAX
         MOV    ECX,EAX
         XOR    EAX,200000h
         PUSH   EAX
         POPFD
         PUSHFD
         POP    EAX
         XOR    EAX,ECX
         MOV    EAX,4
         JE     @Exit
         PUSH   EBX
         MOV    EAX,1
         DB     0Fh,0A2h      //CPUID
         MOV    AL,AH
         AND    EAX,0Fh
         POP    EBX
@Exit:   POPFD
         MOV    ESP,EBX
         POP    EDX
         POP    ECX
         POP    EBX
end;

initialization
  FCPUType := GetCPUType;
  if FCPUType > 3 then
  begin
    SwapInteger := BSwapInt;
    SwapIntegerBuffer := BSwapIntBuf;
  end else
  begin
    SwapInteger := SwapInt;
    SwapIntegerBuffer := SwapIntBuf;
  end;
{this calculate a Checksum (CRC32) over the function CRC32 and the TestVector,
 if InitTestIsOk = False any modification from Testvector or CRC32() detected, :-) }
  InitTestIsOk := CRC32(CRC32($29524828, PChar(@CRC32) + 28, 1076), GetTestVector, 32) = $93C3557F;
finalization
end.

