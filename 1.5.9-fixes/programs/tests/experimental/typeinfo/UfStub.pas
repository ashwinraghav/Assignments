unit UfStub;

interface

uses
  SilBeTypeInfo,
  SilLiTypeInfo,
  UiStub;


function NewInterface(const TypeInfo: ITypeInterface; const Instance: RInterfaceStub): Pointer;

implementation

uses
  UhStub,
  Windows;

const
  CDefPageSize = 4096;

{$O+,W-,Q-,I-}


procedure IntfInvoke; register;
asm
    xchg      eax, [esp]
    add       eax, 3
    and       eax, not 3
    push      ecx
    xor       ecx, ecx
    mov       cx, RMethod[eax].Ordinal
    cmp       ecx, 3
    jg        @@Custom

@@IUnknown:
    or        ecx, ecx
    jz        @@QI
    mov       edx, RMethod[eax].Instance
    lea       ecx, [edx + ecx * type TMethod]
    push      TMethod[ecx].Data
    call      TMethod[ecx].Code
    pop       ecx
    pop       ecx //  call return
    ret       4
    
@@QI:

@@Custom:
    pop       ecx


@@Done:
    pop       ebx
    pop       ecx
    xchg      eax, [esp]
end;

function PageAlloc(Next: PInterfacePage; PageSize: LongWord = CDefPageSize): PInterfacePage; forward;
function BlockGet(BlockSize: LongWord): PInterfaceBlock; forward;

function CountMethods(const Info: ITypeInterface): Integer;
begin
  if Assigned(Info) then
    Result := Info.MethodCount + CountMethods(Info.ParentType) else
    Result := 0;
end;

function CalcJmpOffset(Src, Dest: Pointer): Longint;
begin
  Result := Longint(Dest) - (Longint(Src) + 5);
end;

function NewInterface(const TypeInfo: ITypeInterface; const Instance: RInterfaceStub): Pointer;
var
  Count: Integer;
  Size: LongWord;
  Block: PInterfaceBlock;
  P: PPointer;
  M: PMethodEntry;
  I: Integer;
begin
  Count := CountMethods(TypeInfo);

  Size := SizeOf(RInterfaceBlock) + Count * (SizeOf(RMethodEntry) + SizeOf(Pointer));

  Block := BlockGet(Size);
  if not Assigned(Block) then System.Error(reOutOfMemory);

  Block.Code.Opcode := opJump;
  Block.Code.Offset := CalcJmpOffset(@Block.Code.Opcode, @IntfInvoke);
  Block.Count := Count;
  Block.TypeInfo := TypeInfo;
  Block.VmtLink := @Block.Entries;
  Block.Instance := Instance;
  Block.Methods := PMethodArray(PChar(@Block.Entries) + Count * SizeOf(Pointer));
  
  P := Block.VmtLink;
  M := @Block.Methods[0];
  for I := 0 to Count - 1 do
  begin
    M.Code.Opcode := opCall;
    M.Code.Offset := CalcJmpOffset(@M.Code.Opcode, @Block.Code);
    M.Method.Ordinal := I;    
    M.Method.Offset := LongWord(M) - LongWord(Block);    
    M.Method.Instance := @Block.Instance;
    P^ := @M.Code;
    Inc(P);
    Inc(M);
  end;

  Result := Block;
end;

var
  MList: PInterfacePage = nil;

function PageGet(BlockSize: LongWord): PInterfacePage;
var
  P: ^PInterfacePage;
  Q: PInterfacePage;
begin
  Result := nil;
  P := @MList;
  while not Assigned(Result) and Assigned(P^) do
  begin
    Q := P^;
    if Q.Avail >= BlockSize then
      Result := Q;
    P := @P^.Next;
  end;
  if not Assigned(Result) then
  begin
    Result := PageAlloc(nil);
    P^ := Result;
  end;
end;


function PageAlloc(Next: PInterfacePage; PageSize: LongWord): PInterfacePage;
begin
  Result := Windows.VirtualAlloc(nil, PageSize, MEM_COMMIT, PAGE_EXECUTE_READWRITE);
  Result.Next := Next;
  Result.Avail := PageSize - SizeOf(Result^);
  Result.Count := 0;
  Result.FreeList := @Result.Interfaces;
  Result.FreeList.Next := nil;
  Result.FreeList.Size := Result.Avail;
end;


function BlockAlloc(Page: PInterfacePage; BlockSize: LongWord): PInterfaceBlock;
var
  P: ^PFreeBlock;
  R, S: PFreeBlock;
  Remain: LongWord;
begin
  Result := nil;

  P := @Page.FreeList;
  while not Assigned(Result) and Assigned(P^) do
  begin
    R := P^;
    if (R.Size >= BlockSize) then
    begin
      Remain := R.Size - BlockSize;
      if Remain > 0 then
      begin
        S := PFreeBlock(PChar(R) + BlockSize);
        S.Next := R.Next;
        S.Size := Remain;
      end else
        S := nil;
      P^ := S;
      Result := PInterfaceBlock(R);
      Inc(Page.Count);
      Dec(Page.Avail, BlockSize);
    end;
    P := @P^.Next;
  end;
end;


function BlockGet(BlockSize: LongWord): PInterfaceBlock;
var
  Page: PInterfacePage;
begin
  Page := PageGet(BlockSize);
  if Assigned(Page) then
    Result := BlockAlloc(Page, BlockSize) else
    Result := nil;
end;


(*)function NewInterface(const Instance: RInterfaceStub): IUnknown;
var
  Block: PInterfacePage;
  TotalSize: Integer;
  Entry: PInterfaceBlock;
begin
  TotalSize := Instance.Count * SizeOf(RMethodEntry) + SizeOf(RInterfaceBlock);

  if (MList = nil) or (MList.Avail < TotalSize) then
  begin
    Block := Windows.VirtualAlloc(nil, PageSize, MEM_COMMIT, PAGE_EXECUTE_READWRITE);
    Block.Next := MList;
    Block.Avail := PageSize - SizeOf(RInterfacePage);
    Block.Count := 0;
    Block.FreeList := @Block.Interfaces;
    MList := Block;
  end else
    Block := MList;

  Entry := Block.FreeList;
  Dec(Block.Avail, TotalSize);
  Inc(PByte(Block.FreeList), TotalSize);
  Inc(Block.Count);


  Result := InstFreeList;
  Instance := InstFreeList;
  InstFreeList := Instance^.Next;
  Instance^.Method := Method;
end;(*)

end.
