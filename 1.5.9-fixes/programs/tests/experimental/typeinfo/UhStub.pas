unit UhStub;

interface

uses
  Sil,
  SilLiTypeInfo,
  UiStub;

type
  TOpcode = (
      opCall = $E8,
      opJump = $E9
    );

type
  RJmpCode = packed record
    Opcode: TOpcode;       // CALL:$E8 o JMP:$E9
    Offset: Integer;
  end;

type
  PMethodEntry = ^RMethodEntry;
  PMethodArray = ^RMethodArray;
  PInterfaceBlock = ^RInterfaceBlock;

  RInterfaceBlock = packed record
    VmtLink: Pointer;   // VmtLink o NextLink ...
    Pop: TOpcode;
    Code: RJmpCode;
    Padding: Word;
    Count: Integer;
    TypeInfo: ITypeInterface;
    Instance: RInterfaceStub;
    Methods: PMethodArray;
    Entries: record end;
    //Pointers: record end;  // array[0 .. Count - 1] of Pointer
    //Methods: record end;  // array[0 .. Count - 1] of RMethodEntry
  end;

  RMethodEntry = packed record
    Code: RJmpCode;
    Padding: array[0..2] of Byte;
    Method: RMethod;
  end;

  RMethodArray = array[0 .. High(Integer) div SizeOf(RMethodEntry) - 1] of RMethodEntry;

const
  CMethodEntrySize = SizeOf(RMethodEntry);

type
  PFreeBlock = ^RFreeBlock;
  RFreeBlock = packed record
    Next: PFreeBlock;
    Size: LongWord;
  end;

const
  CInterfaceBlockSize = SizeOf(RInterfaceBlock);

type
  PInterfaceList = ^TInterfaceList;
  TInterfaceList = array[0 .. 0] of RInterfaceBlock;

type
  PInterfacePage = ^RInterfacePage;
  RInterfacePage = packed record
    Next: PInterfacePage;
    FreeList: PFreeBlock;
    Count: Integer;
    Avail: LongWord;
    Interfaces: record end;
  end;

implementation
end.

