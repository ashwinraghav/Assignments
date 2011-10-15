unit UiBlock;

interface

uses
  Sil;

const
  CDefPageSize = 4 * 1024;

type
  IPageManager = interface;
  IPageList = interface;
  IPage = interface;
  IBlockList = interface;
  IBlock = interface;
  IFreeBlock = interface;

  IPageManager = interface
    ['{C773E6A2-9B9C-48BB-92C8-56DA83B5ABDD}']
    function GetPages: IPageList;
    function Get(Size: Integer): Pointer;
    procedure Free(var Memory);
    property Pages: IPageList read GetPages;
  end;

  IPageList = interface
    ['{7144962F-B1ED-4E74-A41D-646A58F2B850}']
    function GetCount: Integer;
    function Enumerate(var Enum: IEnumerator; out Page: IPage): Boolean;
    function Find(Memory: Pointer; out Page: IPage): Boolean; overload;
    function Find(Size: Integer; out Page: IPage): Boolean; overload;
    function Get(Size: Integer): IPage;
    procedure Release(var Page: IPage);
    property Count: Integer read GetCount;
  end;

  IPage = interface
    ['{576B4EDD-A494-403D-B18C-423C6D146C03}']
    function GetAvail: Integer;
    function GetBlocks: IBlockList;
    function Alloc(Size: Integer): Pointer;
    procedure Free(var Memory);
    property Avail: Integer read GetAvail;
    property Blocks: IBlockList read GetBlocks;
  end;

  IBlockList = interface
    ['{2D835E2B-9ED1-4E50-B209-228D411A4B1F}']
    function GetCount: Integer;
    function GetFree: IFreeBlock;
    function Find(Size: Integer; out Block: IFreeBlock): Boolean;
    property Free: IFreeBlock read GetFree;
    property Count: Integer read GetCount;
  end;

  IBlock = interface
    ['{369BB112-2EE5-4F62-9513-C50D7BA99D34}']
    function GetSize: Integer;
    function GetAddress: Pointer;
    property Size: Integer read GetSize;
    property Address: Pointer read GetAddress;
  end;

  IFreeBlock = interface (IBlock)
    ['{F0B11B69-8332-47C4-AF5A-7CF8844ADFED}']
    function GetNext: IFreeBlock;
    property Next: IFreeBlock read GetNext;
  end;
  
implementation
end.
 