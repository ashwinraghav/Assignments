unit SilSfValues;

{$INCLUDE Defines.inc}

interface

function New(ByteSize: Integer): Pointer; overload;
function New(Value: Pointer): Pointer; overload;
procedure Release(var Value);
procedure Invalidate(Value: Pointer); overload;
function IsValid(Value: Pointer): Boolean;
function GetBool(Value: Pointer): Boolean;
procedure SetBool(Value: Pointer; Bool: Boolean);

implementation

uses
  Sil;

type
  PDataBlock = ^RDataBlock;
  PDataValue = ^RDataValue;
  PDataLink  = ^RDataLink;

  TDataFlag = (dfActive, dfTrue);
  TDataFlags = set of TDataFlag;

  RDataValue = record
    Words: Byte;        // 7 bits de tamaño en palabras (Bytes div 4)
    Offs: Byte;         // Offset desde el inicio
    Refs: Byte;         // refcount
    Flags: TDataFlags;  // indicadores: Activo/Historico, y True/False (p/ Boolean)
    Data: record end; 
  end;
  
  RDataBlock = record
    Next: PDataBlock;  // forma una lista en anillo
    First: PDataLink;   // apunta al primer valor libre
    Last: PDataLink;   // apunta al ultimo valor libre
    Words: Integer;
    Data: record end;  // elementos de este bloque
  end;

  RDataLink = record
    Next: PDataLink;    // el puntero está redondeado a direcciones alineadas a 8 bytes.
                        // los dos bits menos significativos se usan para saber el tamaño de link
    Words: Integer;      // este miembro está solo si esos dos bits valen 1
  end;

const
  CBlockSize        = 256;
  CMaxItemSize      = CBlockSize * 2 - 1; // 256 palabras de 4 bytes
  CBlockHeaderSize  = (SizeOf(RDataBlock) + 7) div 8;

function DoBlockNew(QuadSize: Integer): PDataBlock; forward;
function DoDataNew(Block: PDataBlock; WordSize: Integer = 0): PDataValue; forward;
procedure DoDataDelete(Data: PDataValue); forward;

var
  MBlockList: PDataBlock = nil;

{ public }

function New(ByteSize: Integer): Pointer;
var
  Ptr: ^PDataBlock;
  Block: PDataBlock;
  Item: PDataValue;
  Words: Integer;
begin
  Words := ((SizeOf(RDataValue) + ByteSize + 3) div 4); // redondeo a 4 bytes 

  if Words > CMaxItemSize then // máx: 256 * 8 = 512 * 4 = 2048
    raise Sil.Error.Create('No se pueden reservar más de %d byte a la vez', [CMaxItemSize]);

  Result := nil;
  Item := nil;
  Ptr := @MBlockList;
  
  repeat
  
    if not Assigned(Ptr^) then
      Ptr^ := DoBlockNew(CBlockSize);

    Block := Ptr^;

    if Block.Words >= Words then
      Item := DoDataNew(Block, Words);

    if Item <> nil then
      Result := @Item.Data else
      Ptr := @Block.Next;
    
  until Result <> nil;
end;

function New(Value: Pointer): Pointer;
var
  Ptr: PDataValue;
begin
  Ptr := Pointer(Integer(Value) - SizeOf(RDataValue));
  Inc(Ptr.Refs);
  Result := Value;
end;

procedure Release(var Value);
var
  Ptr: PDataValue;
begin
  Ptr := Pointer(Integer(Value) - SizeOf(RDataValue));

  Dec(Ptr.Refs);
  
  if Ptr.Refs = 0 then
    DoDataDelete(Ptr);

  Pointer(Value) :=  nil;
end;

procedure Invalidate(Value: Pointer);
var
  Ptr: PDataValue;
begin
  Ptr := Pointer(Integer(Value) - SizeOf(RDataValue));
  Exclude(Ptr.Flags, dfActive);  
end;

function IsValid(Value: Pointer): Boolean;
var
  Ptr: PDataValue;
begin
  Ptr := Pointer(Integer(Value) - SizeOf(RDataValue));
  Result := dfActive in Ptr.Flags;
end;                    

function GetBool(Value: Pointer): Boolean;
var
  Ptr: PDataValue;
begin
  Ptr := Pointer(Integer(Value) - SizeOf(RDataValue));
  Result := dfTrue in Ptr.Flags;
end;

procedure SetBool(Value: Pointer; Bool: Boolean);
var
  Ptr: PDataValue;
begin
  Ptr := Pointer(Integer(Value) - SizeOf(RDataValue));
  if Bool then
    Include(Ptr.Flags, dfTrue) else
    Exclude(Ptr.Flags, dfTrue);
end;

{ privates }

function DoBlockNew(QuadSize: Integer): PDataBlock;
begin
  GetMem(Result, SizeOf(RDataBlock) + QuadSize * 8);
  Result.Next := nil;
  Result.First := @Result.Data;
  Result.Last := nil;
  Result.First.Next := nil;
  Result.First.Words := QuadSize * 2;
  Integer(Result.First) := Integer(Result.First) or 3;
  Result.Words := QuadSize * 2; // en palabras de 4 bytes
end;

function DoDataNew(Block: PDataBlock; WordSize: Integer = 0): PDataValue;
var
  Mark: Integer;
  Words: Integer;
  Item: PDataLink;
  Link: ^PDataLink;
begin
  Link := @Block.First;
  Result := nil;

  while (Result = nil) and Assigned(Link^) do
  begin
    Mark := Integer(Link^);
    Item := Pointer(Mark and $FFFFFFFC); // obtengo el valor real del puntero
    Words := Integer(Mark and $00000003); // obtengo la marca del tamaño del espacio libre

    if Words = 3 then // marca es 3? (11b)
      Words := Item.Words; // tomo el size del campo siguiente

    if WordSize <= Words then // el espacio alcanza?
    begin
      Dec(Words, WordSize); // ok, este espacio sirve: descuento lo que quiero de su tamaño
      Dec(Block.Words, WordSize); // descuento lo que tomé del header del bloque 
        
      Result := Pointer(Item); // ahora: voy a devolver este, me acuerdo del puntero
      Result.Words := WordSize;  // inicializo el header: size en multiplos de 8
      Result.Offs := (Integer(Result) - Integer(@Block.Data)) div 4; // determino el offs al inicio del bloque
      Result.Refs := 1;    // refcount en uno y positivo es activo
      Result.Flags := [dfActive];


      if Words > 0 then // todavia queda espacio libre en el que tomé?
      begin
      
        Item := Pointer(Integer(Item) + Result.Words * 4); // ok, dejo el disponible: obtengo la dirección del resto
        Item.Next := nil; // inicializo el header del resto
        
        if Words > 2 then  // es un bloque grande?
        begin
          Item.Words := Words; // sip, guardo el size
          Words := 3;      // la marca de este bloque es 3
        end;
        
        Integer(Item) := Integer(Item) or Words; // armo el valor combinado del puntero y la marca
        
        Link^ := Item;  // asigno el nuevo espacio creado en el link del anterior

      end else // tomé lo último: desasigno el bloque de la lista de disponibles

        Link^ := Item.Next; // el ultimo asigna nil al Free del Block

    end else 
    
      Link := @Item.Next; // voy a por el próximo

  end;
end;

procedure DoDataDelete(Data: PDataValue);
var
  Block: PDataBlock;
  Link: PDataLink;
  Mark: Integer;
begin
  Block := Pointer(Integer(Data) - Data.Offs * 4 - SizeOf(RDataBlock));

  Mark := Data.Words;

  Link := Pointer(Data);
  if Mark > 2 then
  begin
    Link.Words := Mark;
    Mark := 3;
  end;

  Inc(Block.Words, Data.Words); 
  Link.Next := Block.First;
  Integer(Block.First) := Integer(Link) or Mark;
end;

end.
