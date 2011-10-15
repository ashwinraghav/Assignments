unit SilSeHolors;

interface

uses
  Sil;

type
  SliceType = record
    Size: Integer;
    Stride: Integer;
  end;

  MerateArray = array of Double;
  IndexArray = array of Integer;
  
  SliceArray = array of SliceType;

  HolorType = record
    Merates: MerateArray;
    Slicing: SliceArray;
  end;

  HolorArray = array of HolorType;

  Holor = class (Tool)
    class function Create(const Data: array of Double; const Slicing: SliceArray = nil): HolorType; overload;
    class function Create(const Data: array of Double; const Slicing: array of SliceType): HolorType; overload;
    class function Create(const Slicing: SliceArray): HolorType; overload;
    class function Create(const Scalar: Double): HolorType; overload;
    class function Create(const Holor: HolorType; CopyMerates: Boolean = False): HolorType; overload;
    class function Slice(const Holor: HolorType; const Slicing: array of SliceType): HolorType; overload;
    class function List(const Data: array of HolorType): HolorArray; overload;
    class function Merates(const Data: array of Double): MerateArray; overload;
    class function Merates(Size: Integer): MerateArray; overload;
    class function Slice(Size, Stride: Integer): SliceType; overload;
    class function Slices(const Data: array of SliceType): SliceArray; overload;
    class function Index(const Slicing: array of SliceType; const Index: array of Integer): Integer; overload;
    class function Indexes(Valence: Integer): IndexArray; overload;
    class function Indexes(const Slicing: SliceArray): IndexArray; overload;
    class function Indexes(const Holor: HolorType): IndexArray; overload;
    class function Check(const Index, Size: IndexArray): Boolean; overload;
    class function Check(const Index: IndexArray): Boolean; overload;
    class function Next(var Index: IndexArray; const Size: IndexArray): Boolean;
    class function Count(const Slicing: SliceArray): Integer; overload;
    class function Count(const Holor: HolorType): Integer; overload;
    class function Val(const Slicing: SliceArray): Integer; overload;
    class function Val(const Holor: HolorType): Integer; overload;
    class function Dim(const Slicing: SliceArray; const Index: Integer): Integer; overload;
    class function Dim(const Holor: HolorType; const Index: Integer): Integer; overload;
    class function Dim(const Slicing: SliceArray): IndexArray; overload;
    class function Dim(const Holor: HolorType): IndexArray; overload;
    class function IsScalar(const Holor: HolorType): Boolean;
    class function IsZero(const Holor: HolorType): Boolean;
    class function Value(const Holor: HolorType): Double; overload;
    class function Value(const Holor: HolorType; const Index: array of Integer): Double; overload;
    class procedure Value(const Holor: HolorType; const Index: array of Integer; const Value: Double); overload;
    class function Diagonal(const Slicing: array of SliceType; const Value: Double): HolorType; overload;
    class function Identity(const Slicing: array of SliceType): HolorType; overload;
    class function Zero(const Slicing: array of SliceType): HolorType; overload;
    class function Sum(const Holor1, Holor2: HolorType): HolorType; overload;
    class function Product(const Holor1, Holor2: HolorType): HolorType; overload;
    class function Product(const Scalar: Double; const Holor2: HolorType): HolorType; overload;
    class procedure Sum(var Result: HolorType; const Holor1, Holor2: HolorType); overload;
    class procedure Sum(var Result: HolorType; const Holors: array of HolorType); overload;
    class function Sum(const Holors: array of HolorType): HolorType; overload;
  end;

  Vector = class
    class function Create(const Data: array of Double): HolorType; overload;
    class function Sum(const Vector1, Vector2: HolorType): HolorType; overload; virtual;
    class procedure Sum(var Result: HolorType; const Vector1, Vector2: HolorType); overload; virtual;
    class procedure Sum(var Result: HolorType; const Vectors: array of HolorType); overload; virtual;
    class function Sum(const Vectors: array of HolorType): HolorType; overload; virtual;
  end;

  Matrix = class
    class function Create(const Data: array of Double): HolorType; overload;
    class function Sum(const Matrix1, Matrix2: HolorType): HolorType; overload;
    class procedure Sum(var Result: HolorType; const Matrix1, Matrix2: HolorType); overload; 
    class procedure Sum(var Result: HolorType; const Matrices: array of HolorType); overload;
    class function Sum(const Matrices: array of HolorType): HolorType; overload;
  end;

implementation

{ Holor }

class function Holor.Create(const Data: array of Double; const Slicing: SliceArray): HolorType;
begin
  Result.Merates := Merates(Data);
  if Assigned(Slicing) then
  begin
    if Count(Slicing) <> Length(Data) then
      raise Sil.Error.Create('Slicing debe ser del mismo orden que Data');
    Result.Slicing := Slicing;
  end else
    Result.Slicing := Slices([Slice(Length(Data), 1)]);
end;

class function Holor.Create(const Data: array of Double; const Slicing: array of SliceType): HolorType;
begin
  Result := Create(Data, Slicing);
end;

class function Holor.Create(const Slicing: SliceArray): HolorType;
begin
  Result.Merates := Merates(Count(Slicing));
  Result.Slicing := Slicing;
end;

class function Holor.Create(const Scalar: Double): HolorType;
begin
  Result.Merates := Merates([Scalar]);
  Result.Slicing := nil;
end;

class function Holor.Create(const Holor: HolorType; CopyMerates: Boolean): HolorType;
begin
  if CopyMerates then
    Result.Merates := Merates(Holor.Merates) else
    Result.Merates := Merates(Count(Holor.Slicing));
  Result.Slicing := Slices(Holor.Slicing);
end;

class function Holor.Slice(const Holor: HolorType; const Slicing: array of SliceType): HolorType;
begin
  Result.Merates := Merates(Holor.Merates);
  Result.Slicing := Slices(Slicing);
end;

class function Holor.List(const Data: array of HolorType): HolorArray;
var
  I: Integer;
begin
  SetLength(Result, Length(Data));
  for I := Low(Data) to High(Data) do
    Result[I] := Data[I];
end;

class function Holor.Merates(const Data: array of Double): MerateArray;
begin
  Result := Merates(Length(Data));
  System.Move(Data[0], Result[0], Length(Data) * System.SizeOf(Data[0]));
end;

class function Holor.Merates(Size: Integer): MerateArray;
begin
  SetLength(Result, Size);
end;

class function Holor.Slice(Size, Stride: Integer): SliceType;
begin
  Result.Size := Size;
  Result.Stride := Stride;
end;

class function Holor.Slices(const Data: array of SliceType): SliceArray;
begin
  SetLength(Result, Length(Data));
  System.Move(Data[0], Result[0], Length(Data) * System.SizeOf(Data[0]));
end;

class function Holor.Index(const Slicing: array of SliceType; const Index: array of Integer): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := Low(Index) to High(Index) do
    System.Inc(Result, Index[I] * Slicing[I].Stride);
end;

class function Holor.Indexes(Valence: Integer): IndexArray;
begin
  SetLength(Result, Valence);
end;

class function Holor.Indexes(const Slicing: SliceArray): IndexArray;
begin
  Result := Indexes(Val(Slicing));
end;

class function Holor.Indexes(const Holor: HolorType): IndexArray;
begin
  Result := Indexes(Holor.Slicing);
end;

class function Holor.Check(const Index, Size: IndexArray): Boolean;
var
  N: Integer;
begin
  Result := True;
  for N := Low(Index) to High(Index) do
  begin
    Result := Result and (Index[N] < Size[N]);
    if not Result then Break;
  end;
end;

class function Holor.Check(const Index: IndexArray): Boolean;
var
  N: Integer;
begin
  Result := False;
  for N := Low(Index) to High(Index) do
  begin
    Result := Result or (Index[N] > 0);
    if Result then Break;
  end;
end;

class function Holor.Next(var Index: IndexArray; const Size: IndexArray): Boolean;
var
  N: Integer;
  I: ^Integer;
begin
  Result := False;
  for N := Low(Index) to High(Index) do
  begin
    I := @Index[N];
    System.Inc(I^);
    Result := (I^ < Size[N]) or (N = High(Index));
    if not Result then
      I^ := 0 else
      Exit;
  end;
end;

class function Holor.Count(const Slicing: SliceArray): Integer;
var
  I: Integer;
begin
  Result := 1;
  for I := Low(Slicing) to High(Slicing) do
    Result := Result * Slicing[I].Size;
end;

class function Holor.Count(const Holor: HolorType): Integer;
begin
  Result := Count(Holor.Slicing);
end;

class function Holor.Val(const Slicing: SliceArray): Integer;
begin
  Result := System.Length(Slicing);
end;

class function Holor.Val(const Holor: HolorType): Integer;
begin
  Result := Val(Holor.Slicing);
end;

class function Holor.Dim(const Slicing: SliceArray; const Index: Integer): Integer;
begin
  Result := Slicing[Index].Size;
end;

class function Holor.Dim(const Holor: HolorType; const Index: Integer): Integer;
begin
  Result := Dim(Holor.Slicing, Index);
end;

class function Holor.Dim(const Slicing: SliceArray): IndexArray;
var
  I: Integer;
begin
  Result := Indexes(Slicing);
  for I := Low(Result) to High(Result) do
    Result[I] := Slicing[I].Size;
end;

class function Holor.Dim(const Holor: HolorType): IndexArray;
begin
  Result := Dim(Holor.Slicing);
end;

class function Holor.IsScalar(const Holor: HolorType): Boolean;
begin
  Result := not Assigned(Holor.Slicing);
end;

class function Holor.IsZero(const Holor: HolorType): Boolean;
begin
  Result := not Assigned(Holor.Merates);
end;

class function Holor.Value(const Holor: HolorType): Double;
begin
  Result := Holor.Merates[0];
end;

class function Holor.Value(const Holor: HolorType; const Index: array of Integer): Double;
var
  I: Integer;
begin
  with Holor do
  begin
    if Assigned(Merates) then
    begin
      if Assigned(Slicing) then
        I := Self.Index(Slicing, Index) else
        I := Index[0];
      Result := Merates[I];
    end else 
      Result := 0;
  end;
end;

class procedure Holor.Value(const Holor: HolorType; const Index: array of Integer; const Value: Double);
begin
  Holor.Merates[Self.Index(Holor.Slicing, Index)] := Value;
end;

class function Holor.Diagonal(const Slicing: array of SliceType; const Value: Double): HolorType;
var
  I, K: Integer;
begin
  Result := Create(Slices(Slicing));
  K := 0;
  for I := Low(Slicing) to High(Slicing) do
  begin
    System.Inc(K, I * Slicing[I].Stride);
    Result.Merates[K] := Value;
  end;
end;

class function Holor.Identity(const Slicing: array of SliceType): HolorType;
begin
  Result := Diagonal(Slicing, 1);
end;

class function Holor.Zero(const Slicing: array of SliceType): HolorType;
begin
  Result.Merates := nil;
  Result.Slicing := Slices(Slicing);
end;

class procedure Holor.Sum(var Result: HolorType; const Holor1, Holor2: HolorType);
var
  Index: IndexArray;
  Count: IndexArray;
  V1, V2, R: Double;
begin
  Index := Indexes(Result);
  Count := Dim(Result);
  while Check(Index, Count) do
  begin
    V1 := Value(Holor1, Index);
    V2 := Value(Holor2, Index);
    R := V1 + V2;
    Value(Result, Index, R);
    Next(Index, Count);
  end;
end;

class function Holor.Sum(const Holor1, Holor2: HolorType): HolorType;
begin
  Result := Create(Holor1);
  Sum(Result, Holor1, Holor2);
end;

class procedure Holor.Sum(var Result: HolorType; const Holors: array of HolorType);
var
  Index: IndexArray;
  Count: IndexArray;
  R: Double;
  I: Integer;
begin
  Index := Indexes(Result);
  Count := Dim(Result);
  while Check(Index, Count) do
  begin
    R := 0;
    for I := Low(Holors) to High(Holors) do
      R := R + Value(Holors[I], Index);
    Value(Result, Index, R);
    Next(Index, Count);
  end;
end;

class function Holor.Sum(const Holors: array of HolorType): HolorType;
begin
  Result := Create(Holors[0]);
  Sum(Result, Holors);
end;

class function Holor.Product(const Holor1, Holor2: HolorType): HolorType;
begin

end;

class function Holor.Product(const Scalar: Double; const Holor2: HolorType): HolorType;
begin

end;

{ Vector }

class function Vector.Create(const Data: array of Double): HolorType;
begin
  Result := Holor.Create(Data, nil);
end;

class procedure Vector.Sum(var Result: HolorType; const Vector1, Vector2: HolorType);
var
  I: Integer;
begin
  for I := Low(Result.Merates) to High(Result.Merates) do
    Result.Merates[I] := Vector1.Merates[I] + Vector2.Merates[I];
end;

class function Vector.Sum(const Vector1, Vector2: HolorType): HolorType;
begin
  Result := Holor.Create(Vector1);
  Sum(Result, Vector1, Vector2);
end;

class function Vector.Sum(const Vectors: array of HolorType): HolorType;
begin

end;

class procedure Vector.Sum(var Result: HolorType; const Vectors: array of HolorType);
var
  I, J: Integer;
begin
  for I := Low(Result.Merates) to High(Result.Merates) do
  begin
    Result.Merates[I] := 0;
    for J := Low(Vectors) to High(Vectors) do
      Result.Merates[I] := Result.Merates[I] + Vectors[J].Merates[I];
  end;
end;

{ Matrix }

class function Matrix.Create(const Data: array of Double): HolorType;
begin
  Result := Holor.Create(Data, nil);
end;

class procedure Matrix.Sum(var Result: HolorType; const Matrix1, Matrix2: HolorType);
begin

end;

class function Matrix.Sum(const Matrix1, Matrix2: HolorType): HolorType;
begin

end;

class function Matrix.Sum(const Matrices: array of HolorType): HolorType;
begin

end;

class procedure Matrix.Sum(var Result: HolorType; const Matrices: array of HolorType);
begin

end;

end.
