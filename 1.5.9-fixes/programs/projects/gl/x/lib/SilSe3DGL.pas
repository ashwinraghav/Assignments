unit SilSe3DGL;

interface

type
  P2IArray        = ^P2IArray;
  P2SArray        = ^P2SArray;
  P2DArray        = ^P2DArray;
  P3IArray        = ^P3IArray;
  P3SArray        = ^P3SArray;
  P4IArray        = ^P4IArray;
  P4SArray        = ^P4SArray;
  P4DArray        = ^P4DArray;
  P3DArray        = ^P3DArray;

  P2IPoint        = ^R2IPoint;
  P2SPoint        = ^R2SPoint;
  P2DPoint        = ^R2DPoint;
  P3IPoint        = ^R3IPoint;
  P3SPoint        = ^R3SPoint;
  P3DPoint        = ^R3DPoint;
  P4IPoint        = ^R4IPoint;
  P4SPoint        = ^R4SPoint;
  P4DPoint        = ^R4DPoint;

  P2IVector       = ^R2IVector;
  P2SVector       = ^R2SVector;
  P2DVector       = ^R2DVector;
  P3IVector       = ^R3IVector;
  P3SVector       = ^R3SVector;
  P3DVector       = ^R3DVector;
  P4IVector       = ^R4IVector;
  P4SVector       = ^R4SVector;
  P4DVector       = ^R4DVector;

  P2IRect         = ^R2IRect;


  RDegrees        = 0 .. 360;

  R2IArray        = packed array[0..1] of Integer;
  R2SArray        = packed array[0..1] of Single;
  R2DArray        = packed array[0..1] of Double;
  R3IArray        = packed array[0..2] of Integer;
  R3SArray        = packed array[0..2] of Single;
  R3DArray        = packed array[0..2] of Double;
  R4IArray        = packed array[0..3] of Integer;
  R4SArray        = packed array[0..3] of Single;
  R4DArray        = packed array[0..3] of Double;

  TGLColor        = type LongWord;

  R2IPoint        = packed record
    case Integer of
      0: (x: Integer; y: Integer;);
      1: (a: R2IArray);
  end;

  R2SPoint        = packed record
    case Integer of
      0: (x: Single; y: Single;);
      1: (a: R2SArray);
  end;

  R2DPoint        = packed record
    case Integer of
      0: (x: Double; y: Double;);
      1: (a: R2DArray);
  end;

  R3IPoint        = packed record
    case Integer of
      0: (x: Integer; y: Integer; z: Integer; );
      1: (a: R3IArray);
  end;

  R3SPoint        = packed record
    case Integer of
      0: (x: Single; y: Single; z: Single; );
      1: (a: R3SArray);
  end;

  R3DPoint        = packed record
    case Integer of
      0: (x: Double; y: Double; z: Double; );
      1: (a: R3DArray);
  end;

  R4IPoint        = packed record
    case Integer of
      0: (x: Integer; y: Integer; z: Integer; w: Integer; );
      1: (a: R4IArray);
  end;

  R4SPoint        = packed record
    case Integer of
      0: (x: Single; y: Single; z: Single; w: Single;  );
      1: (a: R4SArray);
  end;

  R4DPoint        = packed record
    case Integer of
      0: (x: Double; y: Double; z: Double; w: Double;  );
      1: (a: R4DArray);
  end;

  R2IVector       = type R2IPoint;
  R2SVector       = type R2SPoint;
  R2DVector       = type R2DPoint;
  R3IVector       = type R3IPoint;
  R3SVector       = type R3SPoint;
  R3DVector       = type R3DPoint;
  R4IVector       = type R4IPoint;
  R4SVector       = type R4SPoint;
  R4DVector       = type R4DPoint;

  R2IRect         = packed record
    case Integer of
      0: (  Origin: R2IPoint;
            Size: R2IVector; );
      1: (  Ox: Integer;
            Oy: Integer;
            Sx: Integer; 
            Sy: Integer; );
  end;

  R3DRect          = packed record
    case Integer of
      0: (  Origin: R2DPoint;
            Size: R2DVector; );
      1: (  Ox: Double;
            Oy: Double;
            Oz: Double;
            Sx: Double;
            Sy: Double;
            Sz: Double; );
  end;

  RPerspective    = packed record
  end;

function Vector(const X, Y, Z: Double): R3DVector;
function Point(const X, Y, Z: Double): R3DPoint;

implementation

function Vector(const X, Y, Z: Double): R3DVector;
begin
  Result.X := X;
  Result.Y := Y;
  Result.Z := Z;
end;

function Point(const X, Y, Z: Double): R3DPoint;
begin
  Result.X := X;
  Result.Y := Y;
  Result.Z := Z;
end;


end.
