unit SilSeGlTypes;

interface

type
  P2iArray        = ^R2iArray;
  P2sArray        = ^R2sArray;
  P2dArray        = ^R2dArray;
  P3iArray        = ^R3iArray;
  P3sArray        = ^R3sArray;
  P4iArray        = ^R4iArray;
  P4sArray        = ^R4sArray;
  P4dArray        = ^R4dArray;
  P3dArray        = ^R3dArray;

  P2iPoint        = ^R2iPoint;
  P2sPoint        = ^R2sPoint;
  P2dPoint        = ^R2dPoint;
  P3iPoint        = ^R3iPoint;
  P3sPoint        = ^R3sPoint;
  P3dPoint        = ^R3dPoint;
  P4iPoint        = ^R4iPoint;
  P4sPoint        = ^R4sPoint;
  P4dPoint        = ^R4dPoint;

  P2iVector       = ^R2iVector;
  P2sVector       = ^R2sVector;
  P2dVector       = ^R2dVector;
  P3iVector       = ^R3iVector;
  P3sVector       = ^R3sVector;
  P3dVector       = ^R3dVector;
  P4iVector       = ^R4iVector;
  P4sVector       = ^R4sVector;
  P4dVector       = ^R4dVector;

  P2iRect         = ^R2IRect;


  RDegrees        = 0 .. 360;

  R2iArray        = packed array[0..1] of Integer;
  R2sArray        = packed array[0..1] of Single;
  R2dArray        = packed array[0..1] of Double;
  R3iArray        = packed array[0..2] of Integer;
  R3sArray        = packed array[0..2] of Single;
  R3dArray        = packed array[0..2] of Double;
  R4iArray        = packed array[0..3] of Integer;
  R4sArray        = packed array[0..3] of Single;
  R4dArray        = packed array[0..3] of Double;

  TGlColor        = type LongWord;

  R2iPoint        = packed record
    case Integer of
      0: (x: Integer; y: Integer;);
      1: (a: R2iArray);
  end;

  R2sPoint        = packed record
    case Integer of
      0: (x: Single; y: Single;);
      1: (a: R2sArray);
  end;

  R2dPoint        = packed record
    case Integer of
      0: (x: Double; y: Double;);
      1: (a: R2dArray);
  end;

  R3iPoint        = packed record
    case Integer of
      0: (x: Integer; y: Integer; z: Integer; );
      1: (a: R3iArray);
  end;

  R3sPoint        = packed record
    case Integer of
      0: (x: Single; y: Single; z: Single; );
      1: (a: R3sArray);
  end;

  R3dPoint        = packed record
    case Integer of
      0: (x: Double; y: Double; z: Double; );
      1: (a: R3dArray);
  end;

  R4iPoint        = packed record
    case Integer of
      0: (x: Integer; y: Integer; z: Integer; w: Integer; );
      1: (a: R4iArray);
  end;

  R4sPoint        = packed record
    case Integer of
      0: (x: Single; y: Single; z: Single; w: Single;  );
      1: (a: R4sArray);
  end;

  R4dPoint        = packed record
    case Integer of
      0: (x: Double; y: Double; z: Double; w: Double;  );
      1: (a: R4dArray);
  end;

  R2iVector       = type R2iPoint;
  R2sVector       = type R2sPoint;
  R2dVector       = type R2dPoint;
  R3iVector       = type R3iPoint;
  R3sVector       = type R3sPoint;
  R3dVector       = type R3dPoint;
  R4iVector       = type R4iPoint;
  R4sVector       = type R4sPoint;
  R4dVector       = type R4dPoint;

  R2iRect         = packed record
    case Integer of
      0: (  O: R2iPoint;
            S: R2iVector; );
      1: (  Ox: Integer;
            Oy: Integer;
            Sx: Integer;
            Sy: Integer; );
  end;

  R3dRect          = packed record
    case Integer of
      0: (  O: R3dPoint;
            S: R3dVector; );
      1: (  Ox: Double;
            Oy: Double;
            Oz: Double;
            Sx: Double;
            Sy: Double;
            Sz: Double; );
  end;

  R2iMatrix       = packed array[0..1, 0..1] of Integer;
  R2sMatrix       = packed array[0..1, 0..1] of Single;
  R2dMatrix       = packed array[0..1, 0..1] of Double;
  R3iMatrix       = packed array[0..2, 0..2] of Integer;
  R3sMatrix       = packed array[0..2, 0..2] of Single;
  R3dMatrix       = packed array[0..2, 0..2] of Double;
  R4iMatrix       = packed array[0..3, 0..3] of Integer;
  R4sMatrix       = packed array[0..3, 0..3] of Single;
  R4dMatrix       = packed array[0..3, 0..3] of Double;

function V(const X, Y, Z: Double): R3DVector; overload;
function P(const X, Y, Z: Double): R3DPoint; overload;

implementation

function V(const X, Y, Z: Double): R3DVector;
begin
  Result.X := X;
  Result.Y := Y;
  Result.Z := Z;
end;

function P(const X, Y, Z: Double): R3DPoint;
begin
  Result.X := X;
  Result.Y := Y;
  Result.Z := Z;
end;


end.
