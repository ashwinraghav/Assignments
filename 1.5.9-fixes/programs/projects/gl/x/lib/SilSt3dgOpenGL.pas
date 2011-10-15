unit SilSt3dgOpenGL;

interface

uses
  Sil,
  SilSi3DGL;

type
  Transform = class(Tool)
    class function Contruct(const Matrix: I3dgMatrix): I3dgMatrixBuilder;
  end;

  Point = class(Tool)
    class procedure SlideX(const Step: Double);
		class procedure SlideY(const Step: Double);
		class procedure SlideZ(const Step: Double);
  end;

implementation

uses
  SilSm3dgOpenGL;

{ Transform }

class function Transform.Contruct(const Matrix: I3dgMatrix): I3dgMatrixBuilder;
begin
  Result := TSil3dgOpenglMatrixBuilder.Create(Matrix);
end;

{ Point }

class procedure Point.SlideX(const Step: Double);
begin

end;

class procedure Point.SlideY(const Step: Double);
begin

end;

class procedure Point.SlideZ(const Step: Double);
begin

end;

end.
