unit SilSiMotor;

{$INCLUDE Defines.inc}

interface

uses
  Sil, SilSiModel;

(*             SilSmMotor
  dudas:
    1. Como definir una Rule que sea una simple operacion matematica entre Tags?
    2. Como asociar Tags con diferentes Layers? Es necesario?
    3. Es correcta la representacion de vectores y matrices?
    4. Algoritmos: deben estar en el Motor o en la logica externa?
         Quizá deba haber un repositorio de algoritmos tipicos, tale como
         - diferentes productos de matrices y vectores.
         - inversion de matrices.
         - solucion de ecuaciones.
         - etc.
    5. Sería util definir Tags sobre los mismos Tags? creo que no ...
    
  falta:
    1. Entidades: definir las interfaces para los Layers, Grupos y Coordenadas.
    2. Reglas: resolver como hacer para que la evaluacion de una regla vea los Tags
        de los objetos que necesite.
 *)

type
  IMotor = interface
    ['{77C22291-2E7D-4290-AABB-A98A8BF3DA17}']
    procedure Step(const Model: IModel);
  end;

implementation
end.
