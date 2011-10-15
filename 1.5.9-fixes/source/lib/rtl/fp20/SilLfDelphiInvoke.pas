{********************************************************************************
 *                  Standard Interface Library (SIL)                            *
 *                                                                              *
 *       General purpose library whose design is based in STRONG                *
 *   use of interfaces.                                                         *
 *                                                                              *
 *                                                                              *
 *     Copyright (C) 2000 Mariano Podestá    antiriad@gmail.com                 *
 *     Copyright (C) 2000 Leandro Conde      lconde@str.com.ar                  *
 *     Copyright (C) 2000 Lisandro Podestá   lisandrop@movi.com.ar              *
 *                                                                              *
 *     See License.txt for details.                                             *
 *                                                                              *
 *   This library is free software; you can redistribute it and/or              *
 *   modify it under the terms of the GNU Lesser General Public                 *
 *   License as published by the Free Software Foundation; either               *
 *   version 2.1 of the License, or (at your option) any later version.         *
 *                                                                              *
 *   This library is distributed in the hope that it will be useful,            *
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of             *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU          *
 *   Lesser General Public License for more details.                            *
 *                                                                              *
 *   You should have received a copy of the GNU Lesser General Public           *
 *   License along with this library; if not, write to the Free Software        *
 *   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA  *
 *                                                                              *
 ********************************************************************************}

unit SilLfDelphiInvoke;

{$I Defines.inc}

interface

uses
  SilLiTypeInfo;

procedure InvokeMethod(
      const Instance: IUnknown;
      const Info: ITypeInterface;
      const MethodName: string;
      const Args: array of const);

function GetProperty(
      const Instance: IUnknown;
      const Info: ITypeInterface;
      const PropertyName: string): Variant;

implementation

uses
  Sil,
  SilLfStackUtils,
  SilBeTypeInfo,
  SilBdTypeInfo,
  SilLfInvokeUtils,
  SilLkInvokeParam,
  SilLtInvokeParam;

type
  TParamObject = class(SilLkInvokeParam.TSilParamObject);

function DoStdcallInvoke(
    const Instance: IUnknown;
          Position: Integer;
    const Params: Pointer): Pointer; stdcall;
asm
                      PUSH    EBX
                      
                      MOV     EBX, Params
                      MOV     EAX, RBufferHeader[EBX].Last      // toma el puntero al ultimo
                      TEST    EAX, EAX
                      JZ      @DoResult

@PushLoop:            MOV     ECX, TParamObject[EAX].FCount
@DoPush1:             DEC     ECX
                      JL      @DoNext
                      LEA     EDX, TParamObject[EAX].FData
                      PUSH    [EDX+4*ECX]
                      JMP     @DoPush1
@DoNext:              MOV     EAX, TParamObject[EAX].FPrev
                      TEST    EAX, EAX
                      JNZ     @PushLoop

@DoResult:            MOV     EAX, RBufferHeader[EBX].Result
                      TEST    EAX, EAX
                      JZ      @DoInvoke
                      MOV     ECX, TParamObject[EAX].FCount
@DoPush2:             DEC     ECX
                      JL      @DoInvoke
                      LEA     EDX, TParamObject[EAX].FData
                      PUSH    [EDX+4*ECX]
                      JMP     @DoPush2

@DoInvoke:            MOV     ECX, Instance     // cargo Instance en EDI
                      MOV     EDX, Position
                      MOV     ECX, [ECX]        // ahora tomo el puntero al VMT
                      CALL    DWORD PTR [ECX+EDX*4]

                      POP     EBX
end;

procedure InvokeMethod(
  const Instance: IUnknown;
  const Info: ITypeInterface;
  const MethodName: string;
  const Args: array of const);
var
  Temp: ITypeMethod;
  Method: ITypeInterfaceMethod;
  Intf: IUnknown;
  Result: Pointer;
  Buffer: Pointer;
begin
  if Instance.QueryInterface(Info.GUID, Intf) <> 0 then
    raise Sil.Error.Create('la instancia no soporta la interface %s', [Info.Name]); //!TODO!

  if not Info.Methods.Find(MethodName, Temp) then
    raise Sil.Error.Create('no existe el metodo %s', [MethodName]); //!TODO!

  Method := Temp as ITypeInterfaceMethod;

  if Method.CallingKind <> ckStdCall then
    raise Sil.Error.Create('este runtime no soporta llamar metodos con convención %s', [Sil.Enum.Name(TypeInfo(TCallingKind), Ord(Method.CallingKind), 'ck')]); //!TODO!

  if    ((Method.MethodKind =  mkFunction) and (Length(Args) <> Method.Params.Count))
    or  ((Method.MethodKind <> mkFunction) and (Length(Args) <> Method.Params.Count - 1))  then
    raise Sil.Error.Create('la cantidad de argumentos no corresponde al metodo a invocar'); //!TODO!

  Buffer := StackAlloc(Params.GetSize(Method));
  Result := nil;
  
  Params.Setup(Buffer, Intf, Method, Args);
  try
    Result := DoStdcallInvoke(Intf, Method.Position, Buffer);
  finally
    Params.Cleanup(Buffer, True, Result);
  end;
  
end;

function GetProperty(
      const Instance: IUnknown;
      const Info: ITypeInterface;
      const PropertyName: string): Variant;
var
  Item: ITypeMethod;
begin

  if    not Info.Methods.Find('Get' + PropertyName, Item)
    and not Info.Methods.Find('DoGet' + PropertyName, Item)
    and not Info.Methods.Find('Get_' + PropertyName, Item)
    and not Info.Methods.Find('Do' + PropertyName, Item) then
      raise Sil.Error.Create('No puede resolver el nombre de property %s a ningun método de la interface %s', [PropertyName, Info.Name]);

  InvokeMethod(Instance, Info, Item.Name, [@Result]);
end;

end.
