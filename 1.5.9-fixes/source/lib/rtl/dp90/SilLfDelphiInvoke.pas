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
      const Arguments: array of const;
            Return: PVariant = nil);

function GetProperty(
      const Instance: IUnknown;
      const Info: ITypeInterface;
      const PropertyName: string): Variant;

implementation

uses
  SilBtMem,
  SilBtError,
  SilBtTypeInfo,
  SilAfStackUtils,
  SilBeTypeInfo,
  SilBdTypeInfo,
  SilLfInvokeUtils,
  SilLkInvokeParam,
  SilLtInvokeParam;

resourcestring
  SErrInvokeIntfNotSupported    = 'La instancia no soporta la interface %s';
  SErrInvokeMethodNotFound      = 'La interface %s no posee el método %s';
  SErrInvokeWrongCallingConv    = 'Este runtime no soporta llamar metodos con convención %s. Sólo soporta StdCall.';
  SErrInvokeParamCountMismatch  = 'La cantidad de argumentos pasada (%d) no corresponde al la cantidad de argumentos (%d) del metodo (%s::%s) a invocar';
  SErrInvokePropertyNotFound    = 'No puede resolver el nombre de property %s a ningun método de la interface %s';  

type
  TSilParamObject = class(SilLkInvokeParam.TSilParamObject);

procedure DoStdcallInvoke(
    const Instance: IUnknown;
          Position: Integer;
    const Params: Pointer); stdcall;
asm
                      PUSH    EBX
                      
                      MOV     EBX, Params
                      MOV     EAX, RBufferHeader[EBX].Last      // toma el puntero al ultimo
                      TEST    EAX, EAX
                      JZ      @DoResult

@PushLoop:            MOV     ECX, TSilParamObject[EAX].FCount
@DoPush1:             DEC     ECX
                      JL      @DoNext
                      LEA     EDX, TSilParamObject[EAX].FData
                      PUSH    [EDX+4*ECX]
                      JMP     @DoPush1
@DoNext:              MOV     EAX, TSilParamObject[EAX].FPrev
                      TEST    EAX, EAX
                      JNZ     @PushLoop

@DoResult:            MOV     EAX, RBufferHeader[EBX].Result
                      TEST    EAX, EAX
                      JZ      @DoInvoke
                      MOV     ECX, TSilParamObject[EAX].FCount
@DoPush2:             DEC     ECX
                      JL      @DoInvoke
                      MOV     EDX, TSilParamObject[EAX].FBuffer
                      PUSH    [EDX+4*ECX]
                      JMP     @DoPush2

@DoInvoke:            MOV     ECX, Instance     // cargo Instance en ECX
                      MOV     EDX, Position
                      MOV     ECX, [ECX]        // ahora tomo el puntero al VMT
                      CALL    DWORD PTR [ECX+EDX*4]

                      MOV     EBX, RBufferHeader[EBX].Result
                      TEST    EBX, EBX
                      JZ      @SkipResult

                      PUSH    EDX
                      PUSH    EAX
                      PUSH    EBX
                      MOV     EBX, [EBX]
                      CALL    [EBX] + VMTOFFSET TSilParamObject.Assign
@SkipResult:
                      POP     EBX
end;

procedure InvokeMethod(
  const Instance: IUnknown;
  const Info: ITypeInterface;
  const MethodName: string;
  const Arguments: array of const;
        Return: PVariant);
var
  Temp: ITypeMethod;
  Method: ITypeInterfaceMethod;
  Intf: IUnknown;
  Buffer: Pointer;
  Value: Variant;
begin
  if Instance.QueryInterface(Info.GUID, Intf) <> 0 then
    raise Error.Create(SErrInvokeIntfNotSupported, [Info.Name]); //!TODO!

  if not Info.Methods.Find(MethodName, Temp) then
    raise Error.Create(SErrInvokeMethodNotFound, [MethodName]); //!TODO!

  Method := Temp as ITypeInterfaceMethod;

  if Method.CallingKind <> ckStdCall then
    raise Error.Create(SErrInvokeWrongCallingConv, [EnumTool.Name(TypeInfo(TCallingKind), Ord(Method.CallingKind), 'ck')]); //!TODO!

  if (Method.MethodKind = mkFunction) and not Assigned(Return) then
    Return := @Value;
  
  if Length(Arguments) <> Method.Params.Count - 1 then // restamos el Self
    raise Error.Create(SErrInvokeParamCountMismatch, [Length(Arguments), Method.Params.Count - 1, Info.Name, Method.Name]); //!TODO!
    
  Buffer := StackAlloc(Params.GetSize(Method));

  Params.Setup(Buffer, Intf, Method, Return, Arguments);
  try
    DoStdcallInvoke(Intf, Method.Position, Buffer);
  finally
    Params.Cleanup(Buffer);
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
      raise Error.Create(SErrInvokePropertyNotFound, [PropertyName, Info.Name]);

  InvokeMethod(Instance, Info, Item.Name, [], @Result);
end;

end.
