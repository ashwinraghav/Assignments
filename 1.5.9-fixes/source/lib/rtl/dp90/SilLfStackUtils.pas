unit SilLfStackUtils;

interface

function StackGetTop: Pointer; register;
procedure StackSetTop(Value: Pointer); register;
function StackGetFrame: Pointer; register;
function StackAlloc(Size: LongWord): Pointer; register;
function StackPullup(Frame: Pointer): Pointer; register;
procedure StackReturn(Value, Frame: Pointer); register;

implementation

{$W-,R-,Q-}

function StackGetTop: Pointer; register;
asm
    mov   eax, esp
end;

function StackGetFrame: Pointer; register;
asm
    mov   eax, ebp
end;

procedure StackSetTop(Value: Pointer); register;
asm
    mov   esp, eax
end;

function StackAlloc(Size: LongWord): Pointer; register;
asm
    POP   EDX
    MOV   ECX,  EAX
    ADD   ECX,  3
    AND   ECX,  NOT 3
    SUB   ESP,  ECX
    SHR   ECX,  1
    SHR   ECX,  1
    MOV   EAX,  ESP
    PUSH  EDI
    MOV   EDI,  EAX
    XOR   EAX,  EAX
    CLD
    REP   STOSD
    POP   EDI
    MOV   EAX,  ESP
    JMP   EDX
end;

function StackPullup(Frame: Pointer): Pointer; register;
asm
    pop   [Frame]
    pop   eax
end;

procedure StackReturn(Value, Frame: Pointer); register;
asm
    mov   ebp, Frame
    jmp   eax
end;

end.
 