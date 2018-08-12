{$IFDEF i386}

  {$IFDEF FPC}
    {$ASMMODE intel}
  {$ENDIF}

  function ror(x: Cardinal; y: Byte): Cardinal; assembler;
  asm
    mov cl,dl
    ror eax,cl
  end;

  function rol(x: Cardinal; y: Byte): Cardinal; assembler;
  asm
    mov cl,dl
    rol eax,cl
  end;
 
  function bswap(x: Cardinal): Cardinal; assembler;
  asm
    bswap eax
  end;

  function swap64(x: int64): int64; assembler;
  asm
    mov edx,dword ptr[x]
    mov eax,dword ptr[x+4]
    bswap edx
    bswap eax
  end;
{$ELSE}
  function ror(x: Cardinal; y: Byte): Cardinal;
  begin
    ror:=
      (x shr y) +
      (x shl (32-y));
  end;

  function rol(x: Cardinal; y: Byte): Cardinal;
  begin
    rol:=
      (x shl y) +
      (x shr (32-y));
  end;

  function bswap(x: Cardinal): Cardinal;
  begin
    bswap:=
      ((x and $000000FF) shl 24) +
      ((x and $0000FF00) shl  8) +
      ((x and $00FF0000) shr  8) +
      ((x and $FF000000) shr 24);
  end;

  function swap64(x: uint64): uint64;
  begin
    swap64:=
      ((x and $00000000000000FF) shl 56) +
      ((x and $000000000000FF00) shl 40) +
      ((x and $0000000000FF0000) shl 24) +
      ((x and $00000000FF000000) shl 8) +
      ((x and $000000FF00000000) shr 8) +
      ((x and $0000FF0000000000) shr 24) +
      ((x and $00FF000000000000) shr 40) +
      ((x and $FF00000000000000) shr 56);
  end;
{$ENDIF}