unit SeSHA256;

{$IFDEF FPC}
{$MODE delphi}
{$ENDIF}

interface

uses Sysutils, Classes;

type
  TSHA256HASH = array [0 .. 7] of Cardinal;
  PSHA256HASH = ^TSHA256HASH;

  // Calcula el hash SHA256 de una cadena de texto
function CalcSHA256(Msg: AnsiString): TSHA256HASH; overload;
// Calcula el hash SHA256 de un stream
function CalcSHA256(Stream: TStream): TSHA256HASH; overload;
// Convierte el hash en una cadena de texto
function SHA256ToStr(Hash: TSHA256HASH): String;

function SHA256ToBinaryStr(Hash: TSHA256HASH): ansistring;

implementation

type
  // Un bloque de datos de 64 bytes de longitud
  TChunk = array [0 .. 15] of Cardinal;
  PChunk = ^TChunk;

const
  k: array [0 .. 63] of Cardinal = ($428A2F98, $71374491, $B5C0FBCF, $E9B5DBA5,
    $3956C25B, $59F111F1, $923F82A4, $AB1C5ED5, $D807AA98, $12835B01, $243185BE,
    $550C7DC3, $72BE5D74, $80DEB1FE, $9BDC06A7, $C19BF174, $E49B69C1, $EFBE4786,
    $0FC19DC6, $240CA1CC, $2DE92C6F, $4A7484AA, $5CB0A9DC, $76F988DA, $983E5152,
    $A831C66D, $B00327C8, $BF597FC7, $C6E00BF3, $D5A79147, $06CA6351, $14292967,
    $27B70A85, $2E1B2138, $4D2C6DFC, $53380D13, $650A7354, $766A0ABB, $81C2C92E,
    $92722C85, $A2BFE8A1, $A81A664B, $C24B8B70, $C76C51A3, $D192E819, $D6990624,
    $F40E3585, $106AA070, $19A4C116, $1E376C08, $2748774C, $34B0BCB5, $391C0CB3,
    $4ED8AA4A, $5B9CCA4F, $682E6FF3, $748F82EE, $78A5636F, $84C87814, $8CC70208,
    $90BEFFFA, $A4506CEB, $BEF9A3F7, $C67178F2);

{$INCLUDE asm.inc}

  // Calcula el hash de un bloque
function CalcChunk(Hash: TSHA256HASH; var Chunk: TChunk): TSHA256HASH;
var
  i: Integer;
  s0, s1, maj, t1, t2, ch: Cardinal;
  w: array [0 .. 63] of Cardinal;
begin
  // Copiamos el bloque al comienzo de array "W"
  for i := 0 to 15 do
    w[i] := bswap(Chunk[i]);
  // Calculamos el resto de valores del array "W"
  for i := 16 to 63 do
  begin
    s0 := ror(w[i - 15], 7) xor ror(w[i - 15], 18) xor (w[i - 15] shr 3);
    s1 := ror(w[i - 2], 17) xor ror(w[i - 2], 19) xor (w[i - 2] shr 10);
    w[i] := w[i - 16] + s0 + w[i - 7] + s1;
  end;
  // Ahora hacemos las 64 "pasadas" sobre "W" para calcular el hash
  for i := 0 to 63 do
  begin
    s0 := ror(Hash[0], 2) xor ror(Hash[0], 13) xor ror(Hash[0], 22);
    maj := (Hash[0] and Hash[1]) xor (Hash[0] and Hash[2])
      xor (Hash[1] and Hash[2]);
    t2 := s0 + maj;
    s1 := ror(Hash[4], 6) xor ror(Hash[4], 11) xor ror(Hash[4], 25);
    ch := (Hash[4] and Hash[5]) xor ((not Hash[4]) and Hash[6]);
    t1 := Hash[7] + s1 + ch + k[i] + w[i];
    Hash[7] := Hash[6];
    Hash[6] := Hash[5];
    Hash[5] := Hash[4];
    Hash[4] := Hash[3] + t1;
    Hash[3] := Hash[2];
    Hash[2] := Hash[1];
    Hash[1] := Hash[0];
    Hash[0] := t1 + t2;
  end;
  // Devolvemos el hash de este bloque
  Result := Hash;
end;

// Calcula el hash SHA256 de una cadena de texto
function CalcSHA256(Msg: AnsiString): TSHA256HASH; overload;
var
  Stream: TMemoryStream;
begin
  Stream := TMemoryStream.Create;
  try
    // Guardamos el texto en un stream
    Stream.WriteBuffer(PAnsiChar(Msg)^, Length(Msg));
    Stream.Position := 0;
    // Calculamos el hash del stream
    Result := CalcSHA256(Stream);
  finally
    Stream.Free;
  end;
end;

// Calcula el hash SHA256 de un stream
function CalcSHA256(Stream: TStream): TSHA256HASH; overload;
var
  i, j, k: Integer;
  Size: int64;
  P: PAnsiChar;
  Chunk: PChunk;
  H: TSHA256HASH;
begin
  // Colocamos los valores iniciales
  Result[0] := $6A09E667;
  Result[1] := $BB67AE85;
  Result[2] := $3C6EF372;
  Result[3] := $A54FF53A;
  Result[4] := $510E527F;
  Result[5] := $9B05688C;
  Result[6] := $1F83D9AB;
  Result[7] := $5BE0CD19;
  Size := 0;
  // Reservamos espacio para 2 bloques
  GetMem(P, 64 * 2);
  try
    // Apuntamos al principio del buffer
    Chunk := PChunk(P);
    // Rellenamos el buffer con ceros
    FillChar(P^, 64 * 2, #0);
    // Leemos un bloque
    i := Stream.Read(P^, 64);
    // Mientras leemos bloques completos
    while i = 64 do
    begin
      // Calculamos el hash de este bloque
      H := CalcChunk(Result, Chunk^);
      // Y lo sumamos al hash anterior
      for k := 0 to 7 do
        Result[k] := Result[k] + H[k];
      // Calculamos el tamaño del stream
      inc(Size, i);
      // Rellenamos el buffer con ceros
      FillChar(P^, 64 * 2, #0);
      // Leemos el siguiente bloque
      i := Stream.Read(P^, 64);
    end;
    // Calculamos el tamaño del stream
    inc(Size, i);
    // Le añadimos un bit 1 al final
    P[i] := #$80;
    // Calculamos el tamaño de los datos que faltan
    j := i + 9;
    // Ajustamos el tamaño a un multiplo de 64
    if j mod 64 > 0 then
      inc(j, 64 - (j mod 64));
    // Guardmos el tamaño original en formato "big-endian"
    Size := swap64(Size * 8);
    move(Size, P[j - 8], 8);
    // Procesamos cada uno de los bloques de 64 bytes que faltan
    for i := 1 to j div 64 do
    begin
      // Calculamos el hash de este bloque
      H := CalcChunk(Result, Chunk^);
      // Y lo sumamos al hash anterior
      for k := 0 to 7 do
        Result[k] := Result[k] + H[k];
      // Apuntamos al siguiente bloque
      inc(Chunk);
    end;
  finally
    FreeMem(P);
  end;
end;

function SHA256ToStr(Hash: TSHA256HASH): String;
var
  i: Integer;
begin
  Result := EmptyStr;
  for i := 0 to 6 do
    Result := Result + IntToHex(Hash[i], 8);
  Result := Result + IntToHex(Hash[7], 8);
end;

function SHA256ToBinaryStr(Hash: TSHA256HASH): ansistring;
var
  i: Integer;
  temp: string;
begin
  temp := EmptyStr;
  for i := 0 to 7 do
    temp := temp + IntToHex(Hash[i], 8);

  Result := '';
  for i := 0 to 31 do
  begin
    Result := Result + ansichar
      (StrToInt('$' + temp[(i * 2) + 1] + temp[(i * 2) + 2]));
  end;

end;

end.
