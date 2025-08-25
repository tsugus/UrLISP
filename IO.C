/*                                   */
/*          Input / Output           */
/*                                   */

#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>
#include "LISP.H"

Index error(char *str)
{
  if (err == off)
  {
    err = on;
    message = str;
  }
  sp = 0;
  return Nil;
}

Index eof_error()
{
  err = eof;
  message = "EOF was entered.";
  sp = 0;
  return Nil;
}

Index gc_getFreeCell()
{
  Index indx;

  if (freecells != CELLS_SIZE - 1) /* The last cell is a freecell that is not used. */
  {
    indx = freecells;
    freecells = cdr(freecells);
    cdr(indx) = Nil;
  }
  else
  {
    mark_and_sweep();
    if (freecells == CELLS_SIZE - 1)
      return error("There are no cells available.");
    else
    {
      indx = freecells;
      freecells = cdr(freecells);
      cdr(indx) = Nil;
    }
  }
  return indx;
}

int isSeparator(char ch)
{
  switch (ch)
  {
  case '\0':
  case '(':
  case ')':
  case '[':
  case ']':
  case '.':
  case ';':
    return 1;
  default:
    if (isspace(ch))
      return 1;
    return 0;
  }
}

Index gc_strToName(char *str)
{
  Index indx, indx2;
  int i, j;

  indx = gc_getFreeCell();
  ec;
  push(indx);
  ec;
  indx2 = indx;
  j = 0;
  for (i = 0; str[i] != '\0'; i++)
  {
    if (j == sizeof(Index))
    {
      cdr(indx2) = gc_getFreeCell();
      ec;
      indx2 = cdr(indx2);
      j = 0;
    }
    (cas(indx2))[j++] = str[i];
  }
  if (j < sizeof(Index))
    for (i = j; i < sizeof(Index); i++)
      (cas(indx2))[i] = '\0';
  cdr(indx2) = Nil;
  pop();
  return indx;
}

void nameToStr(Index indx, char *str)
{
  int i = 0, j = 0;
  do
  {
    if (j == sizeof(Index))
    {
      indx = cdr(indx);
      if (indx == Nil)
        break;
      j = 0;
    }
    str[i] = (cas(indx))[j++];
  } while (str[i++] != '\0');
  str[i] = '\0';
}

Index gc_makeSymbol(char *str)
{
  Index cell;

  cell = gc_getFreeCell();
  ec;
  push(cell);
  ec;
  car(cell) = gc_strToName(str);
  ec;
  cdr(cell) = Nil;
  tag(cell) = SYMBOL;
  pop();
  return cell;
}

Index addSymbol(int hash_n, Index symbol)
{
  cdr(symbol) = symbol_table[hash_n];
  symbol_table[hash_n] = symbol;
  return symbol;
}

int hash(char *str)
{
  int hash_n, i;

  hash_n = 0;
  for (i = 0; str[i] != '\0'; i++)
    hash_n = (hash_n + str[i]) % SYMBOLTABLE_SIZE;
  return hash_n;
}

/* Find the symbol with the name in 'symbol_table[hash_n]'. */
Index findSymbol(int hash_n, char *name)
{
  Index symbol;
  char strbuf[TEXTBUF_SIZE];

  symbol = symbol_table[hash_n];
  while (symbol) /* If not found, returns Nil. */
  {
    nameToStr(car(symbol), strbuf);
    if (!strcmp(strbuf, name))
      return symbol;
    symbol = cdr(symbol);
  }
  return symbol;
}

Index gc_getSymbol()
{
  int i, hash_n;
  Index symbol;

  for (i = 0; !isSeparator(*txtp); i++)
    namebuf[i] = *(txtp++);
  namebuf[i] = '\0';
  if (!strcmp(namebuf, "nil"))
    return Nil;
  hash_n = hash(namebuf);
  symbol = findSymbol(hash_n, namebuf);
  if (symbol)
    return symbol;
  symbol = gc_makeSymbol(namebuf);
  ec;
  addSymbol(hash_n, symbol);
  return symbol;
}

void printSymbol(Index atom)
{
  switch (atom)
  {
  case Nil:
    printf("nil");
    return;
  case Nullchar:
    return;
  case Whitespace:
    putchar(' ');
    return;
  case Newline:
    putchar('\n');
    return;
  }
  nameToStr(car(atom), namebuf);
  printf("%s", namebuf);
}

Index gc_makeatom_sub(char *str)
{
  Index cell, cell2;
  char *txtp2;

  cell = gc_getFreeCell();
  ec;
  push(cell);
  ec;
  cell2 = gc_getFreeCell();
  ec;
  txtp2 = txtp;
  txtp = str; /* A trailing space required */
  car(cell) = gc_getSymbol();
  ec;
  txtp = ++txtp2;
  car(cell2) = gc_readS(Nil);
  ec;
  cdr(cell) = cell2;
  pop();
  return cell;
}

Index gc_makeAtom()
{
  if (*txtp == '\'') /* Shorthand */
    return gc_makeatom_sub("quote ");
  if (*txtp == '#' && *(txtp + 1) == '\'')
  {
    txtp++;
    return gc_makeatom_sub("function ");
  }
  if (*txtp == '#' && *(txtp + 1) == '=')
  {
    txtp++;
    return gc_makeatom_sub("len ");
  }
  if (*txtp == '#')
    return gc_makeatom_sub("num ");
  return gc_getSymbol();
}

void printAtom(Index indx)
{
  printSymbol(indx);
}

char *getstr()
{
  printf("%s ", prompt);
  txtp = textbuf;
  *txtp = '\0';
  return fgets(textbuf, TEXTBUF_SIZE, ifp);
}

int skipspace()
{
  for (;;)
  {
    while (isspace(*txtp))
      txtp++;
    if (*txtp != '\0' && *txtp != ';')
      return 1;
    if (getstr() == NULL)
      return 0;
    ec;
  }
}

Index gc_makeList(int from_top)
{
  Index indx, indx2, indx3;

  int super_bracket;
  if (*txtp++ == '[')
    super_bracket = 1;
  else
    super_bracket = 0;
  if (!skipspace())
    return eof_error();
  if (*txtp == ')')
  {
    txtp++;
    return Nil;
  }
  if (*txtp == ']')
  {
    if (super_bracket || from_top)
      txtp++;
    return Nil;
  }
  if (*txtp == '.')
    return error("There is no \"car\" of the dotted pair.");
  indx2 = indx = gc_getFreeCell();
  ec;
  push(indx);
  ec;
  car(indx2) = gc_readS(Nil);
  ec;
  if (!skipspace())
  {
    pop();
    return eof_error();
  }
  while (*txtp != ')' && *txtp != ']')
  {
    if (*txtp == '.')
    {
      txtp++;
      if (!skipspace())
      {
        pop();
        return eof_error();
      }
      if (*txtp == ')' || *txtp == ']')
      {
        pop();
        return error("There is no \"cdr\" of the dotted pair.");
      }
      cdr(indx2) = gc_readS(Nil);
      ec;
      if (!skipspace())
      {
        pop();
        return eof_error();
      }
      if (*txtp != ')' && *txtp != ']')
      {
        pop();
        return error("There is no ')' immediately after the dotted pair.");
      }
      break;
    }
    indx3 = gc_getFreeCell();
    ec;
    cdr(indx2) = indx3;
    car(indx3) = gc_readS(Nil);
    ec;
    indx2 = indx3;
    if (!skipspace())
    {
      pop();
      return eof_error();
    }
  }
  if (*txtp == ']')
    if (!super_bracket && !from_top)
    {
      pop();
      return indx;
    }
  txtp++;
  pop();
  return indx;
}

Index gc_readS(int from_top)
{
  if (!skipspace())
    return eof_error();
  else if (*txtp == '(' || *txtp == '[')
    return gc_makeList(from_top);
  else if (!isSeparator(*txtp))
    return gc_makeAtom();
  else if (*txtp++ == ']') /* ']' is treated specially because it is an auxiliary character for input. The opening parenthesis does not come here in the first place. */
    return gc_readS(from_top);
  else
    return error("Unexpected character.");
}

void printS(Index indx)
{
  if (!is(indx, CELL))
    printAtom(indx);
  else
  {
    putchar('(');
    for (;;)
    {
      printS(car(indx));
      indx = cdr(indx);
      if (!is(indx, CELL))
        break;
      putchar(' ');
    }
    if (indx)
    {
      printf(" . ");
      printAtom(indx);
    }
    putchar(')');
  }
}
