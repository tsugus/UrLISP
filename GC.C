/*                                   */
/*         Gabage Collection         */
/*                                   */

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "LISP.H"

Index push(Index indx)
{
  if (sp >= STACK_SIZE)
  {
    sp = 0;
    return error("The software stack overflowed.");
  }
  stack[sp++] = indx;
  return indx;
}

Index pop()
{
  Index indx;

  if (sp < 1)
    sp = 1;
  indx = stack[--sp];
  return indx;
}

void purseName(Index indx)
{
  for (;;)
  {
    tag(indx) = abs(tag(indx));
    if (!cdr(indx))
      return;
    indx = cdr(indx);
  }
}

Index purseSymbol(Index indx)
{
  tag(indx) = abs(tag(indx));
  purseName(car(indx));
  return indx;
}

Index purseAtom(Index indx)
{
  if (indx)
  {
    int hash_n;
    Index symbol;

    purseSymbol(indx);
    ec;
    nameToStr(car(indx), namebuf);
    hash_n = hash(namebuf);
    symbol = findSymbol(hash_n, namebuf);
    if (!symbol) /* If 'indx' is not in the symbol table */
      addSymbol(hash_n, indx);
  }
  return indx;
}

Index purseS(Index indx)
{
  if (!is(indx, CELL))
    purseAtom(indx);
  else
  {
    tag(indx) = abs(tag(indx));
    /* putchar('('); */
    for (;;)
    {
      if (tag(car(indx)) < 0)
        purseS(car(indx));
      indx = cdr(indx);
      tag(indx) = abs(tag(indx));
      if (!is(indx, CELL))
        break;
      /* putchar(' '); */
    }
    if (indx)
      /* { */
      /* printf(" . "); */
      purseAtom(indx);
    /* } */
    /* putchar(')'); */
  }
  return indx;
}

void mark_and_sweep()
{
  Index indx;
  int i;

  if (display_GC)
    printf(".");  /* A sign that GC has started */
  fflush(stdout); /* To display sequentially */
  /* Mark with a minus sign. */
  for (indx = 0; indx < CELLS_SIZE; indx++)
    tag(indx) = -abs(tag(indx));
  /* Cancel freeCells' mark. */
  for (indx = freecells; indx; indx = cdr(indx))
    tag(indx) = CELL;
  /* The symbol table is rebuilt. */
  for (i = 0; i < SYMBOLTABLE_SIZE; i++)
    symbol_table[i] = Nil;
  /* Scanning the S-expression, remove cells with a negative ID, and re-add the symbols to the empty table. */
  for (i = 0; i < ENV_ARRAY_SIZE; i++)
    purseS(env_array[i]);
  for (i = 0; i < Last; i++) /* 0 to 'Last'-1 are the indexes of reserved symbols */
    purseS(i);
  for (i = 0; i < sp; i++)
    purseS(stack[i]);
  for (i = 0; i <= d_stack_p; i++)
    purseS(d_stack[i]);
  purseS(expr);
  purseS(env);
  purseS(result);
  purseS(func);
  purseS(args);
  purseS(clauses);
  purseS(members);
  purseS(indx);
  /* Recycle cells with a negative tag ID to freecells. */
  for (indx = CELLS_SIZE - 2; indx > 0; indx--) /* The last cell is the "sentinel". */
  {
    if (tag(indx) < 0)
    {
      tag(indx) = CELL;
      car(indx) = Nil;
      cdr(indx) = freecells;
      freecells = indx;
    }
  }
}
