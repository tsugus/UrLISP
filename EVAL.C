/*                                   */
/*       Evaluator & functions       */
/*                                   */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "LISP.H"
#define check_1_arg(x)                    \
  if (!is(x, CELL))                       \
    return error("Not enough arguments"); \
  ec
#define check_2_args(x)                   \
  if (!is(x, CELL) || !is(cdr(x), CELL))  \
    return error("Not enough arguments"); \
  ec

void print_error(Index exp, char *msg)
{
  printf("%s\n", msg);
  printf("At ");
  printS(exp);
  putchar('\n');
  err = print_no_more;
}

Index error_(Index code, Index exp)
{
  switch (code)
  {
  case Num1:
    printf("Not found");
    break;
  case Num2:
    printf("Invalid form");
    break;
  default:
    printf("Error");
  }
  printf(": ");
  printS(exp);
  putchar('\n');
  return Nil;
}

Index atom(Index x)
{
  return (abs(tag(x)) == CELL ? Nil : T);
}

Index cons(Index x, Index y)
{
  Index z;

  push(x);
  ec;
  push(y);
  ec;
  z = gc_getFreeCell();
  ec;
  car(z) = x;
  cdr(z) = y;
  pop();
  pop();
  return z;
}

Index rplaca(Index x, Index y)
{
  if (!is(x, CELL))
    return error("the 1st argument is an atom.");
  car(x) = y;
  return x;
}

Index rplacd(Index x, Index y)
{
  if (!is(x, CELL))
    return error("the 1st argument is an atom.");
  cdr(x) = y;
  return x;
}

Index rev_append(Index x, Index y)
{
  push(x);
  ec;
  for (; x != Nil; x = cdr(x))
  {
    push(y);
    ec;
    y = cons(car(x), y);
    ec;
    pop();
  }
  pop();
  return y;
}

Index append(Index x, Index y)
{
  return rev_append(rev_append(x, Nil), y);
}

Index rev_pairs(Index keys, Index values)
{
  Index indx;

  if (keys == Nil)
    return Nil;
  indx = Nil;
  push(keys);
  ec;
  push(values);
  ec;
  while (atom(keys) == Nil && atom(values) == Nil)
  {
    if (!is(car(keys), SYMBOL))
      return error("A formal argument is not an symbol.");
    push(indx);
    ec;
    indx = cons(cons(car(keys), car(values)), indx);
    ec;
    keys = cdr(keys);
    values = cdr(values);
    pop();
  }
  if (keys != Nil)
  {
    push(indx);
    ec;
    indx = cons(cons(keys, values), indx);
    ec;
    pop();
  }
  pop();
  pop();
  return indx;
}

Index pairlis(Index keys, Index values, Index lst)
{
  return rev_append(rev_pairs(keys, values), lst);
}

Index assoc(Index key, Index lst)
{
  if (!is(key, SYMBOL))
    return error("A key is not an symbol.");
  for (; lst != Nil; lst = cdr(lst))
    if (key == car(car(lst)))
      return car(lst);
  error_(Num1, key);
  return Nil;
}

Index def(Index var, Index val)
{
  Index env;

  push(var);
  ec;
  env = cons(cons(var, val), environment);
  ec;
  if (env != Nil) /* A workaround for unquoted lambda expressions clearing the environment list. */
    environment = env;
  pop();
  return var;
}

Index setq(Index key, Index val, Index lst)
{
  if (!is(key, SYMBOL))
    return error("A key is not an symbol.");
  for (; lst != Nil; lst = cdr(lst))
    if (key == car(car(lst)))
      return cdr(rplacd(car(lst), val));
  error_(Num1, key);
  return Nil;
}

Index while_(Index cndt, Index bodies, Index env)
{
  Index Ss, result;

  push(cndt);
  ec;
  push(bodies);
  ec;
  push(env);
  ec;
  result = Nil;
  while (eval(cndt, env) != Nil)
  {
    for (Ss = bodies; Ss != Nil; Ss = cdr(Ss))
    {
      push(Ss);
      ec;
      result = eval(car(Ss), env);
      ec;
      pop();
    }
  }
  pop();
  pop();
  pop();
  return result;
}

Index dowhile(Index cndt, Index bodies, Index env)
{
  Index Ss, result;

  push(cndt);
  ec;
  push(bodies);
  ec;
  push(env);
  ec;
  do
  {
    for (Ss = bodies; Ss != Nil; Ss = cdr(Ss))
    {
      push(Ss);
      ec;
      result = eval(car(Ss), env);
      ec;
      pop();
    }
  } while (eval(cndt, env) != Nil);
  pop();
  pop();
  pop();
  return result;
}

Index num(Index arg)
{
  Index num, indx, result;
  int i;

  if (!is(arg, SYMBOL))
    return error("The argument isn't a symbol.");
  nameToStr(car(arg), namebuf);
  num = atoi(namebuf);
  result = Nil;
  for (i = 0; i < num; i++)
  {
    push(result);
    ec;
    indx = gc_getFreeCell();
    ec;
    car(indx) = T;
    cdr(indx) = result;
    result = indx;
    pop();
  }
  return result;
}

Index len(Index arg)
{
  Index indx;
  int i;

  if (is(arg, SYMBOL))
    return error("The argument is an atom.");
  for (i = 0; arg; arg = cdr(arg))
    if (i++ < 0)
      return error("Numeric overflow.");
  sprintf(namebuf, "%d", i);
  return gc_makeSymbol(namebuf);
}

Index quit()
{
  free(cells);
  free(tags);
  exit(0);
}

Index cls()
{
  printf("\033[2J");   /* Clear the screen. */
  printf("\033[0;0H"); /* Move the cursor to (0,0). */
  err = print_no_more; /* Do not display the return value. */
  return Nil;
}

Index display(Index indx)
{
  printS(indx);
  return indx;
}

Index promptt(Index atom)
{
  nameToStr(car(atom), namebuf);
  strcpy(prompt, namebuf);
  return T;
}

Index isSUBR(Index x)
{
  switch (x)
  {
  case Atom:
  case Eq:
  case Car:
  case Cdr:
  case Cons:
  case Rplaca:
  case Rplacd:
  case Eval:
  case Error:
  case Len:
  case ImportEnv:
  case Read:
  case Display:
  case Prompt:
  case Verbose:
    return T;
  default:
    return Nil;
  }
}

Index evcon(Index clauses, Index env)
{
  for (; clauses != Nil; clauses = cdr(clauses))
  {
    if (is(clauses, SYMBOL))
      return error("Invalid clause");
    if (is(car(clauses), SYMBOL))
      return error("Invalid clause");
    if (eval(car(car(clauses)), env) != Nil)
    {
      if (atom(cdr(car(clauses))) == T)
        return eval(car(car(clauses)), env);
      return eval(car(cdr(car(clauses))), env);
    }
  }
  return Nullchar; /* Meaning undefined. */
}

Index evlis(Index members, Index env)
{
  Index indx;

  for (indx = Nil; members != Nil; members = cdr(members))
  {
    push(indx);
    ec;
    indx = cons(eval(car(members), env), indx);
    ec;
    pop();
  }
  return rev_append(indx, Nil);
}

Index eval(Index exp, Index env)
{
  Index result;

  push(exp);
  ec;
  push(env);
  ec;
  if (exp == T)
    result = T;
  else if (exp == Nil)
    result = Nil;
  else if (atom(exp) == T)
    result = cdr(assoc(exp, env));
  else if (isSUBR(car(exp)) == T ||
           (atom(car(exp)) != T && car(car(exp)) == Lambda))
    result = apply(car(exp), evlis(cdr(exp), env), env);
  else
    result = apply(car(exp), cdr(exp), env);
  if (err == on)
  {
    print_error(exp, message);
    result = Nil;
  }
  pop();
  pop();
  return result;
}

Index apply(Index func, Index args, Index env)
{
  if (atom(func) == T)
  {
    switch (func)
    {
    case Quote:
      check_1_arg(args);
      return car(args);
    case Atom:
      check_1_arg(args);
      if (atom(car(args)) == T)
        return T;
      else
        return Nil;
    case Eq:
      check_2_args(args);
      if (car(args) == car(cdr(args)))
        return T;
      else
        return Nil;
    case Car:
      check_1_arg(args);
      if (car(args) == Nil)
        return error("1st item is a nil.");
      else if (atom(car(args)) == T)
        return error("1st item is invalid.");
      else
        return car(car(args));
    case Cdr:
      check_1_arg(args);
      if (car(args) == Nil)
        return error("1st item is a nil.");
      else if (atom(car(args)) == T)
        return error("1st item is invalid.");
      else
        return cdr(car(args));
    case Cons:
      check_2_args(args);
      return cons(car(args), car(cdr(args)));
    case Cond:
      check_1_arg(args);
      return evcon(args, env);
    case Rplaca:
      check_2_args(args);
      return rplaca(car(args), car(cdr(args)));
    case Rplacd:
      check_2_args(args);
      return rplacd(car(args), car(cdr(args)));
    case Eval:
      check_2_args(args);
      return eval(car(args), car(cdr(args)));
    case Def:
      check_2_args(args);
      return def(car(args), eval(car(cdr(args)), env));
    case Setq:
      check_2_args(args);
      return setq(car(args), eval(car(cdr(args)), env), env);
    case While:
      check_2_args(args);
      return while_(car(args), (cdr(args)), env);
    case DoWhile:
      check_2_args(args);
      return dowhile(car(args), cdr(args), env);
    case Error:
      check_2_args(args);
      return error_(car(args), car(cdr(args)));
    case Num:
      check_1_arg(args);
      return num(car(args));
    case Len:
      check_1_arg(args);
      return len(car(args));
    case Gc:
      mark_and_sweep();
      return Nil;
    case ImportEnv:
      check_1_arg(args);
      environment = car(args);
      return T;
    case ExportEnv:
      return environment;
    case Quit:
      return quit();
    case Cls:
      return cls();
    case Read:
      return gc_readS(1);
    case Display:
      check_1_arg(args);
      return display(car(args));
    case Prompt:
      check_1_arg(args);
      return promptt(car(args));
    case Verbose:
      check_1_arg(args);
      if (car(args) == Nil)
      {
        display_GC = 0;
        return Nil;
      }
      else
      {
        display_GC = 1;
        return T;
      }
    default:
      return eval(cons(cdr(assoc(func, env)), args), env);
    }
  }
  else if (car(func) == Label)
  {
    check_2_args(cdr(func));
    return eval(cons(car(cdr(cdr(func))), args),
                cons(cons(car(cdr(func)), car(cdr(cdr(func)))),
                     env));
  }
  else if (car(func) == Lambda)
  {
    check_1_arg(cdr(func));
    return eval(car(cdr(cdr(func))),
                pairlis(car(cdr(func)), args, env));
  }
  else
  {
    error_(Num2, cons(func, args));
    return Nil;
  }
}
