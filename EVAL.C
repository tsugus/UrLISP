/*                                   */
/*       Evaluator & functions       */
/*                                   */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "LISP.H"
#define ec2 \
  if (err)  \
  goto Error
#define check_1_arg(x)             \
  if (!is(x, CELL))                \
    error("Not enough arguments"); \
  ec2
#define check_2_args(x)                  \
  if (!is(x, CELL) || !is(cdr(x), CELL)) \
    error("Not enough arguments");       \
  ec2

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

int hash_for_env(Index x)
{
  return x % ENV_ARRAY_SIZE;
}

/* Searches for a pair containing key in the array 'env_array',
   which is the global environment list. */
Index global_assoc(Index key)
{
  Index lst;

  if (!is(key, SYMBOL))
    return error("A key is not an symbol.");
  lst = env_array[hash_for_env(key)];
  for (; lst != Nil; lst = cdr(lst))
    if (key == car(car(lst)))
      return car(lst);
  error_(Num1, key);
  err = print_no_more;
  return Nil;
}

Index assoc(Index key, Index lst)
{
  if (!is(key, SYMBOL))
    return error("A key is not an symbol.");
  for (; lst != Nil; lst = cdr(lst))
    if (key == car(car(lst)))
      return car(lst);
  return global_assoc(key);
}

Index def(Index var, Index val)
{
  Index env;
  int hash_v;

  if (!is(var, SYMBOL))
    return error("A variable is not an symbol.");
  push(var);
  ec;
  hash_v = hash_for_env(var);
  env_array[hash_v] =
      cons(cons(var, val), env_array[hash_v]);
  ec;
  pop();
  return var;
}

Index setq(Index var, Index val, Index lst)
{
  Index pair;

  if (!is(var, SYMBOL))
    return error("A variable is not an symbol.");
  pair = assoc(var, lst);
  if (pair == Nil)
    return Nil;
  return cdr(rplacd(pair, val));
}

Index importenv(Index env)
{
  int i;

  for (i = 0; i < ENV_ARRAY_SIZE; i++)
    env_array[i] = Nil;
  push(env);
  ec;
  env = rev_append(env, Nil);
  ec;
  for (; env != Nil; env = cdr(env))
  {
    def(car(car(env)), cdr(car(env)));
    ec;
  }
  pop();
  return T;
}

Index exportenv()
{
  Index result;
  int i;

  result = Nil;
  for (i = ENV_ARRAY_SIZE - 1; i >= 0; i--)
  {
    result = append(env_array[i], result);
    ec;
  }
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

void dpush(Index x)
{
  if (++d_stack_p >= D_STACK_SIZE)
  {
    d_stack_p = -1;
    error("The data stack is overflowed.");
  }
  d_stack[d_stack_p] = x;
}

Index dpop()
{
  if (d_stack_p-- < 0)
  {
    d_stack_p = -1;
    return error("The data stack is empty.");
  }
  return d_stack[d_stack_p + 1];
}

void rpush(char x)
{
  if (++r_stack_p == R_STACK_SIZE)
  {
    r_stack_p = -1;
    error("The return stack is overflowed.");
  }
  r_stack[r_stack_p] = x;
}

char rpop()
{
  if (r_stack_p-- < 0)
  {
    r_stack_p = -1;
    return error("The return stack is empty.");
  }
  return r_stack[r_stack_p + 1];
}

Index eval(Index exp_, Index env_)
{
  d_stack_p = -1;
  r_stack_p = -1;
  dpush(env_);
  ec2;
  dpush(exp_);
  ec2;

/***** eval *****/
Eval:
  expr = dpop();
  env = dpop();
  if (expr == T)
    result = T;
  else if (expr == Nil)
    result = Nil;
  else if (atom(expr) == T)
    result = cdr(assoc(expr, env));
  else if (isSUBR(car(expr)) == T ||
           (atom(car(expr)) != T && car(car(expr)) == Lambda))
  {
    dpush(env); /* escape */
    ec2;
    dpush(expr); /* escape */
    ec2;
    dpush(env);
    ec2;
    dpush(cdr(expr));
    ec2;
    rpush('A');
    goto Evlis;
  A:
    indx = dpop();
    ec2;
    expr = dpop(); /* restore */
    ec2;
    env = dpop(); /* restore */
    ec2;
    dpush(env);
    ec2;
    dpush(indx);
    ec2;
    dpush(car(expr));
    ec2;
    goto Apply;
  }
  else
  {
    dpush(env);
    ec2;
    dpush(cdr(expr));
    ec2;
    dpush(car(expr));
    ec2;
    goto Apply;
  }
  goto Return;

/***** apply *****/
Apply:
  func = dpop();
  ec2;
  args = dpop();
  ec2;
  env = dpop();
  ec2;
  if (atom(func) == T)
  {
    switch (func)
    {
    case Quote:
      check_1_arg(args);
      result = car(args);
      break;
    case Atom:
      check_1_arg(args);
      if (atom(car(args)) == T)
        result = T;
      else
        result = Nil;
      break;
    case Eq:
      check_2_args(args);
      if (car(args) == car(cdr(args)))
        result = T;
      else
        result = Nil;
      break;
    case Car:
      check_1_arg(args);
      if (car(args) == Nil)
      {
        error("1st item is a nil.");
        goto Error;
      }
      else if (atom(car(args)) == T)
      {
        error("1st item is invalid.");
        goto Error;
      }
      else
        result = car(car(args));
      break;
    case Cdr:
      check_1_arg(args);
      if (car(args) == Nil)
      {
        error("1st item is a nil.");
        goto Error;
      }
      else if (atom(car(args)) == T)
      {
        error("1st item is invalid.");
        goto Error;
      }
      else
        result = cdr(car(args));
      break;
    case Cons:
      check_2_args(args);
      result = cons(car(args), car(cdr(args)));
      ec2;
      break;
    case Cond:
      check_1_arg(args);
      dpush(env);
      ec2;
      dpush(args);
      ec2;
      goto Evcon;
    case Rplaca:
      check_2_args(args);
      result = rplaca(car(args), car(cdr(args)));
      ec2;
      break;
    case Rplacd:
      check_2_args(args);
      result = rplacd(car(args), car(cdr(args)));
      ec2;
      break;
    case Eval:
      check_2_args(args);
      dpush(car(cdr(args)));
      ec2;
      dpush(car(args));
      ec2;
      goto Eval;
    case Def:
      check_2_args(args);
      dpush(env); /* escape */
      ec2;
      dpush(args); /* escape */
      ec2;
      dpush(env);
      ec2;
      dpush(car(cdr(args)));
      ec2;
      rpush('B');
      ec2;
      goto Eval;
    B:
      indx = dpop();
      ec2;
      args = dpop(); /* restore */
      ec2;
      env = dpop(); /* restore */
      ec2;
      result = def(car(args), indx);
      ec2;
      break;
    case Setq:
      check_2_args(args);
      dpush(env); /* escape */
      ec2;
      dpush(args); /* escape */
      ec2;
      dpush(env);
      ec2;
      dpush(car(cdr(args)));
      ec2;
      rpush('C');
      ec2;
      goto Eval;
    C:
      indx = dpop();
      ec2;
      args = dpop(); /* restore */
      ec2;
      env = dpop(); /* restore */
      ec2;
      result = setq(car(args), indx, env);
      ec2;
      break;
    case Begin:
      result = Nil;
      check_1_arg(args);
      indx = args;
    Loop:
      if (atom(indx) == T)
        break;
      dpush(env); /* escape */
      ec2;
      dpush(cdr(indx)); /* escape */
      ec2;
      dpush(env);
      ec2;
      dpush(car(indx));
      ec2;
      rpush('D');
      goto Eval;
    D:
      result = dpop();
      ec2;
      indx = dpop(); /* restore */
      ec2;
      env = dpop(); /* restore */
      ec2;
      goto Loop;
    case Error:
      check_2_args(args);
      result = error_(car(args), car(cdr(args)));
      break;
    case Num:
      check_1_arg(args);
      result = num(car(args));
      ec2;
      break;
    case Len:
      check_1_arg(args);
      result = len(car(args));
      ec2;
      break;
    case Gc:
      mark_and_sweep();
      result = Nil;
      break;
    case ImportEnv:
      check_1_arg(args);
      result = importenv(car(args));
      ec2;
      break;
    case ExportEnv:
      result = exportenv();
      ec2;
      break;
    case Quit:
      result = quit();
      break;
    case Cls:
      result = cls();
      break;
    case Read:
      result = gc_readS(1);
      ec2;
      break;
    case Display:
      check_1_arg(args);
      result = display(car(args));
      break;
    case Prompt:
      check_1_arg(args);
      result = promptt(car(args));
      break;
    case Verbose:
      check_1_arg(args);
      if (car(args) == Nil)
      {
        display_GC = 0;
        result = Nil;
        break;
      }
      else
      {
        display_GC = 1;
        result = T;
        break;
      }
    default:
      dpush(env);
      ec2;
      dpush(cons(cdr(assoc(func, env)), args));
      ec2;
      goto Eval;
    }
  }
  else if (car(func) == Label)
  {
    check_2_args(cdr(func));
    dpush(cons(cons(car(cdr(func)), car(cdr(cdr(func)))), env));
    ec2;
    dpush(cons(car(cdr(cdr(func))), args));
    ec2;
    goto Eval;
  }
  else if (car(func) == Lambda)
  {
    check_1_arg(cdr(func));
    dpush(pairlis(car(cdr(func)), args, env));
    ec2;
    dpush(car(cdr(cdr(func))));
    ec2;
    goto Eval;
  }
  else
  {
    error_(Num2, cons(func, args));
    err = print_no_more;
    goto Error;
  }
  goto Return;

/***** evcon *****/
Evcon:
  clauses = dpop();
  env = dpop();
  for (; clauses != Nil; clauses = cdr(clauses))
  {
    if (atom(clauses) == T)
    {
      error("Invalid clause");
      goto Error;
    }
    if (atom(car(clauses)) == T)
    {
      error("Invalid clause");
      goto Error;
    }
    dpush(env); /* escape */
    ec2;
    dpush(clauses); /* escape */
    ec2;
    dpush(env);
    ec2;
    dpush(car(car(clauses)));
    ec2;
    rpush('E');
    ec2;
    goto Eval;
  E:
    indx = dpop();
    ec2;
    clauses = dpop(); /* restore */
    ec2;
    env = dpop(); /* restore */
    ec2;
    if (indx != Nil)
    {
      dpush(env);
      ec2;
      dpush(car(cdr(car(clauses))));
      ec2;
      goto Eval;
    }
  }
  result = Nullchar; /* Meaning undefined. */
  goto Return;

/***** evlis *****/
Evlis:
  members = dpop();
  ec2;
  env = dpop();
  ec2;
  for (indx = Nil; members != Nil; members = cdr(members))
  {
    dpush(env); /* escape */
    ec2;
    dpush(members); /* escape */
    ec2;
    dpush(indx); /* escape */
    ec2;
    dpush(env);
    ec2;
    dpush(car(members));
    ec2;
    rpush('F');
    ec2;
    goto Eval;
  F:
    result = dpop();
    ec2;
    indx = dpop(); /* restore */
    ec2;
    indx = cons(result, indx);
    ec2;
    members = dpop(); /* restore */
    ec2;
    env = dpop(); /* restore */
    ec2;
  }
  result = rev_append(indx, Nil);
  goto Return;

Error:
  if (err == on)
  {
    print_error(expr, message);
    err = print_no_more;
  }
  return Nil;

Return:
  dpush(result);
  ec2;
  expr = env = result = func = args = clauses = members = indx = Nil;
  if (r_stack_p >= 0)
    switch (rpop())
    {
    case 'A':
      goto A;
    case 'B':
      goto B;
    case 'C':
      goto C;
    case 'D':
      goto D;
    case 'E':
      goto E;
    case 'F':
      goto F;
    }
  return dpop();
}
