#include "lm_env.h"
#include "keywords.h"
#include "prims.h"
#include "types.h"

static int
lm_env_update_guard (Cell *x)
{
  if (!IS_INST (x, SYMBOL))
    return 1;

  if (keyword_lookup (x->symbol.str, x->symbol.len))
    return -1;

  return 0;
}

bool
lm_env_define (LM *lm, Cell *car, Cell *cdr)
{
  int check = lm_env_update_guard (car);
  if (check)
    {
      ErrorCode code = (check > 0) ? ERR_ARG_TYPE_MISMATCH : ERR_INVALID_ARG;
      return lm_err (lm, code, "define");
    }

  return (lm->env.sp >= 1)
             ? dict_insert (lm->env.dict[lm->env.sp - 1], car->symbol.str, cdr)
             : false;
}

bool
lm_env_set (LM *lm, Cell *car, Cell *cdr)
{
  int check = lm_env_update_guard (car);
  if (check)
    {
      ErrorCode code = (check > 0) ? ERR_ARG_TYPE_MISMATCH : ERR_INVALID_ARG;
      return lm_err (lm, code, "set!");
    }

  for (size_t i = lm->env.sp; i >= 1; --i)
    if (dict_has_key (lm->env.dict[i - 1], car->symbol.str))
      return dict_insert (lm->env.dict[i - 1], car->symbol.str, cdr);

  return lm_err (lm, ERR_SYMBOL_NOT_FOUND, car->symbol.str);
}

Cell *
lm_env_lookup (LM *lm, Cell *sym)
{
  Cell *res = keyword_lookup (sym->symbol.str, sym->symbol.len);
  if (res)
    return res;

  for (size_t i = lm->env.sp; i >= 1; --i)
    {
      DictEntity *entity = dict_lookup (lm->env.dict[i - 1], sym->symbol.str);
      if (entity)
        return entity->val;
    }

  lm_err (lm, ERR_SYMBOL_NOT_FOUND, sym->symbol.str);
  return NIL;
}
