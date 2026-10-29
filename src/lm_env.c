#include "lm_env.h"
#include "keywords.h"
#include "lm_err.h"
#include "prims.h"
#include "types.h"
#include <assert.h>

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
      Err code = (check > 0) ? ERR_ARG_TYPE_MISMATCH : ERR_INVALID_ARG;
      return lm_err_bool (lm, code, "define");
    }

  return lm->env.current
             ? dict_insert (lm->env.current->env.dict, car->symbol.str, cdr)
             : false;
}

bool
lm_env_set (LM *lm, Cell *car, Cell *cdr)
{
  int check = lm_env_update_guard (car);
  if (check)
    {
      Err code = (check > 0) ? ERR_ARG_TYPE_MISMATCH : ERR_INVALID_ARG;
      return lm_err_bool (lm, code, "set!");
    }

  for (Cell *frame = lm->env.current; frame; frame = frame->env.parent)
    if (dict_has_key (frame->env.dict, car->symbol.str))
      return dict_insert (frame->env.dict, car->symbol.str, cdr);

  return lm_err_bool (lm, ERR_SYM_NOT_FOUND, car->symbol.str);
}

Cell *
lm_env_lookup (LM *lm, Cell *sym)
{
  Cell *res = keyword_lookup (sym->symbol.str, sym->symbol.len);
  if (res)
    return res;

  for (Cell *frame = lm->env.current; frame; frame = frame->env.parent)
    {
      assert (IS_INST (frame, ENV));
      DictEntity *entity = dict_lookup (frame->env.dict, sym->symbol.str);
      if (entity)
        return entity->val;
    }

  return lm_err_nil (lm, ERR_SYM_NOT_FOUND, sym->symbol.str);
}
