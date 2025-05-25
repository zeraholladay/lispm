#ifndef OOM_HANDLERS
#define OOM_HANDLERS

#define OOM_STRINGIFY_HELPER(x) #x
#define OOM_STRINGIFY(x) OOM_STRINGIFY_HELPER (x)
#define OOM_LOCATION "[" __FILE__ ":" OOM_STRINGIFY (__LINE__) "] "

typedef void (*oom_handler_t) (void *, const char *msg);

#endif
