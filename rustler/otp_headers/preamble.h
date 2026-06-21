typedef struct ErlNifEnv ErlNifEnv;
typedef struct ErlNifBinary ErlNifBinary;
typedef struct ErlNifMutex ErlNifMutex;
typedef struct ErlNifCond ErlNifCond;
typedef struct ErlNifRWLock ErlNifRWLock;
typedef struct ErlNifThreadOpts ErlNifThreadOpts;
typedef struct ErlNifSysInfo ErlNifSysInfo;
typedef struct ErlNifResourceType ErlNifResourceType;
typedef struct ErlNifResourceTypeInit ErlNifResourceTypeInit;
typedef struct ErlNifMapIterator ErlNifMapIterator;
typedef struct ErlNifMonitor ErlNifMonitor;
typedef struct ErlNifIOQueue ErlNifIOQueue;
typedef struct ErlNifIOVec ErlNifIOVec;
typedef struct SysIOVec SysIOVec;
typedef struct FILE FILE;
typedef int ErlNifTSDKey;
typedef int ErlNifTid;
typedef int ErlNifPid;
typedef int ErlNifPort;
typedef int ErlNifResourceFlags;
typedef int ErlNifMapIteratorEntry;
typedef int ErlNifEvent;
typedef int ErlNifHash;
typedef int ErlNifTime;
typedef int ErlNifTimeUnit;
typedef int ErlNifBinaryToTerm;
typedef int ErlNifUniqueInteger;
typedef int ErlNifSInt64;
typedef int ErlNifUInt64;
typedef int ErlNifOption;
typedef int ErlNifCharEncoding;
typedef int ErlNifTermType;
typedef int ErlNifIOQueueOpts;
typedef int va_list;
typedef unsigned long ERL_NIF_TERM;
typedef unsigned long size_t;
enum ErlNifSelectFlags { __RUSTLER_ERL_NIF_SELECT_FLAGS_DUMMY = 0 };
#define ERL_NAPI_ATTR_MALLOC_USD(...)
#define ERL_NAPI_ATTR_MALLOC_D(...)
#define ERL_NAPI_ATTR_ALLOC_SIZE(...)
#define ERL_NAPI_ATTR_MALLOC_US(...)
#define ERL_NAPI_ATTR_WUR

#define ERL_NIF_API_FUNC_DECL(ret, name, args) \
    ret name args
