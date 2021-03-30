#define CAML_NAME_SPACE
#include <caml/mlvalues.h>
#include <sys/resource.h>

CAMLprim value rlimit_no_file_c() {
  struct rlimit rlmt;
  getrlimit(RLIMIT_NOFILE, &rlmt);
  return Val_int(rlmt.rlim_cur);
}
