#include <stdint.h>
#include <stdio.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>

value zooph_set32(value vs, value vpos, value vi){
  CAMLparam3(vs, vpos, vi);
  char* buf = String_val(vs);
  int pos = Int_val(vpos);
  int32_t i = Int32_val(vi);

  char* buf_off = &buf[pos];
  int32_t* casted = (int32_t*)buf_off;
  casted[0] = i;
  CAMLreturn (Val_unit);
}

value zooph_get32(value vs,value vpos){
    CAMLparam2(vs,vpos);
    CAMLlocal1(result);
    char* buf = String_val(vs);
    int pos = Int_val(vpos);
    char* buf_off = &buf[pos];
    int32_t* casted = (int32_t*)buf_off;
    int32_t i32 = casted[0];
    result = caml_copy_int32(i32);
    CAMLreturn(result);
}
