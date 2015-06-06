#define CAML_NAME_SPACE
#define _GNU_SOURCE

#include <assert.h>
#include <string.h>
#include <sys/mman.h>
#include <dlfcn.h>
#include <stdio.h>
#include <caml/alloc.h>
#include <caml/callback.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/fail.h>

long magic_value = 1298379187928;

static void *named_value_code(const char *named_value)
{
    value *v = caml_named_value(named_value);
    assert(v);
    return (void *) Code_val(*v);
}

struct mapping
{
    long magic;
    size_t size;
    char code[];
};

value codeloader_create(value code, value arity)
{
    CAMLparam2(code, arity);
    CAMLlocal1 (result);
    
    if (Int_val(arity) < 1)
        caml_invalid_argument("arity < 1");

    size_t code_length = caml_string_length(code);
    size_t mapping_size = sizeof(struct mapping) + code_length;
    struct mapping *mapping = mmap(NULL, mapping_size, PROT_WRITE, MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);

    if (mapping == MAP_FAILED)
        caml_failwith("mmap failed");

    if (mapping == NULL || (((long) mapping) & 0x07) != 0)
    {
        munmap(mapping, mapping_size);
        caml_failwith("mmap bad alignment");
    }

    mapping->magic = magic_value;
    mapping->size = mapping_size;
    memcpy(mapping->code, String_val(code), code_length);

    if (mprotect(mapping, mapping->size, PROT_EXEC) != 0)
    {
        munmap(mapping, mapping->size);
        caml_failwith("mprotect");
    }

    result = caml_alloc_small(3, Closure_tag);

    Field(result, 1) = arity;
    Field(result, 2) = (value) mapping->code;

    if (Int_val(arity) == 1)
    {
        Field(result, 0) = (value) mapping->code;
    }
    else
    {
        char sym[30];
        int snl = snprintf(sym, sizeof(sym), "caml_curry%i", Int_val(arity));
        void *curry_fn;

        if (snl >= sizeof(sym))
            curry_fn = NULL;
        else
            curry_fn = dlsym(RTLD_DEFAULT, sym);

        if (curry_fn != NULL)
            Field(result, 0) = (value) curry_fn;
        else
            Field(result, 0) = (value) named_value_code("codeloader_fail_curry");
    }

    CAMLreturn(result);
}

value codeloader_free(value closure)
{
    CAMLparam1(closure);
    char *code = (char *) Field(closure, 2);
    if (code != 0)
    {
        struct mapping *mapping = (void *) (code - sizeof(mapping));
        assert(mapping->magic == magic_value);
        munmap(mapping, mapping->size);
        Store_field(closure, 2, (value) named_value_code("codeloader_fail_freed"));
    }
    CAMLreturn(Val_unit);
}
