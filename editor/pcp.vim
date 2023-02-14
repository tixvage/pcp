if exists("b:current_syntax")
  finish
endif

syntax keyword pcpKeywords if while const extern fn struct as return true false
syntax region pcpComment start="//" end="$"
syntax region pcpString start=/\v"/ skip=/\v\\./ end=/\v"/ contains=pcpEscapes
syntax match pcpEscapes display contained "\\[nrt\"']"
syntax match pcpNumber display "\<[0-9][0-9_]*\%([iu]\%(size\|8\|16\|32\|64\|128\)\)\="
syntax keyword pcpType i8 i16 i32 i64 u8 u16 u32 u64 void cstr auto bool va_arg

syntax match pcpOperator /[-+%<>!&;.|^*=]=\?/
syntax match pcpOperator /\/\%(=\|\ze[^/*]\)/
syntax match pcpOperator /:=\|||\|<-\|++\|--/

highlight default link pcpKeywords Keyword
highlight default link pcpComment Comment
highlight default link pcpString String
highlight default link pcpNumber Number
highlight default link pcpType Type
highlight default link pcpEscapes SpecialChar
highlight default link pcpOperator Operator

let b:current_syntax = "pcp"
