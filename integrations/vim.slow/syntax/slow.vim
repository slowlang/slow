" Vim syntax file
" Language: slow


if exists("b:current_syntax")
	finish
endif

let b:current_syntax = "slow"

syntax case match
set foldminlines=3 foldnestmax=3


" base

syntax keyword	Keyword			package if else for return var const type
syntax keyword	basicTypes		string byte rune bool
syntax keyword	standardTypes	context.Context
syntax keyword	todo			TODO NOTE containedin=comment

syntax match	intTypes		"\v<u?int\d*>"

hi def link		basicTypes		Type
hi def link		intTypes		Type

" func

syntax keyword	funcKeywordG	func skipwhite nextgroup=funcDecl,funcArgs
syntax keyword	funcKeywordF	func contained containedin=block skipwhite nextgroup=funcArgs
syntax region	funcBody		start="{" end="}"	contained skipwhite nextgroup=funcArgs transparent
syntax region	funcArgs		start="(" end=")"	contained skipwhite nextgroup=funcDecl,funcBody
syntax match	funcDecl		"\v\i+"				contained
syntax match	funcCall		"\v\i+\ze\s*\("		contained

hi def link		funcKeywordG	Keyword
hi def link		funcKeywordF	Keyword
hi def link		funcDecl		Identifier
hi def link		funcCall		Type
"hi def link		funcArgs		Comment

hi def link		funcCall		Type

" numbers

syntax match	decimalInt		"\v<(\d(\d|_)*)?\d>"
syntax match	binaryInt		"\v<0b[01_]*[01]>"
syntax match	octalInt		"\v<0o(\o|_)*\o>"
syntax match	hexInt			"\v<0x(\x|_)*\x>"

syntax cluster	Number			contains=decimalInt,binaryInt,octalInt,hexInt

hi def link		decimalInt		Number
hi def link		binaryInt		Number
hi def link		octalInt		Number
hi def link		hexInt			Number

hi def link		Number			Constant

" strungs

syntax region	stringLit		start=:": end=:": oneline
syntax region	stringRaw		start=:`: end=:`:

syntax cluster	String			contains=stringLit,stringRaw

hi def link		stringLit		String
hi def link		stringRaw		String

hi def link		String			Constant

" comments

syntax region	commentol		start="//" end="$" skip="\n\s*//"		fold contains=todo
syntax region	comment			start="/\*" end="\*/"					fold contains=todo,commentedBlock

hi def link		comment			Comment
hi def link		commentol		Comment

" block

syntax region	commentedBlock	start="{" end="}"		fold contains=commentedBlock containedin=comment transparent
syntax region	block			start="{" end="}"		fold contains=block,Keyword,@Number,@String,basicTypes,comment,commentol,funcCall

" preproc

syntax region	preproc			start="//slow:" end="$"

hi def link		preproc			Preproc

" vim: sw=4 ts=4 noet
