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

syntax match	intTypes		"\v<u=int\d*>"

syntax cluster	Type			contains=basicTypes,standardTypes,intTypes

hi def link		basicTypes		Type
hi def link		intTypes		Type

" type

syntax keyword	structKeyword	struct skipwhite nextgroup=structDecl
syntax keyword	ifaceKeyword	struct skipwhite nextgroup=ifaceDecl
syntax region	structDecl		start="{" end="}"	contained transparent contains=@Type
syntax region	ifaceDecl		start="{" end="}"	contained transparent contains=@Type

hi def link		structKeyword	Keyword
hi def link		ifaceKeyword	Keyword

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
hi def link		funcCall		Identifier
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


syntax region	stringLit		start=:": end=:": oneline contains=@Escape
syntax region	stringRaw		start=:`: end=:`:

syntax cluster	String			contains=stringLit,stringRaw

hi def link		stringLit		String
hi def link		stringRaw		String

hi def link		String			Constant

syntax match	escapeSym		:\v\\[nrtvb0"]:	contained
syntax match	escapeX			"\v\\x\x{2}"	contained
syntax match	escapeO			"\v\\o\o{3}"	contained
syntax match	escapeU			"\v\\u\x{4}"	contained
syntax match	escapeU8		"\v\\U\x{8}"	contained

syntax cluster	Escape			contains=escapeSym,escapeX,escapeO,escapeU,escapeU8

hi def link		escapeSym		Preproc
hi def link		escapeX			Preproc
hi def link		escapeO			Preproc
hi def link		escapeU			Preproc
hi def link		escapeU8		Preproc

" comments

syntax region	commentol		start="//" end="$" skip="\n\s*//"		fold contains=todo
syntax region	comment			start="/\*" end="\*/"					fold contains=todo,commentedBlock

hi def link		comment			Comment
hi def link		commentol		Comment

" block

syntax region	commentedBlock	start="{" end="}"		fold contains=commentedBlock containedin=comment transparent
syntax region	block			start="{" end="}"		fold contains=block,Keyword,@Number,@String,@Type,comment,commentol,funcCall,preproc

" preproc

syntax region	preproc			start="//slow:" end="$"

hi def link		preproc			Preproc

" vim: sw=4 ts=4 noet
