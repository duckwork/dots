" Vim Util Functions

function! util#fixLineEndings()
  update
  edit ++ff=dos
  setl ff=unix
  write
endfunction

function! util#removeEOLSpaces()
  let winview = winsaveview()
  let prevsearch = @/
  keepjumps silent! %s/\v\s+$//
  let @/ = prevsearch
  unlet prevsearch
  call winrestview(winview)
endfunction

function! util#updateModifiable()
  if !exists("b:setmodifiable")
    let b:setmodifiable = 0
  endif
  if &readonly
    if &modifiable
      setlocal nomodifiable
      let b:setmodifiable = 1
    endif
  else
    if b:setmodifiable
      setlocal modifiable
    endif
  endif
endfunction

function! util#unintelligent()
  let gdef_save  = &gdefault
  set nogdefault
  let oldsearch = @/
  let winview = winsaveview()
  keepjumps %sm/[‘’]/'/ge
  keepjumps %sm/[“”]/"/ge
  keepjumps %sm/—/---/ge
  keepjumps %sm/–/--/ge
  call winrestview(winview)
  let &gdefault = gdef_save
  let @/ = oldsearch
  unlet gdef_save oldsearch
endfunction

function! util#keepjumps(command) " keep jumps! LOLOLOL
  let winview = winsaveview()
  exe "keepjumps normal!" a:command
  call winrestview(winview)
endfunction
