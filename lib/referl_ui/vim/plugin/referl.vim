" Vim global plugin for using RefactorErl
" 
" TODO: File reloading after save (not after refactorings)
"       Handling RefactorErl error messages
"       Position based rename function (ri support needed)
"       Position based refactorings
"
" Maintainer: Roland Király <kiralyroland@inf.elte.hu>
"
" License:
"   
" The  contents of this  file are  subject to  the Erlang  Public License,
" Version  1.1, (the  "License");  you may  not  use this  file except  in
" compliance  with the License.  You should  have received  a copy  of the
" Erlang  Public License  along  with this  software.  If not,  it can  be
" retrieved at http://plc.inf.elte.hu/erlang/
"
" Software  distributed under  the License  is distributed  on an  "AS IS"
" basis, WITHOUT  WARRANTY OF ANY  KIND, either expressed or  implied. See
" the License  for the specific language governing  rights and limitations
" under the License.
"
" The Original Code is RefactorErl.
"
" The Initial Developer of the  Original Code is Eötvös Loránd University.
" Portions created  by Eötvös  Loránd University are  Copyright 2009,
" Eötvös Loránd University. All Rights Reserved.

" Set plugin:
"
" Move this file into your vim plugin directory (linux:/usr/share/vim/vimcurrent/plugin/)
" Set the s:refpath variable
"
" Usage:
"
" VIM command
"
" RHelp     RefactorErl Help
" Refstart  Start RefactorErl
" Refstop   Stop RefactorErl
" Add       Add file
" Drop      Drop file
" Metric    Run metric query
" Sq        Run semantic query
" Renmod    Rename module
" Renheader Rename header file
" Renvar    Rename variable
" Renfun    Rename function
" Sqr       Run semantic query with file position
" Ls        Show files
"
" In GVim: use the RefactorErl menu
"

if exists("g:loaded_referl")
 finish
endif
let g:loaded_referl = 1

let s:save_cpo = &cpo
set cpo&vim

if !hasmapto('<Plug>RefactorErl')
  map <unique> <Leader>a <Plug>RefactorErl
endif

let s:refpath = '?/tool/bin/RefactorErl '

noremenu  <silent> RefactorErl.RefactorErl\ help :call <SID>help()<CR>

noremenu  RefactorErl.-Sep1- :

noremenu  <silent> RefactorErl.RefactorErl\ start :call <SID>refsystem('start')<CR>
noremenu  <silent> RefactorErl.RefactorErl\ stop :call <SID>refsystem('stop')<CR>

noremenu  RefactorErl.-Sep1- :

noremenu  <silent> RefactorErl.Add\ File :call <SID>addfile()<CR>
noremenu  <silent> RefactorErl.Drop\ File :call <SID>dropfile()<CR>
noremenu  <silent> RefactorErl.Show\ Database :call <SID>show()<CR>
noremenu  <silent> RefactorErl.Draw\ Graph :call <SID>drgph()<CR>

"noremenu  <silent> RefactorErl.Function.Extract\ Function 
                           \ :call <SID>referl('extractfun', winline(), wincol())<CR>
"noremenu  <silent> RefactorErl.Function.Reorder\ Function\ 
                     \ Parameters :call <SID>referl('reorder', winline(), wincol())<CR>

noremenu  RefactorErl.-Sep2- :
noremenu  <silent> RefactorErl.Rename.Rename\ Variable :call <SID>renvar()<CR>
noremenu  <silent> RefactorErl.Rename.Rename\ Function :call <SID>renfun()<CR>
noremenu  <silent> RefactorErl.Rename.Rename\ Module :call <SID>renamefile('renmod')<CR>
noremenu  <silent> RefactorErl.Rename.Rename\ Header\ File 
                           \ :call <SID>renamefile('renheader')<CR>
"noremenu  <silent> RefactorErl.Rename.Rename\ Macro 
                           \ :call <SID>referl('renamemac',winline(), wincol())<CR>
"noremenu  <silent> RefactorErl.Rename.Rename\ Record 
                           \ :call <SID>referl('renamerec',winline(), wincol())<CR>
"noremenu  <silent> RefactorErl.Rename.Rename\ Record\ Filed 
                           \ :call <SID>referl('renamerecfiled',winline(), wincol())<CR>


noremenu  <silent> RefactorErl.Semantic\ Query.Metric\ Query :call <SID>query('metric')<CR>
noremenu  RefactorErl.Semantic\ Query.-Sep3- :
noremenu  <silent> RefactorErl.Semantic\ Query.SQ\ Query\-Command :call <SID>query('q')<CR>
noremenu  <silent> RefactorErl.Semantic\ Query.SQ\ Query :call <SID>paramsq()<CR>

command  RHelp call s:help()
command  Refstart call s:refsystem('start')
command  Refstop call s:refsystem('stop')
command  Add call s:addfile()
command  Drop call s:dropfile()
command  Metric call s:query('metric')
command  Sq call s:query('q')
command  Renmod call s:renamefile('renmod')
command  Renheader call s:renamefile('renheader')
command  Renvar call s:renvar()
command  Renfun call s:renfun()
command  Sqr call s:paramsq()
command  Ls call s:show()

let s:winop = 0

function s:help()
  if s:winop == 1 
     execute ":confirm close"
  endif
  let s:winop = 1
  botright new 
  set buftype=nofile bufhidden=wipe nobuflisted noswapfile nowrap
  call setline(2, 'Refstart  Start RefactorErl')
  call setline(3, 'Refstop   Stop RefactorErl')
  call setline(4, 'Add       Add file')
  call setline(5, 'Drop      Drop file')
  call setline(6, 'Metric    Run metric query')
  call setline(7, 'Sq        Run semantic query')
  call setline(8, 'Renmod    Rename module')
  call setline(9, 'Renheader Rename header file')
  call setline(10, 'Renvar    Rename variable')
  call setline(11, 'Renfun    Rename function')
  call setline(12, 'Sqr       Run semantic query with file position')
  call setline(13, 'Ls        Show files')
  1 
endfunction

function s:refsystem(fun)
      let stcmd = s:refpath.a:fun
      execute '!'.stcmd
      1
endfunction

function s:addfile()
      let stcmd = s:refpath." ri add \"".expand("%:p")."\""
      execute '!'.stcmd
      1
endfunction

function s:dropfile()
      let stcmd = s:refpath." ri drop \"".expand("%:p")."\""
      execute '!'.stcmd
      1
endfunction

function s:renamefile(filetype)
      let fn = bufname(bufnr("%"))
      let oldname = s:shname(fn)
      let path = s:bfnpath(fn) 
      let newname = input("Type the new name :")
      let stcmd = s:refpath." ri ".a:filetype." ".oldname." ".newname
      execute '!'.stcmd
      let newfile = path.newname.".erl" 
      call s:refreshFileWindow(bufnr("%"), newfile)
      1     
endfunction

function s:renvar()
      let line = winline()
      let col = wincol()
      let newname = input("Type the new name :")
      let stcmd = s:refpath." ri renvar ".expand("%:p")." \"{".line.",".col."}\" ".newname
      execute '!'.stcmd
      call s:refreshFileWindow(bufnr("%"), bufname(bufnr("%")))
      1
endfunction

function s:renfun()
      let funname = input("Type funname :")
      let arity = input("Type arity :")
      let newname = input("Type the new name :")
      let stcmd = s:refpath." ri renfun ".expand("%:p")." \"{".funname.",".arity."}\" ".newname
      execute '!'.stcmd
      call s:refreshFileWindow(bufnr("%"), bufname(bufnr("%")))
      1
endfunction

function s:referl(refactoring, line, col)
      let cbf = expand("%:p")
      let stcmd = s:refpath." ".a:refactoring." ".cbf." ".a:line.":".a:col
      "execute '!'.stcmd
      echo stcmd
      call s:refreshFileWindow(cbf)
      1
endfunction

function s:paramsq()     
  let cbf = expand("%:p")
  let querystr = input("Type the query :")
  let shellcmd = s:refpath." refusr_sq run \"[{positions, linecol}, 
      \ {output, stdio}]\" \"[{file,\\\"".cbf."\\\"}, 
      \ {position, ".winline()."}]\" \" ".querystr."\""
   let bnum = bufnr('referl')
   if bufexists(bnum)
        execute ':buffer '.bnum
        execute ':bd'
   endif
   execute ':botright  new '.'referl'
   set buftype=nofile noswapfile nowrap
  call setline(1, 'Referl Command:  ' . querystr)
  call append(line('$'), substitute(getline(2), '.', '=', 'g'))
  silent execute '$read !'. shellcmd
  1  
endfunction

function s:bfnpath(bfname)
      let lastSlash = strridx(a:bfname, '/')
      return strpart(a:bfname, 0, lastSlash+1)    
endfunction

function s:shname(bfname)
      let lastSlash = strridx(a:bfname, '/')
      let shrtname = strpart(a:bfname, lastSlash+1, strlen(a:bfname))  
      let dotp = strridx(shrtname, '.') 
      return strpart(shrtname, 0, dotp) 
endfunction

function s:refreshFileWindow(currentbuf, filename)
      exe 'bw '.a:currentbuf
      exe ':edit '.a:filename
endfunction

function s:show()
   let bnum = bufnr('referl')
   if bufexists(bnum)
        execute ':buffer '.bnum
        execute ':bd'
   endif
   execute ':botright  new '.'referl'
   set buftype=nofile noswapfile nowrap
   let shellcmd = s:refpath." ri ls "
  call setline(1, 'Referl Command:  ' ." Show database")
  call append(line('$'), substitute(getline(2), '.', '=', 'g'))
  silent execute '$read !'. shellcmd
  1
endfunction

function s:drgph()
      let file = input("Type the file name here :")
      let stcmd = s:refpath." ri graph \"".file."\""
      execute '!'.stcmd
      echo stcmd
      1
endfunction

function s:query(querytp)
  let pr = input("Type the query :")
  let shellcmd = s:refpath."ri ".a:querytp." \"".pr."\""
   let bnum = bufnr('referl')
   if bufexists(bnum)
        execute ':buffer '.bnum
        execute ':bd'
   endif
   execute ':botright  new '.'referl'
   set buftype=nofile noswapfile nowrap
  set buftype=nofile bufhidden=wipe nobuflisted noswapfile nowrap
  
  call setline(1, 'Referl Command:  ' . pr)
  call append(line('$'), substitute(getline(2), '.', '=', 'g'))
  silent execute '$read !'. shellcmd
  1
endfunction

command! -complete=shellcmd -nargs=+ DoShellCmd call s:refAutoCommand(<q-args>)
function s:refAutoCommand(command)
  "let mainbuf = bufnr("$")
  let shellcmd = s:refpath.pr
  for word in split(shellcmd)
    if word[0] =~ '\v[%#<]'
        let word = expand(word)
    endif
    let word = shellescape(word, 1)
    call add(words, word)
  endfor
  let shellcmd = join(words)
   let bnum = bufnr('referl')
   if bufexists(bnum)
        execute ':buffer '.bnum
        execute ':bd'
   endif
   execute ':botright  new '.'referl'
   set buftype=nofile noswapfile nowrap
  call setline(1, 'Referl Command:  ' . pr)
  call append(line('$'), substitute(getline(2), '.', '=', 'g'))
  silent execute '$read !'. shellcmd
  1
endfunction

let &cpo = s:save_cpo
