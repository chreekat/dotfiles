vim9script
# Run: vim -Nu NONE --cmd 'set rtp+=.' -S test/test_hledger.vim

import autoload 'hledger.vim'

def Test_FindMainJournal_NotFound()
    # In a temp dir with no main.journal
    var oldcwd = getcwd()
    cd /tmp
    assert_equal('', hledger.FindMainJournal())
    cd `=oldcwd`
enddef

def Test_GetAccounts_BufferFallback()
    new
    setline(1, ['expenses:food  $10', 'income:salary  $100', 'assets:cash  $50'])
    var accounts = hledger.GetAccounts()
    assert_equal(3, len(accounts))
    assert_true(index(accounts, 'expenses:food') >= 0)
    assert_true(index(accounts, 'income:salary') >= 0)
    assert_true(index(accounts, 'assets:cash') >= 0)
    bwipe!
enddef

def Test_CompleteAccount_FindstartNonEmpty()
    new
    setline(1, 'exp')
    assert_equal(0, hledger.CompleteAccount(1, ''))
    bwipe!
enddef

def Test_CompleteAccount_FindstartEmpty()
    new
    setline(1, '')
    assert_equal(-3, hledger.CompleteAccount(1, ''))
    bwipe!
enddef

def Test_CompleteAccount_FindstartWithLeadingSpaces()
    new
    setline(1, '    exp')
    assert_equal(4, hledger.CompleteAccount(1, ''))
    bwipe!
enddef

def Test_CompleteAccount_FuzzyMatch()
    new
    setline(1, ['expenses:food  $10', 'expenses:gas  $20', 'income:salary  $100'])
    var matches = hledger.CompleteAccount(0, 'exp')
    assert_true(len(matches) >= 2)
    bwipe!
enddef

def Test_FindMainJournal_Found()
    # Create temp fixture
    var tmpdir = tempname()
    mkdir(tmpdir, 'p')
    writefile(['2024-01-01 test', '    assets:cash  $100', '    equity'], tmpdir .. '/main.journal')

    var oldcwd = getcwd()
    exe 'cd' tmpdir
    assert_true(hledger.FindMainJournal() =~ 'main.journal$')
    cd `=oldcwd`

    delete(tmpdir, 'rf')
enddef

def Test_GetAccounts_WithMainJournal()
    # Create temp fixture
    var tmpdir = tempname()
    mkdir(tmpdir, 'p')
    writefile(['2024-01-01 test', '    assets:cash  $100', '    expenses:food'], tmpdir .. '/main.journal')

    var oldcwd = getcwd()
    exe 'cd' tmpdir
    new
    var accounts = hledger.GetAccounts()
    # Should get accounts via hledger command
    assert_true(len(accounts) >= 2)
    bwipe!
    cd `=oldcwd`

    delete(tmpdir, 'rf')
enddef

def RunTests()
    v:errors = []
    Test_FindMainJournal_NotFound()
    Test_GetAccounts_BufferFallback()
    Test_CompleteAccount_FindstartNonEmpty()
    Test_CompleteAccount_FindstartEmpty()
    Test_CompleteAccount_FindstartWithLeadingSpaces()
    Test_CompleteAccount_FuzzyMatch()
    Test_FindMainJournal_Found()
    Test_GetAccounts_WithMainJournal()

    if empty(v:errors)
        echom 'All tests passed!'
    else
        for err in v:errors
            echom 'FAIL: ' .. err
        endfor
    endif
    qall!
enddef

RunTests()
