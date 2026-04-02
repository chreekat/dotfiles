vim9script
setlocal omnifunc=hledger#CompleteAccount
b:undo_ftplugin = get(b:, 'undo_ftplugin', '') .. '|setl omnifunc<'
