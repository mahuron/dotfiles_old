# .zshrc    -- loaded if interactive shell  .zshenv .zprofile .zshrc .zlogin

export PAGER="less"
export LESS="-R"
export EDITOR="emacsclient -n"
export PATH=$HOME/.cabal/bin:/usr/local/bin:$PATH
 
setopt extended_glob
setopt autocd

alias ls='ls --color -F'

# LS_COLORS
eval $(dircolors -b ~/.dircolors)

case ${TERM} in
	(*)
		export PS1="%B[%b%3>>%n%>>@%5>>%m%>>%B]%b %2~ %! %B%#%b "
	;;
esac

# init completion
autoload -U compinit
compinit -i

# colorized completion
zmodload -i zsh/complist
zstyle ':completion:*' list-colors $LS_COLORS
