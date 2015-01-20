# Display finder name in RPROMPT
percol.view.prompt_replacees["F"] = lambda self, **args: self.model.finder.get_name()
percol.view.RPROMPT = ur"(%F) [%i/%I]"

# Change prompt in response to the status of case sensitivity
percol.view.__class__.PROMPT = property(
    lambda self:
    ur"<bold><blue>QUERY </blue>[a]:</bold> %q" if percol.model.finder.case_insensitive
    else ur"<bold><green>QUERY </green>[A]:</bold> %q"
)

percol.import_keymap({
    "C-+" : lambda percol: percol.command.yank(),
    "C-@" : lambda percol: percol.command.beginning_of_line(),
    "C-e" : lambda percol: percol.command.end_of_line(),
    "C-t" : lambda percol: percol.command.select_next(),
    "C-c" : lambda percol: percol.command.select_previous(),
    "C-n" : lambda percol: percol.command.select_next_page(),
    "C-h" : lambda percol: percol.command.select_previous_page(),
    "C-g" : lambda percol: percol.command.select_top(),
    "C-G" : lambda percol: percol.command.select_bottom(),
    "C-v" : lambda percol: percol.command.toggle_mark_and_next(),
    "C-m" : lambda percol: percol.finish(),
    "C-j" : lambda percol: percol.finish(),
    "C-d" : lambda percol: percol.cancel(),
})


percol.view.CANDIDATES_LINE_BASIC    = ("on_default", "default")
percol.view.CANDIDATES_LINE_SELECTED = ("reverse","dim", "on_black", "green", )
percol.view.CANDIDATES_LINE_MARKED   = ("bold", "on_black", "green")
percol.view.CANDIDATES_LINE_QUERY    = ("yellow", "bold")

#Foreground Colors

    #"black" for curses.COLOR_BLACK
    #"red" for curses.COLOR_RED
    #"green" for curses.COLOR_GREEN
    #"yellow" for curses.COLOR_YELLOW
    #"blue" for curses.COLOR_BLUE
    #"magenta" for curses.COLOR_MAGENTA
    #"cyan" for curses.COLOR_CYAN
    #"white" for curses.COLOR_WHITE

#Background Color

    #"on_black" for curses.COLOR_BLACK
    #"on_red" for curses.COLOR_RED
    #"on_green" for curses.COLOR_GREEN
    #"on_yellow" for curses.COLOR_YELLOW
    #"on_blue" for curses.COLOR_BLUE
    #"on_magenta" for curses.COLOR_MAGENTA
    #"on_cyan" for curses.COLOR_CYAN
    #"on_white" for curses.COLOR_WHITE

#Attributes

    #"altcharset" for curses.A_ALTCHARSET
    #"blink" for curses.A_BLINK
    #"bold" for curses.A_BOLD
    #"dim" for curses.A_DIM
    #"normal" for curses.A_NORMAL
    #"standout" for curses.A_STANDOUT
    #"underline" for curses.A_UNDERLINE
    #"reverse" for curses.A_REVERSE

