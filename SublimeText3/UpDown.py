import sublime
import sublime_plugin


class PlusLineCommand(sublime_plugin.TextCommand):
    def run(self, edit, lines=1):
        (row, col) = self.view.rowcol(self.view.sel()[0].begin())
        self.view.run_command("goto_line", {"line": row + 1 + lines})

class MinusLineCommand(sublime_plugin.TextCommand):
    def run(self, edit, lines=1):
        (row, col) = self.view.rowcol(self.view.sel()[0].begin())
        # Avoid line wrapping.
        self.view.run_command("goto_line", {"line": max(1, row + 1 - lines)})
