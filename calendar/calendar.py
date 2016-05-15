#!/usr/bin/env python3

from tkinter import Tk, Button, Frame, Menu, messagebox


class Calendar(Tk):

    months_names = [
        'Декабрь',
        'Январь',
        'Февраль',
        'Март',
        'Апрель',
        'Май',
        'Июнь',
        'Июль',
        'Август',
        'Сентябрь',
        'Отябрь',
        'Ноябрь'
    ]

    def __init__(self, *args, **kwargs):
        Tk.__init__(self, *args, **kwargs)
        self.make_months()
        self['menu'] = self.make_menu()
        self.title('Calendar')
        self.resizable(width=False, height=False)
        # Change this for different size
        self.geometry('580x150')

    def make_months(self):
        self.months = []
        self.frames = []
        months = list(self.months_names)
        for anchor in ['nw', 'w', 'w', 'sw']:
            frame = Frame(self)
            buttons = [Button(frame, text=months.pop(0), width=20)
                       for _ in range(3)]
            frame.pack(anchor=anchor)
            for button in buttons:
                button.pack(side='left', padx=3, pady=3)
            self.months.extend(buttons)

    def make_menu(self):
        menu = Menu(self)

        file_menu = Menu(menu, tearoff=0)
        file_menu.add_command(label='Exit', command=self.ask_for_quiting)

        help_menu = Menu(menu, tearoff=0)
        help_menu.add_command(label='About',
                              command=lambda: messagebox.showinfo('Calendar',
                                                                  'By Fedor'))

        menu.add_cascade(label='File', menu=file_menu)
        menu.add_cascade(label='Help', menu=help_menu)

        return menu

    def ask_for_quiting(self):
        answer = messagebox.askokcancel(
            title='Confirm Exit',
            message='Are you sure you want to exit program?',
            default='ok'
        )
        if answer:
            self.destroy()


if __name__ == '__main__':
    c = Calendar()
    c.mainloop()
