from jinja2 import Environment, FileSystemLoader
env = Environment(loader=FileSystemLoader("templates"))
makefile_template = env.get_template('Makefile')
print(makefile_template.render(SIMULATOR='variables', VPATH='here', VTOP="tst"))
