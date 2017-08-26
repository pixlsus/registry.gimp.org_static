import gimp, gimpplugin
from gimpenums import *
pdb = gimp.pdb

class minimalplugin(gimpplugin.plugin):
    def start(self):
        gimp.main(self.init, self.quit, self.query, self._run)

    def init(self):
        print "init\n"

    def quit(self):
        print "quit\n"

    def query(self):
        print "query\n"
        gimp.install_procedure("minimal",
                               "This is a minimal *real*, non-gimpfu python plugin",
                               "", "Michael Schumacher", "Michael Schumacher",
			       "2007", "<Toolbox>/Xtns/Minimal *Real* Python", "", PLUGIN,
			       [(PDB_INT32, "run_mode", "Run mode")], [])

    def minimal(self, par1):
        print "minimal"
        pass
            
if __name__ == '__main__':
    minimalplugin().start()
