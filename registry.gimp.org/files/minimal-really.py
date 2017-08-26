import gimp, gimpplugin
from gimpenums import *
pdb = gimp.pdb

class minimalplugin(gimpplugin.plugin):
    def query(self):
        print "query\n"
        gimp.install_procedure("moreminimal",
                               "This is a even more minimal *real*, non-gimpfu python plugin",
                               "", "Michael Schumacher", "Michael Schumacher",
                               "2007", "<Toolbox>/Xtns/Even more Minimal *Real* Python", "", PLUGIN,
                               [(PDB_INT32, "run_mode", "Run mode")], [])

    def moreminimal(self, par1):
        print "moreminimal"
        pass
            
if __name__ == '__main__':
    minimalplugin().start()
