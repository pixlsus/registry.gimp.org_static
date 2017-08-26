#!/usr/bin/env python

from DocXMLRPCServer import DocXMLRPCServer
from gimpfu import *
       
def xmlrpcserver(run, port):
    server = DocXMLRPCServer(("localhost", port))
    server.register_introspection_functions()
    server.register_function(gimp.gtkrc, "gtkrc")
    server.register_instance(gimp)
    if run:
        server.serve_forever()
    pass
    
register(
    "python_xmlrpcserver",
    "A XML-RPC interfce to the PDB",
    "The purpose of this extension is to provide an XML-RPC based web srrvices interface to a running GIMP. The main goal is technology demonstration, but there may be some real uses, too.",
    "Michael Schumacher",
    "Michael Schumacher",
    "2007",
    "<Toolbox>/Xtns/Languages/Python-Fu/Start XML-RPC Server...",
    "",
    [
    (PF_BOOL,  "run",  "Start Server", 0),
    (PF_INT,   "port", "Port", 8000)
    ],
    [],
    xmlrpcserver
    )

main()
