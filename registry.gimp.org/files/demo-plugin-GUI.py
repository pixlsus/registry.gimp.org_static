#!/usr/bin/env python

'''
demonstrate GUI widgets for Gimp plugins in Python
2009 Lloyd Konneker

This is a template for registration-time specs for plugin parameters,
demonstrating how to program them and what they look like in the GUI.

Notes:
The types like PF_xxx are types for plugin parameter specs at registration time.
(They are constants defined in the PyGimp package.)
There are similar types named SF_xxx for Scheme and C language.
Some of the PF_XXX types map to PDB_XXX types defined by gimp sourcecode.
See gimpfu.py for the mapping.

The Python types of actual parameter values sent to the plugin are different, and a smaller set.
For example, PF_INT8 becomes a Python integer.

Some type checking is done by PyGimp.
For example, PyGimp throws an exception when a user enters a alphabetic in a PF_FLOAT.
Exception is thrown when the user chooses OK button of the dialog.

The GUI widgets displayed are done by PyGimp, calling other libraries.  See .../python/gimpui.py.

GUI style guide: 
  Don't use the capitalization shown here for descriptions, just capitalize the first letter.
  Do use the colon after the description.
  Do use keyboard shortcuts (underbar).
  Use the ellipsis ... after the menu item if plugin opens a dialog.
  
  Some would advise to right justify the descriptions.
  Its hard to do (with spaces) and probably doesn't internationalize.
'''


def plugin_main( timg, tdrawable, p0, p02, p03,  p04, p1, p2, p3, p4, p14, p15, p16, p17, p18, p19, p20, p21, p22, p23, p24, p25, p26, p27, p28):
  ''' The meat of your plugin.
  '''
  pass 
  

try:
    # succeeds if path includes gimpfu (ie invoked from Gimp app as a plugin)
    from gimpfu import *    
    
    register(
        "python_fu_demo_GUI",
        "Demonstrate builtin plugin GUI widgets",
        "Does nothing but display builtin plugin widgets.",
        "Lloyd Konneker (bootch nc.rr.com)",
        "Copyright 2010 Lloyd Konneker",
        "2010",
        "<Image>/Filters/_Demo GUI widgets...",
        "", # image types: blank means don't care but no image param
        [ 
          # Plugin parameter tuples: (type, name, description, default, [extra])
          # Note these determine both the type of the parameter and the GUI widget displayed.
          # Note the underbar in the description tells what letter is the shortcut key.
          #
          # Editable text boxes
          (PF_INT, "p0", "_INT:", 0), # PF_INT8, PF_INT16, PF_INT32  similar but no difference in Python.
          (PF_FLOAT, "p02", "_FLOAT:", 3.141),
          (PF_STRING, "p03", "_STRING:", "foo"),  # alias PF_VALUE
          (PF_TEXT, "p04", "TEXT:", "bar"),
          # PF_VALUE
          # Pick one from set of choices
          (PF_OPTION,"p1",   "OPTION:", 0, ["0th","1st","2nd"]), # initially 0th is choice
          (PF_RADIO, "p16", "RADIO:", 0, (("0th", 1),("1st",0))), # note bool indicates initial setting of buttons
          # PF_RADIO is usually called a radio button group.
          # SLIDER, ADJUSTMENT types require the extra parameter of the form (min, max, step).
          (PF_TOGGLE, "p2",   "TOGGLE:", 1), # initially True, checked.  Alias PF_BOOL
          # PF_TOGGLE is usually called a checkbox.
          (PF_SLIDER, "p3", "SLIDER:", 0, (0, 100, 10)),
          (PF_SPINNER, "p4", "SPINNER:", 21, (1, 1000, 50)),  # alias PF_ADJUSTMENT
          # Pickers ie combo boxes ie choosers from lists of existing Gimp objects
          (PF_COLOR, "p14", "_COLOR:", (100, 21, 40) ), # extra param is RGB triple
          # PF_COLOUR is an alias by aussie PyGimp author lol
          (PF_IMAGE, "p15", "IMAGE:", None), # should be type gimp.image, but None works
          (PF_FONT, "p17", "FONT:", 0),
          (PF_FILE, "p18", "FILE:", 0),
          (PF_BRUSH, "p19", "BRUSH:", 0),
          (PF_PATTERN, "p20", "PATTERN:", 0),
          (PF_GRADIENT, "p21", "GRADIENT:", 0),
          (PF_PALETTE, "p22", "PALETTE:", 0),
          (PF_LAYER, "p23", "LAYER:", None),
          (PF_CHANNEL, "p24", "CHANNEL:", None),  # ??? Usually empty, I don't know why.
          (PF_DRAWABLE, "p25", "DRAWABLE:", None),
          # Mostly undocumented, but work
          (PF_VECTORS, "p26", "VECTORS:", None),
          (PF_FILENAME, "p27", "FILENAME:", 0),
          (PF_DIRNAME, "p28", "DIRNAME:", 0)
          # PF_REGION might work but probably of little use.  See gimpfu.py.
        ],
        [],
        plugin_main
        
        # Optional registration parameters in PyGimp:
        # menu="<Image>/Filters")   # This is really the menupath, the menuitem is above and can include the path
        # !!! But note no parameters are passed for certain menu paths.
        # Certain menu path prefixes are mangled by PyGimp: <Image>/Languages means sibling to Script Fu and Python Fu items.
        # domain=("gimp20-python", gimp.locale_directory) # internationalization
        )
        
    print "Calling main"
    main()
    
except:
    '''
    invoked standalone.  For testing, you might call plug-in main with testing parameters,
    but you would first need to import the proper PyGimp modules,
    so its unless your plugin doesn't do much with Gimp,
    its easier to test invoked from Gimp.
    '''
    pass
