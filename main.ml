Py.initialize ();
Py.Run.eval ~start:Py.File "
from test import main
main()"
