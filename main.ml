Py.initialize ();
Py.Run.eval ~start:Py.File "
import torch
print(torch.cuda.is_available())"
