Py.initialize ();
Py.Run.eval ~start:Py.File "
import torch
x = torch.ones(1).cuda()
print(x)"
