p x = x+1
q x = x+2

cmap [] _ = []
cmap (ca:co) x = (ca x):(cmap co x)