import sys
environmentVariables = open("./model/configuration/environmentVariables.config").readlines()
mech_file = str(sys.argv[1])

for x in environmentVariables: print(x)

#file[0]

for x in environmentVariables:
    x = x.split()
    
    try:
        if x[1] == "DILUTE" :
            y = (x[2])
    
    except IndexError:
        continue

if y == "NOTUSED" :
    sys.exit()

        
spec = open("./model/configuration/mechanism.species").readlines()

f = open(f"./{mech_file}","a+")

for x in spec:
    x = x.split()
    z = (x[1])
    f.write(f"% {y} : {z} = LOSS ;\n")
        
f.close()


