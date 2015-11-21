# Automatically update the sources
##################################

git checkout master && \
 cd vasm && \
  rm -rf * && \
   cd .. && \
    wget "http://sun.hasenbraten.de/vasm/daily/vasm.tar.gz" && \
     tar -xvzf vasm.tar.gz && \
      rm vasm.tar.gz && \
       cd vasm && \
        git add --verbose -A && \
          git commit  -m "Update vasm from original source http://sun.hasenbraten.de/vasm/daily/vasm.tar.gz" 
cd ..


# Automatically build the assembler
###################################
if false
then
cd vasm && \
	make CPU=z80 SYNTAX=oldstyle && \
		cp vobjdump vasmz80_oldstyle /usr/local/bin
fi
