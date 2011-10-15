.PHONY:	all units dpkg clean tar zip archives

DCC = dcc32
RM = rm -vf
RMR = $(RM)R
TAR = tar \
	--exclude="*.dof" \
	--exclude="*.vss" \
	--exclude="*.lnk" \
	--exclude="*.cmd" \
	--exclude="*.scc" \
	--exclude="*.vcx" \
	--exclude="*.js*" \
	--exclude="*.avi" \
	--exclude="*.res" \
	--exclude="*.xls" \
	--exclude="CVS/*.*" \
	--exclude="CVS" \
	--exclude="_svn/*.*" \
	--exclude=".svn/*.*" \
	--exclude="_svn" \
	--exclude=".svn" \
	--exclude="*.rar" \
	--exclude="*.zip" \
	--exclude="*.tar" \
	--exclude="*.bz2" \
	-cvjf 

WZZIP := $(shell cygpath -u "$(PROGRAMFILES)\WinZip\WZZIP.EXE")

ZIP = "$(WZZIP)" -r -p \
	-x"*.dof" \
	-x"*.vss" \
	-x"*.lnk" \
	-x"*.cmd" \
	-x"*.scc" \
	-x"*.vcx" \
	-x"*.js*" \
	-x"*.avi" \
	-x"*.res" \
	-x"*.xls" \
	-x"CVS/*.*" \
	-x"CVS" \
	-x"_svn" \
	-x".svn" \
	-x"_svn/*.*" \
	-x".svn/*.*" \
	-x"*.rar" \
	-x"*.zip" \
	-x"*.tar" \
	-x"*.bz2" \


all:	units dpks

units:
	$(DCC) -Ebin programs\all.dpr

dpks:

clean:
	$(RMR) *.#*
	$(RMR) *.rsm
	$(RMR) *.rej
	$(RMR) *.tpu
	$(RMR) *.dpu
	$(RMR) *.dpc
	$(RMR) *.ddp
	$(RMR) *.~*
	$(RMR) *.*~
	$(RMR) *.dsm
	$(RMR) *.MB
	$(RMR) *.tmp
	$(RMR) *.log
	$(RMR) *.dcu
	$(RMR) *.exe

VER := $(shell cat VERSION.txt | sed -e 's/:/-/' - )
DATE := $(shell date +%Y%m%d-%H%M)

distname = "sil-${VER}.${DATE}"
tarfile = ${distname}".src.tar.bz2"
zipfile = ${distname}".src.zip"

$(tarfile):
	echo ${VER}
	echo ${DATE}
	${TAR} ../$@ .

$(zipfile):
	$(ZIP) ../$@ .

tar: $(tarfile)
zip: $(zipfile)

archives: tar zip


