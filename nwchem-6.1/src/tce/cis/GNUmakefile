#$Id: GNUmakefile 19706 2010-10-29 17:52:31Z d3y133 $

OBJ_OPTIMIZE =  cis.o cis_x1.o cis_xguess.o cis_xdiagon.o cis_xtidy.o

LIB_INCLUDES = -I../include

LIBRARY = libtce.a

USES_BLAS = cis_x1.F cis_xdiagon.F \


LIB_DEFINES = -DDEBUG_PRINT
#
# Possible #defines
#
#              -DDEBUG_PRINT          /* Debug printing routines */
#	       -DNOCOMMS              /* Turn off ALL communication calls */
#              -DBLOCK_TRANSF         /* Use block-transfer for comms calls */
#              -DOLD_AOBLOCK          /* Recover original AO blocking scheme */
#

HEADERS = 

include ../../config/makefile.h
include ../../config/makelib.h

