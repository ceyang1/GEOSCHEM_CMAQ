#######################################################################
# Makefile
# GEOS2CMAQ IC/BC Interface Makefile for
#             domain ${DOMAIN} from GEOSCHEM Results ${SPECIES}
# File Organization Schema:
#  1) ${DIR_SRC} dir is the source file root
#  2) ${DIR_SRC_APPL} contains domain/species specific files
#     ${DIR_SRC_COMM} contains general files
#  3) ${DIR_SRC} contain common files for domains/species
#  4) Output generated in $PWD
# Zuopan Li (zuli@cs.utk.edu), Thu Jul 13 18:12:53 EDT 2006
#######################################################################

#######################################################################
# 1.  Source Dir
#    Note:
#     It might cause too long a line in source code because of SUBST
#     One way to go arround is to link the src dir to shorter path
#######################################################################
DIR_SRC_ROOT = /path/to/GEOSChem2CMAQ

#######################################################################
# 2. SPECIES denotes the species number in GEOSCHEM results
#  This is related to files:
#  1. GEOSCHEM_${SPECIES}.EXT,
#  2. read_geoschem_ij_avg_species.${SPECIES}.F90
#  3. icon_out_with_ppmv_in_${SPECIES}.F90
#  4. bcon_out_with_ppmv_in_${SPECIES}.F90
#  5. SPECIES_MAPPING_${SPECIES}_OUTUNIT_PPMV.F90
#  6. CMAQ_${DOMAIN}_${SPECIES}.EXT
#######################################################################
SPECIES = STDSOA_95SPC

#######################################################################
# 3. DOMAIN is the domain used.
# Files related:
#  1. CMAQ_${DOMAIN}_${SPECIES}_${CB_VERSION}.EXT
#######################################################################
DOMAIN = CO12_R55XC70

#######################################################################
# 4. Layer and CBversion
#######################################################################
LAYER = 25
CB_VERSION = CB05e51

#######################################################################
# 5. DIR_SRC_APPL and DIR_SRC_COMM
#  Note:
#    Same as DIR_SRC_ROOT, link to APPL_${CB_VERSION}
#    Make sure you know what you are doing here.
#######################################################################
DIR_SRC_APPL =  ${DIR_SRC_ROOT}/APPL_ALL
DIR_SRC_COMM =  ${DIR_SRC_ROOT}/APPL_ALL

#######################################################################
# 6. TARGET IC BC file names
#######################################################################
TARGETS = BCON_${DOMAIN}_${SPECIES}_LAYER${LAYER}_${CB_VERSION} ICON_${DOMAIN}_${SPECIES}_LAYER${LAYER}_${CB_VERSION}

#######################################################################
# 7. IOAPI and NETCEF
#######################################################################
IOAPI_EXT_DIR = /lustre/atlas2/cli106/scratch/ceyang1/GEOSChem2CMAQ/LIB/ioapi_3/ioapi/fixed_src
IOAPI_LIBDIR  = /lustre/atlas2/cli106/scratch/ceyang1/GEOSChem2CMAQ/LIB/ioapi_3/Linux3_x86_64pg
NETCDF_LIBDIR = /lustre/atlas2/cli106/scratch/ceyang1/GEOSChem2CMAQ/LIB/netCDF/Linux
#STENEX_LIBDIR = /lustre/atlas2/cli106/scratch/ceyang1/GEOSChem2CMAQ/LIB/stenex/Linux2_x86_64pg

#######################################################################
# 8. Compiling info
#######################################################################
FC    = ftn
CC    = cc
CPLUS = CC
FPP   = ftn
F_FLAGS = -Mfixed -Mextend -O2  -I. -byteswapio
C_FLAGS = -D__USE_LARGEFILE64

LINK_FLAGS = -Bstatic
#LINK_FLAGS = -mcmodel=medium
CPP_FLAGS = -DF90 -DSUBST_MODULES=NOOP_MODULES -DSUBST_GLOBAL_SUM=NOOP_GLOBAL_SUM

#LIBRARIES = -L${IOAPI_LIBDIR} -lioapi -L${NETCDF_LIBDIR} -lnetcdf -lnetcdff \
 -L${STENEX_LIBDIR} -lsef90_noop
LIBRARIES = -L${NETCDF_LIBDIR} -lnetcdff -lnetcdf -L${IOAPI_LIBDIR} -lioapi

INCLUDES = \
 -DSUBST_GRID_ID= \
 -DSUBST_CONST=\"${IOAPI_EXT_DIR}/CONST3.EXT\" \
 -DSUBST_IOPARMS=\"${IOAPI_EXT_DIR}/PARMS3.EXT\" \
 -DSUBST_IOFDESC=\"${IOAPI_EXT_DIR}/FDESC3.EXT\" \
 -DSUBST_IODECL=\"${IOAPI_EXT_DIR}/IODECL3.EXT\" \
 -DSUBST_XSTAT=\"${IOAPI_EXT_DIR}/STATE3.EXT\" \
 -DSUBST_GEOSCHEM_ID=\"${DIR_SRC_APPL}/GEOSCHEM_${SPECIES}.EXT\" \
 -DSUBST_CMAQ_ID=\"${DIR_SRC_APPL}/CMAQ_${DOMAIN}_${SPECIES}_${CB_VERSION}.EXT\" \
 -DSUBST_COORD_ID=\"${DIR_SRC_APPL}/COORD_${DOMAIN}_${LAYER}L.EXT\" \
 -DSUBST_VGRD_ID=\"${DIR_SRC_APPL}/VGRD_${LAYER}.EXT\"

# FILES_COM list of common files used by both IC/BC
FILES_COM=\
 ${DIR_SRC_COMM}/print_info.F90 \
 ${DIR_SRC_COMM}/read_geoschem_bxhght.F90 \
 ${DIR_SRC_APPL}/read_geoschem_ij_avg_species.${SPECIES}.F90 \
 ${DIR_SRC_APPL}/SPECIES_MAPPING_${SPECIES}_${CB_VERSION}_OUTUNIT_PPMV.F90 \
 ${DIR_SRC_COMM}/lat_lon.F90 \
 ${DIR_SRC_COMM}/get_tau.c

FILES_ICON =\
 ${DIR_SRC_COMM}/geos2cmaq_icon_MCIPAirDensity.F90 \
 ${DIR_SRC_APPL}/icon_out_with_ppmv_in_${SPECIES}.F90 \
 ${DIR_SRC_APPL}/MERGE_STD_IC.F90 \
 ${FILES_COM}

FILES_BCON =\
 ${DIR_SRC_COMM}/geos2cmaq_bcon_MCIPAirDensity.F90 \
 ${DIR_SRC_APPL}/bcon_out_with_ppmv_in_${SPECIES}.F90 \
 ${DIR_SRC_APPL}/MERGE_STD_BC.F90 \
 ${FILES_COM}

OBJECTS_COM = \
 print_info.o \
 read_geoschem_bxhght.o \
 read_geoschem_ij_avg_species.${SPECIES}.o \
 SPECIES_MAPPING_${SPECIES}_${CB_VERSION}_OUTUNIT_PPMV.o \
 lat_lon.o \
 get_tau.o

OBJECTS_ICON =\
 geos2cmaq_icon_MCIPAirDensity.o \
 icon_out_with_ppmv_in_${SPECIES}_${CB_VERSION}.o \
 MERGE_STD_IC.o \
 ${OBJECTS_COM}

OBJECTS_BCON =\
 geos2cmaq_bcon_MCIPAirDensity.o \
 bcon_out_with_ppmv_in_${SPECIES}_${CB_VERSION}.o \
 MERGE_STD_BC.o \
 ${OBJECTS_COM}


all: ${TARGETS}
clean:
	rm -rf ${TARGETS} *.o

BCON_${DOMAIN}_${SPECIES}_LAYER${LAYER}_${CB_VERSION}: $(OBJECTS_BCON)
	$(FC) $(LINK_FLAGS) $(OBJECTS_BCON) $(LIBRARIES) -o $@

ICON_${DOMAIN}_${SPECIES}_LAYER${LAYER}_${CB_VERSION}: $(OBJECTS_ICON)
	$(FC) $(LINK_FLAGS) $(OBJECTS_ICON) $(LIBRARIES) -o $@

#COMMON
print_info.o: ${DIR_SRC_COMM}/print_info.F90
	$(FC) -c $(F_FLAGS) $(CPP_FLAGS) $(INCLUDES) $<

read_geoschem_bxhght.o : ${DIR_SRC_COMM}/read_geoschem_bxhght.F90
	$(FC) -c $(F_FLAGS) $(CPP_FLAGS) $(INCLUDES) $<

read_geoschem_ij_avg_species.${SPECIES}.o :${DIR_SRC_APPL}/read_geoschem_ij_avg_species.${SPECIES}.F90
	$(FC) -c $(F_FLAGS) $(CPP_FLAGS) $(INCLUDES) $<

SPECIES_MAPPING_${SPECIES}_${CB_VERSION}_OUTUNIT_PPMV.o: ${DIR_SRC_APPL}/SPECIES_MAPPING_${SPECIES}_${CB_VERSION}_OUTUNIT_PPMV.F90
	$(FC) -c $(F_FLAGS) $(CPP_FLAGS) $(INCLUDES) $<

lat_lon.o : ${DIR_SRC_COMM}/lat_lon.F90
	$(FC) -c $(F_FLAGS) $(CPP_FLAGS) $(INCLUDES) $<

get_tau.o :${DIR_SRC_COMM}/get_tau.c
	$(CC) $(C_FLAGS) -c $<

# ICON
geos2cmaq_icon_MCIPAirDensity.o : ${DIR_SRC_COMM}/geos2cmaq_icon_MCIPAirDensity.F90
	$(FC) -c $(F_FLAGS) $(CPP_FLAGS) $(INCLUDES) $<

icon_out_with_ppmv_in_${SPECIES}_${CB_VERSION}.o : ${DIR_SRC_APPL}/icon_out_with_ppmv_in_${SPECIES}_${CB_VERSION}.F90
	$(FC) -c $(F_FLAGS) $(CPP_FLAGS) $(INCLUDES) $<

MERGE_STD_IC.o : ${DIR_SRC_APPL}/MERGE_STD_IC.F90
	$(FC) -c $(F_FLAGS) $(CPP_FLAGS) $(INCLUDES) $<

#BCON
geos2cmaq_bcon_MCIPAirDensity.o : ${DIR_SRC_COMM}/geos2cmaq_bcon_MCIPAirDensity.F90
	$(FC) -c $(F_FLAGS) $(CPP_FLAGS) $(INCLUDES) $<

bcon_out_with_ppmv_in_${SPECIES}_${CB_VERSION}.o :${DIR_SRC_APPL}/bcon_out_with_ppmv_in_${SPECIES}_${CB_VERSION}.F90
	$(FC) -c $(F_FLAGS) $(CPP_FLAGS) $(INCLUDES) $<

MERGE_STD_BC.o : ${DIR_SRC_APPL}/MERGE_STD_BC.F90
	$(FC) -c $(F_FLAGS) $(CPP_FLAGS) $(INCLUDES) $<
