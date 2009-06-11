# The following two entries are REQUIRED
TUNE=/base
EXT=.ultra

# These are vendor supplied overrides for 132.ijpeg base ultra
EXTRA_CFLAGS     = -I/usr/include -DSYSV
Issue            = 
Page             = 
VENDOR           = sun
Volume           = 
action           = validate
company_name     = XXXtested_byXXX
config           = default.cfg
endian           = 17185
ext              = v8
hw_avail         = Jan-94
hw_cpu           = XXX MHz SPARC
hw_disk          = 400MB
hw_disk1         = 4 x 1.3GB
hw_fpu           = Integrated
hw_memory        = 128MB
hw_model         = SPARCstation XXX
hw_ncpu          = 1
hw_ocache        = None
hw_other         = Ethernet
hw_other1        = 
hw_pcache        = 20KBI+16KBD on chip
hw_scache        = 1024KB(I+D) off chip
license_num      = XXX
mach             = default
machine_name     = SPARCstation XXX
mark_runs        = 1
max_active_compares = 10
notes01          = All used: -fast -xO4 -xdepend
output_format    = raw
report_depth     = 
run              = all
shell            = /bin/sh
size             = ref
sw_Kernel_Extensions: = none
sw_avail         = Jan-94
sw_compiler      = Sun SC4.0
sw_compiler1     = 
sw_file          = UFS
sw_os            = Solaris 2.5
sw_other         = 
sw_state         = Multi User
test_date        = Jan-94
tester_name      = XXX
tune             = base
users            = 1
vendor           = anon

# These are evil nasty includes to build vendor targets
