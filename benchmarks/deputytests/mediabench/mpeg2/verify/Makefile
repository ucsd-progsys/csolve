

.PHONY: verify clean
verify: clean
	../src/mpeg2enc/mpeg2encode.exe test.par new.m2v
	echo Verifying bitstream
	-cmp test.m2v new.m2v
	echo Decode new.m2v
	../src/mpeg2dec/mpeg2decode.exe -f -b new.m2v -o0 r%d
	echo Comparing encoder and decoder output
	cmp -l q0.Y r0.Y
	cmp -l q1.Y r1.Y
	cmp -l q2.Y r2.Y
	cmp -l q0.U r0.U
	cmp -l q1.U r1.U
	cmp -l q2.U r2.U
	cmp -l q0.V r0.V
	cmp -l q1.V r1.V
	cmp -l q2.V r2.V
	echo Verifying decoder
	../src/mpeg2dec/mpeg2decode.exe -f -b test.m2v -o0 new%d
	cmp -l recon0.Y new0.Y
	cmp -l recon1.Y new1.Y
	cmp -l recon2.Y new2.Y
	cmp -l recon0.U new0.U
	cmp -l recon1.U new1.U
	cmp -l recon2.U new2.U
	cmp -l recon0.V new0.V
	cmp -l recon1.V new1.V
	cmp -l recon2.V new2.V


clean: 
	echo Cleaning
	rm -f new.m2v xyz stat.out new?.? q?.? r?.?
