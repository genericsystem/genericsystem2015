#include "opencv2/cvconfig.h"

#ifdef HAVE_CUDA

#include "opencv2/core/core.hpp"
#include "opencv2/cudaarithm.hpp"
#include "opencv2/cudafilters.hpp"
#include "opencv2/cudaimgproc.hpp"
#include <iostream>

using namespace std;
using namespace cv;

static inline void sepFilter2D(jlong src_, jlong dst_, jint ddepth, jlong kernelX_, jlong kernelY_,
		jdouble anchorX, jdouble anchorY, jint borderType) {
    Mat* src = (Mat*)src_;
    Mat* dst = (Mat*)dst_;
    cuda::GpuMat gSrc(*src);
    cuda::GpuMat gDst;
    Mat* kernelX = (Mat*)kernelX_;
    Mat* kernelY = (Mat*)kernelY_;
    Ptr<cuda::Filter> sep = cuda::createSeparableLinearFilter(src->type(), ddepth, *kernelX, *kernelY, Point(anchorX, anchorY), borderType, borderType);
    sep->apply(gSrc, gDst);
	gDst.download(*dst);
}

static inline void morphologyEx(jlong src_, jlong dst_, jint morphOp, jint morph, jdouble width, jdouble height) {
	Mat* src = (Mat*)src_;
	Mat* dst = (Mat*)dst_;
    cuda::GpuMat gSrc(*src);
    cuda::GpuMat gDst;
	Ptr<cuda::Filter> filter = cuda::createMorphologyFilter(morphOp, src->type(), getStructuringElement(morph, Size(width, height)));
	filter->apply(gSrc, gDst);
    gDst.download(*dst);
}

static inline void gemm(jlong src1_, jlong src2_, jdouble alpha, jlong src3_, jdouble beta, jlong result_, jint flags) {
	Mat* src1 = (Mat*)src1_;
	Mat* src2 = (Mat*)src2_;
	Mat* src3 = (Mat*)src3_;
	Mat* result = (Mat*)result_;
    cuda::GpuMat gSrc1(*src1);
    cuda::GpuMat gSrc2(*src2);
    cuda::GpuMat gSrc3(*src3);
    cuda::GpuMat gDst;
    cuda::gemm(gSrc1, gSrc2, alpha, gSrc3, beta, gDst, flags);
    gDst.download(*result);
}

static inline void addWeighted(jlong src1_, jdouble alpha, jlong src2_, jdouble beta, jdouble gamma, jlong dst_, jint dtype) {
	Mat* src1 = (Mat*)src1_;
	Mat* src2 = (Mat*)src2_;
    cuda::GpuMat gSrc1(*src1);
    cuda::GpuMat gSrc2(*src2);
	Mat* dst= (Mat*)dst_;
    cuda::GpuMat gDst;
    cuda::addWeighted(gSrc1, alpha, gSrc2, beta, gamma, gDst, dtype);
    gDst.download(*dst);
}

#endif // HAVE_CUDA
