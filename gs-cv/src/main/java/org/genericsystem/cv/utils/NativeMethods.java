package org.genericsystem.cv.utils;

import org.bytedeco.javacpp.Loader;
import org.bytedeco.javacpp.annotation.Platform;

@Platform(include = "javacv.h", linkpath = { "/usr/local/cuda/lib64/", "/usr/local/lib" }, link = { "opencv_cudaimgproc", "opencv_core", "opencv_cudaarithm" })
public class NativeMethods {

	static {
		Loader.load();
	}

	static native void morphologyEx(long src, long result, int morphOp, int morph, double width, double height);

	static native void sepFilter2D(long src, long result, int ddepth, long kernelX, long kernelY, double anchorX, double anchorY, int borderType);

	static native void gemm(long src1, long src2, double alpha, long src3, double beta, long result, int flags);

	static native void addWeighted(long src1, double alpha, long src2, double beta, double gamma, long dst, int dtype); 
}
