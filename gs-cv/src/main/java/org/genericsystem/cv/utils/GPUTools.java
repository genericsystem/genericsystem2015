package org.genericsystem.cv.utils;

import java.util.regex.Pattern;

import org.opencv.core.Core;
import org.opencv.core.CvType;
import org.opencv.core.Mat;
import org.opencv.core.Point;
import org.opencv.core.Size;
import org.opencv.imgproc.Imgproc;


public class GPUTools {

	private static Boolean CUDA_SUPPORT = null;

	static {
		NativeLibraryLoader.load();
	}

	public static boolean isCudaSupported() {
		if (CUDA_SUPPORT == null)
			CUDA_SUPPORT = Pattern.compile("NVIDIA CUDA:\\s+YES").matcher(Core.getBuildInformation()).find();
		return CUDA_SUPPORT;
	}

	public static Mat sepFilter2D(Mat src, int ddepth, Mat kernelX, Mat kernelY, Point anchor, double delta, int borderType) {
		Mat result = new Mat();
		if (isCudaSupported()) {
			// delta unused in this case.
			if (ddepth == CvType.CV_64F) {
				NativeMethods.sepFilter2D(src.nativeObj, result.nativeObj, CvType.CV_32F, kernelX.nativeObj, kernelY.nativeObj, anchor.x, anchor.y, borderType);
				result.convertTo(result, ddepth);
			} else
				NativeMethods.sepFilter2D(src.nativeObj, result.nativeObj, ddepth, kernelX.nativeObj, kernelY.nativeObj, anchor.x, anchor.y, borderType);
		} else
			Imgproc.sepFilter2D(src, result, ddepth, kernelX, kernelY, anchor, delta, borderType);
		return result;
	}

	public static Mat morphologyEx(Mat src, int morphOp, int morph, Size size) {
		Mat result = new Mat();
		if (isCudaSupported())
			NativeMethods.morphologyEx(src.nativeObj, result.nativeObj, morphOp, morph, size.width, size.height);
		else
			Imgproc.morphologyEx(src, result, morphOp, Imgproc.getStructuringElement(morph, size));
		return result;
	}

	public static Mat gemm(Mat src1, Mat src2) {
		return gemm(src1, src2, 0);
	}

	public static Mat gemm(Mat src1, Mat src2, int flags) {
		return gemm(src1, src2, 1, new Mat(), 0, flags);
	}

	public static Mat gemm(Mat src1, Mat src2, double alpha, Mat src3, double beta, int flags) {
		Mat result = new Mat();
		if (isCudaSupported())
			NativeMethods.gemm(src1.nativeObj, src2.nativeObj, alpha, src3.nativeObj, beta, result.nativeObj, flags);
		else
			Core.gemm(src1, src2, alpha, src3, beta, result, flags);
		return result;
	}

	public static Mat addWeighted(Mat src1, double alpha, Mat src2, double beta, double gamma) {
		return addWeighted(src1, alpha, src2, beta, gamma, -1);
	}

	public static Mat addWeighted(Mat src1, double alpha, Mat src2, double beta, double gamma, int dtype) {
		Mat result = new Mat();
		if (isCudaSupported())
			NativeMethods.addWeighted(src1.nativeObj, alpha, src2.nativeObj, beta, gamma, result.nativeObj, dtype);
		else
			Core.addWeighted(src1, alpha, src2, beta, gamma, result, dtype);
		return result;
	}
}
