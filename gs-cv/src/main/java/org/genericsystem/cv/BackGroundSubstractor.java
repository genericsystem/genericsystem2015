package org.genericsystem.cv;

public class BackGroundSubstractor {

	// private Mat backGround;
	//
	// public Mat createMask(Mat frame, Mat backGround) {
	// // copy as we are going to destruct those images maybe
	// Mat camBlur = frame.clone();
	// int blurx = 3;
	// int blury = 3;
	// int subStrationThreshold = 100;
	// // remove noise
	// Imgproc.blur(camBlur, camBlur, new Size(blurx, blury));
	//
	// // take abs diff and create binary image in all 3 channels
	// Mat diff = new Mat();
	// Core.absdiff(backGround, camBlur, diff);
	// Imgproc.threshold(diff, diff, subStrationThreshold, 255, Imgproc.THRESH_BINARY);
	//
	// // extract color channels and merge them to single bitmask
	// Mat r = ColorSpace.getChannel(diff, 2);
	// Mat g = ColorSpace.getChannel(diff, 1);
	// Mat b = ColorSpace.getChannel(diff, 0);
	//
	// Mat mask = r.clone();
	// Core.add(mask, g, mask);
	// Core.add(mask, b, mask);
	//
	// // dilate to remove some black gaps within balls
	// Imgproc.dilate(mask, mask, Imgproc.getStructuringElement(Imgproc.MORPH_ELLIPSE, new Size(calib.getMorphSize(), calib.getMorphSize())));
	//
	// return mask;
	// }

	// int main( int argc, char** argv )
	// {
	// VideoCapture cap;
	// cap.open(0);
	// if( !cap.isOpened() )
	// {
	// cout << "Could not initialize capturing...\n";
	// return 0;
	// }
	// namedWindow("FG", 1);
	//
	/// Ptr<BackgroundSubtractor> pBgSub = createBackgroundSubtractorCNT();
	// Ptr<BackgroundSubtractor> pBgSub = createBackgroundSubtractorMOG2();
	//
	// for(;;)
	// {
	// Mat frame;
	// cap >> frame;
	// if( frame.empty() )
	// {
	// break;
	// }
	// Mat gray;
	// cvtColor(frame, gray, COLOR_BGR2GRAY);
	// Mat fgMask;
	// pBgSub->apply(gray, fgMask);
	// Mat fg;
	// frame.copyTo(fg, fgMask);
	// imshow("FG", fg);
	// }
	// }
}
