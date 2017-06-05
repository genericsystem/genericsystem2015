package org.genericsystem.cv;

import java.util.ArrayList;
import java.util.List;

import javafx.scene.layout.GridPane;

import org.opencv.core.Core;
import org.opencv.core.CvType;
import org.opencv.core.Mat;
import org.opencv.core.MatOfPoint;
import org.opencv.core.MatOfRect;
import org.opencv.core.Rect;
import org.opencv.core.Scalar;
import org.opencv.imgproc.Imgproc;
import org.opencv.text.ERFilter;
import org.opencv.text.Text;
import org.opencv.videoio.VideoCapture;

public class TextDetector extends AbstractApp {

	static {
		System.loadLibrary(Core.NATIVE_LIBRARY_NAME);
	}

	private final static String imgClassDirectory = "classes/id-fr-front";

	public static void main(String[] args) {
		launch(args);
	}

	@Override
	protected void fillGrid(GridPane mainGrid) {
		VideoCapture camera = new VideoCapture(0);
		Mat frame = new Mat();
		camera.read(frame);
		Img img1 = new Img(frame);
		// Img img1 = Tools.firstImg(imgClassDirectory);
		detect(img1.getSrc());
		mainGrid.add(img1.getImageView(), 0, 0);

	}

	public static void detect(Mat src) {
		List<Mat> channels = new ArrayList<>();
		Text.computeNMChannels(src, channels);
		int size = channels.size();
		for (int c = 0; c < size; c++) {
			Mat dst = new Mat();
			Mat channel = channels.get(c);
			Core.subtract(new Mat(channel.size(), channel.type(), new Scalar(255)), channel, dst);
			channels.set(c, dst);
		}
		ERFilter er_filter1 = Text.createERFilterNM1("trained_classifierNM1.xml", 16, 0.00015f, 0.13f, 0.2f, true, 0.1f);
		ERFilter er_filter2 = Text.createERFilterNM2("trained_classifierNM2.xml", 0.5f);
		// List<List<MatOfPoint>> groups_boxes = new ArrayList<>();
		for (Mat channel : channels) {
			List<MatOfPoint> regions = new ArrayList<>();
			Text.detectRegions(channel, er_filter1, er_filter2, regions);
			MatOfRect mor = new MatOfRect();
			Text.erGrouping(src, channel, regions, mor);// , Text.ERGROUPING_ORIENTATION_ANY, "trained_classifier_erGrouping.xml", 0.5f);
		}
		// groups_draw(src, groups_boxes, channels.size());
	}

	// helper functions

	static void groups_draw(Mat src, List<MatOfRect> groups, int channelsSize) {
		for (int c = 0; c < channelsSize; c++) {
			MatOfRect mor = groups.get(c);
			for (int i = groups.size() - 1; i >= 0; i--) {
				Rect[] rects = mor.toArray();
				if (src.type() == CvType.CV_8UC3)
					Imgproc.rectangle(src, rects[i].tl(), rects[i].br(), new Scalar(0, 255, 255), 3, Imgproc.LINE_8, 0);
				else
					Imgproc.rectangle(src, rects[i].tl(), rects[i].br(), new Scalar(255), 3, Imgproc.LINE_8, 0);
			}
		}
	}

}

// static void er_show(List<Mat> channels, List<List<MatOfPoint> > regions)
// {
// for (int c=0; c<channels.size(); c++)
// {
// Mat dst = Mat.zeros(channels.get(0).rows()+2,channels.get(0).cols()+2,CvType.CV_8UC1);
// for (int r=0; r<regions.get(c).size(); r++)
// {
// MatOfPoint er = regions.get(c).get(r);
// if (er.parent != NULL) // deprecate the root region
// {
// int newMaskVal = 255;
// int flags = 4 + (newMaskVal << 8) + FLOODFILL_FIXED_RANGE + FLOODFILL_MASK_ONLY;
// floodFill(channels[c],dst,Point(er.pixel%channels[c].cols,er.pixel/channels[c].cols),
// Scalar(255),0,Scalar(er.level),Scalar(0),flags);
// }
// }
// char buff[10]; char *buff_ptr = buff;
// sprintf(buff, "channel %d", c);
// imshow(buff_ptr, dst);
// }
// waitKey(-1);
// }