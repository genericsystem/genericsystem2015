package org.genericsystem.cv;

import java.util.List;

import org.genericsystem.cv.ZoneScorer.UnsupervisedZoneScorer;
import org.opencv.core.Core;
import org.opencv.core.Mat;
import org.opencv.core.Point;
import org.opencv.core.Rect;
import org.opencv.core.Scalar;
import org.opencv.imgproc.Imgproc;

public class Zone {

	private final Rect rect;

	public Zone(Rect rect) {
		this.rect = rect;
	}

	public Rect getRect() {
		return rect;
	}

	public double computeUnsupervisedScore(String repositoryPath) {
		return computeUnsupervisedScore(Tools.getClassMats(repositoryPath));
	}

	public double computeUnsupervisedScore(List<Mat> imgs) {
		return new UnsupervisedZoneScorer(this, imgs).getBestScore();
	}

	public String computeUnsupervisedScoredText(List<Mat> imgs) {
		UnsupervisedZoneScorer scorer = new UnsupervisedZoneScorer(this, imgs);
		return scorer.getBestScore() + " : " + scorer.getBestText();
	}

	public double computeSupervisedScore(String repositoryPath) {
		return computeUnsupervisedScore(Tools.getClassMats(repositoryPath));
	}

	public double computeSupervisedScore(List<Mat> imgs) {
		return 0;// TODO
	}

	public Zone adjustRect(double dx, double dy, int maxWidht, int maxHeight) {
		Point tl = new Point(rect.tl().x > dx ? rect.tl().x - dx : 0d, rect.tl().y > dy ? rect.tl().y - dy : 0d);
		Point br = new Point((rect.br().x + dx > maxWidht) ? maxWidht : rect.br().x + dx, (rect.br().y + dy > maxHeight) ? maxHeight : rect.br().y + dy);
		return new Zone(new Rect(tl, br));
	}

	public void draw(Img img, Scalar color, int thickness) {
		Imgproc.rectangle(img.getSrc(), rect.tl(), rect.br(), color, thickness);
	}

	public void write(Img img, String text, double fontScale, Scalar color, int thickness) {
		Imgproc.putText(img.getSrc(), text, new Point(rect.tl().x, rect.br().y), Core.FONT_HERSHEY_PLAIN, fontScale, color, thickness);
	}

}
