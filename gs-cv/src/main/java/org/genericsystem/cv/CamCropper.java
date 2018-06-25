// package org.genericsystem.cv;
//
// import org.genericsystem.cv.utils.NativeLibraryLoader;
// import org.genericsystem.cv.utils.Tools;
// import org.opencv.calib3d.Calib3d;
// import org.opencv.core.Core;
// import org.opencv.core.DMatch;
// import org.opencv.core.Mat;
// import org.opencv.core.MatOfDMatch;
// import org.opencv.core.MatOfKeyPoint;
// import org.opencv.core.MatOfPoint2f;
// import org.opencv.core.Point;
// import org.opencv.core.Size;
// import org.opencv.features2d.DescriptorExtractor;
// import org.opencv.features2d.DescriptorMatcher;
// import org.opencv.features2d.FeatureDetector;
// import org.opencv.features2d.Features2d;
// import org.opencv.imgproc.Imgproc;
// import org.opencv.videoio.VideoCapture;
// import org.slf4j.Logger;
// import org.slf4j.LoggerFactory;
//
// import java.lang.invoke.MethodHandles;
// import java.util.ArrayList;
// import java.util.List;
// import java.util.concurrent.Executors;
// import java.util.concurrent.ScheduledExecutorService;
// import java.util.concurrent.TimeUnit;
//
// import javafx.scene.image.ImageView;
// import javafx.scene.layout.GridPane;
//
// public class CamCropper extends AbstractApp {
//
// private final static Logger logger = LoggerFactory.getLogger(MethodHandles.lookup().lookupClass());
//
// static {
// NativeLibraryLoader.load();
// }
//
// private final VideoCapture camera = new VideoCapture(0);
// private ScheduledExecutorService timer = Executors.newSingleThreadScheduledExecutor();
//
// public static void main(String[] args) {
// launch(args);
// }
//
// @Override
// protected void fillGrid(GridPane mainGrid) {
// FeatureDetector detector = FeatureDetector.create(FeatureDetector.ORB);
// DescriptorExtractor extractor = DescriptorExtractor.create(DescriptorExtractor.ORB);
// DescriptorMatcher matcher = DescriptorMatcher.create(DescriptorMatcher.BRUTEFORCE_HAMMING);
// Mat frame = new Mat();
// camera.read(frame);
// ImageView src = new ImageView(Tools.mat2jfxImage(frame));
// mainGrid.add(src, 0, 0);
// ImageView src2 = new ImageView(Tools.mat2jfxImage(frame));
// mainGrid.add(src2, 1, 0);
// Img[] oldFrame = new Img[] { new Img(frame) };
// MatOfKeyPoint[] oldKeypoints = new MatOfKeyPoint[] { new MatOfKeyPoint() };
// Mat[] oldDescriptors = new Mat[] { new Mat() };
// detector.detect(frame, oldKeypoints[0]);
// extractor.compute(frame, oldKeypoints[0], oldDescriptors[0]);
// timer.scheduleAtFixedRate(() -> {
// camera.read(frame);
//
// Img newFrame = new Img(frame, false);
// MatOfKeyPoint newKeypoints = new MatOfKeyPoint();
// Mat newDescriptors = new Mat();
// detector.detect(frame, newKeypoints);
// MatOfDMatch matches = new MatOfDMatch();
// extractor.compute(frame, newKeypoints, newDescriptors);
// matcher.match(oldDescriptors[0], newDescriptors, matches);
// DMatch[] match = matches.toArray();
// List<DMatch> goodMatches = new ArrayList<>();
// for (DMatch dMatch : match) {
// double dist = dMatch.distance;
// // System.out.println(dist);
// if (dist < 10)
// goodMatches.add(dMatch);
// }
// try {
// Mat imgMatches = new Mat();
// Features2d.drawMatches(oldFrame[0].getSrc(), oldKeypoints[0], newFrame.getSrc(), newKeypoints, new MatOfDMatch(goodMatches.stream().toArray(DMatch[]::new)), imgMatches);
// src.setImage(new Img(imgMatches).toJfxImage());
// List<Point> goodNewKeypoints = new ArrayList<>();
// List<Point> goodOldKeypoints = new ArrayList<>();
// for (DMatch goodMatch : goodMatches) {
// goodNewKeypoints.add(newKeypoints.toList().get(goodMatch.trainIdx).pt);
// goodOldKeypoints.add(oldKeypoints[0].toList().get(goodMatch.queryIdx).pt);
// }
// Mat homography = Calib3d.findHomography(new MatOfPoint2f(goodOldKeypoints.stream().toArray(Point[]::new)), new MatOfPoint2f(goodNewKeypoints.stream().toArray(Point[]::new)), Calib3d.RANSAC, 10);
// Mat transformedImage = new Mat();
// Imgproc.warpPerspective(oldFrame[0].getSrc(), transformedImage, homography, new Size(newFrame.cols(), newFrame.rows()));
// Core.subtract(newFrame.getSrc(), transformedImage, transformedImage);
// src2.setImage(new Img(transformedImage).toJfxImage());
// } catch (Exception e) {
// logger.warn("Exception while looking for matches.", e);
// }
//
// oldFrame[0] = newFrame;
// oldDescriptors[0] = newDescriptors;
// oldKeypoints[0] = newKeypoints;
//
// }, 0L, 333L, TimeUnit.MILLISECONDS);
// }
//
// @Override
// public void stop() throws Exception {
// timer.shutdown();
// camera.release();
// super.stop();
// }
//
// public Mat copyOver(Img source, Img destination) {
// Img result_grey = source.bgr2Gray();
// Mat mask = new Mat();
// Imgproc.threshold(result_grey.getSrc(), mask, 10, 255, Imgproc.THRESH_BINARY);
// Mat mask_inv = new Mat();
// Core.bitwise_not(mask, mask_inv);
// Mat roi = new Mat();
// Core.bitwise_and(source.getSrc(), source.getSrc(), roi, mask);
// Mat im2 = new Mat();
// Core.bitwise_and(destination.getSrc(), destination.getSrc(), im2, mask_inv);
// Mat result = new Mat();
// Core.add(im2, roi, result);
// return result;
// }
//
// }
