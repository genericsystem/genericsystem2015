package org.genericsystem.cv.web;

import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.apache.commons.math3.analysis.polynomials.PolynomialSplineFunction;
import org.genericsystem.cv.Img;
import org.genericsystem.cv.application.GeneralInterpolator;
import org.genericsystem.cv.application.OrientedPoint;
import org.genericsystem.cv.application.ProjectionLines;
import org.genericsystem.cv.application.Segment;
import org.genericsystem.cv.application.SplineInterpolator;
import org.genericsystem.cv.application.TrajectStep;
import org.genericsystem.cv.application.fht.FHT;
import org.genericsystem.cv.application.mesh.MeshManager;
import org.genericsystem.cv.utils.NativeLibraryLoader;
import org.opencv.core.Core;
import org.opencv.core.CvType;
import org.opencv.core.Mat;
import org.opencv.core.MatOfByte;
import org.opencv.core.Scalar;
import org.opencv.imgcodecs.Imgcodecs;

import io.vertx.core.AbstractVerticle;
import io.vertx.core.buffer.Buffer;
import io.vertx.core.http.HttpServer;
import io.vertx.core.http.HttpServerOptions;

public class Server extends AbstractVerticle {
	static {
		NativeLibraryLoader.load();
	}

	public static void main(String[] args) {
		Runner.runExample(Server.class);
	}

	@Override
	public void start() throws Exception {

		// Création du server de Websockets

		HttpServer httpServer = vertx.createHttpServer(new HttpServerOptions().setMaxWebsocketFrameSize(655360));

		// Reception des requêtes HTTP du navigateur et réponses

		httpServer.requestHandler(req -> {
			if (req.uri().equals("/")) {
				req.response().sendFile("index.html");
			} else if (req.uri().equals("/js/main.js"))
				req.response().sendFile("main.js");

			// if (req.uri().equals("/css/style.css"))
			//
			// req.response().sendFile("/home/middleware/Etienne/eclipse-workspace/WScams-master/src/main/webapp/css/style.css");

		});

		// Réponse envoyée au client :

		httpServer.websocketHandler(sws -> {

			sws.handler(buffer -> {

				byte[] imageData = buffer.getBytes();
				Mat buf = new Mat(1, imageData.length, CvType.CV_8U);
				buf.put(0, 0, imageData);
				Mat frame = Imgcodecs.imdecode(buf, Imgcodecs.IMREAD_COLOR);
				long ref = System.currentTimeMillis();
				Img binarized = new Img(frame, false).adaptativeGaussianInvThreshold(7, 5);// .dilate(Imgproc.getStructuringElement(Imgproc.MORPH_ELLIPSE, new Size(2, 2)));// .canny(20, 80);
				Img transposedBinarized = binarized.transpose();
				ref = trace("Binarization", ref);

				double vRecover = 0.75;
				int vStripsNumber = (int) ((16d / 3 - vRecover + 1) / (1 - vRecover));
				double stripWidth = (binarized.width() / (vStripsNumber * (1 - vRecover) + vRecover - 1));
				double vStep = ((1 - vRecover) * stripWidth);
				double minVerticalAccuracy = 180 * (Math.atan(1 / (stripWidth - 1))) / Math.PI;
				System.out.println(vStripsNumber + " verticals strips with width : " + stripWidth + " each step : " + vStep + " min accuracy : " + minVerticalAccuracy);

				double hRecover = 0.75;
				int hStripsNumber = (int) ((9d / 3 - hRecover + 1) / (1 - hRecover));
				double stripHeight = (binarized.height() / (hStripsNumber * (1 - hRecover) + hRecover - 1));
				double hStep = ((1 - hRecover) * stripHeight);
				double minHorizontalAccuracy = 180 * (Math.atan(1 / (stripHeight - 1))) / Math.PI;
				System.out.println(hStripsNumber + " horizontal strips with width : " + stripHeight + " each step : " + hStep + " min accuracy : " + minHorizontalAccuracy);

				Mat enlargedBinarized = new Mat();
				Core.copyMakeBorder(binarized.getSrc(), enlargedBinarized, 0, 0, (int) Math.round(stripWidth / 2), (int) Math.round(stripWidth / 2), Core.BORDER_CONSTANT, new Scalar(0));

				Mat enlargedTransposedBinarized = new Mat();
				Core.copyMakeBorder(transposedBinarized.getSrc(), enlargedTransposedBinarized, 0, 0, (int) Math.round(stripHeight / 2), (int) Math.round(stripHeight / 2), Core.BORDER_CONSTANT, new Scalar(0));

				List<Mat> vStrips = FHT.extractStrips(enlargedBinarized, vStripsNumber, stripWidth, vStep);
				List<Mat> hStrips = FHT.extractStrips(enlargedTransposedBinarized, hStripsNumber, stripHeight, hStep);
				ref = trace("Extract strips", ref);

				List<Mat> vHoughs = vStrips.stream().map(strip -> FHT.fastHoughTransform(strip)).collect(Collectors.toList());
				List<Mat> hHoughs = hStrips.stream().map(strip -> FHT.fastHoughTransform(strip)).collect(Collectors.toList());

				vHoughs.forEach(projectionMap -> Core.normalize(projectionMap, projectionMap, 0, 1, Core.NORM_MINMAX));
				hHoughs.forEach(projectionMap -> Core.normalize(projectionMap, projectionMap, 0, 1, Core.NORM_MINMAX));
				ref = trace("Compute FHT", ref);

				List<List<TrajectStep>> vHoughTrajs = vHoughs.stream().map(projectionMap -> FHT.bestTrajectFHT(projectionMap, 21, -0.08)).collect(Collectors.toList());
				List<List<TrajectStep>> hHoughTrajs = hHoughs.stream().map(projectionMap -> FHT.bestTrajectFHT(projectionMap, 21, -0.08)).collect(Collectors.toList());
				ref = trace("Compute trajects", ref);

				List<List<OrientedPoint>[]> fhtHorizontals = ProjectionLines.toHorizontalsOrientedPoints(vHoughTrajs, vStep, 0.5, 0.05);
				List<List<OrientedPoint>[]> fhtVerticals = ProjectionLines.toVerticalsOrientedPoints(hHoughTrajs, hStep, 0.5, 0.05);

				List<List<Segment>>[] horizontalSegments = Segment.connect(fhtHorizontals, vStep, 0.05, false);
				List<List<Segment>>[] verticalSegments = Segment.connect(fhtVerticals, hStep, 0.05, true);

				Img frameDisplayFHT = new Img(frame.clone(), false);
				Segment.displayHorizontalOps(horizontalSegments[0], frameDisplayFHT.getSrc(), vStep, hStep, new Scalar(0, 255, 0));
				Segment.displayHorizontalOps(horizontalSegments[1], frameDisplayFHT.getSrc(), vStep, hStep, new Scalar(0, 0, 255));
				Segment.displayVerticalOps(verticalSegments[0], frameDisplayFHT.getSrc(), vStep, hStep, new Scalar(255, 255, 0));
				Segment.displayVerticalOps(verticalSegments[1], frameDisplayFHT.getSrc(), vStep, hStep, new Scalar(255, 0, 255));

				ref = trace("Display lines", ref);

				List<PolynomialSplineFunction>[] horizontalSplines = Segment.toSplines(horizontalSegments, false);
				List<PolynomialSplineFunction>[] verticalSplines = Segment.toSplines(verticalSegments, true);
				Img splineDisplay = new Img(frame.clone(), false);
				FHT.displayHSplines(horizontalSplines[0], splineDisplay.getSrc(), 0, 255, 0);
				FHT.displayHSplines(horizontalSplines[1], splineDisplay.getSrc(), 0, 0, 255);
				FHT.displayVSplines(verticalSplines[0], splineDisplay.getSrc(), 255, 255, 0);
				FHT.displayVSplines(verticalSplines[1], splineDisplay.getSrc(), 255, 0, 255);
				ref = trace("Display splines", ref);

				List<OrientedPoint> flatHorizontalSegments = Stream.of(horizontalSegments).flatMap(h -> h.stream()).flatMap(h -> h.stream()).flatMap(edge -> Stream.of(edge.op1, edge.op2)).collect(Collectors.toList());
				List<OrientedPoint> flatVerticalSegments = Stream.of(verticalSegments).flatMap(h -> h.stream()).flatMap(h -> h.stream()).flatMap(edge -> Stream.of(edge.op1, edge.op2)).collect(Collectors.toList());
				GeneralInterpolator interpolatorFHT = new GeneralInterpolator(flatHorizontalSegments, flatVerticalSegments, 4, 0.0001);
				SplineInterpolator superInterpolator = new SplineInterpolator(interpolatorFHT, horizontalSplines, verticalSplines);
				ref = trace("Prepare interpolator", ref);

				MeshManager meshManager = new MeshManager(6, 4, superInterpolator, frame, 700);
				ref = trace("Build and draw mesh", ref);

				ref = trace("3D surface / svd", ref);

				Img dewarpFHT3D = new Img(meshManager.dewarp3D());
				ref = trace("Dewarp 3D", ref);

				MatOfByte byteMat = new MatOfByte();
				// MatOfInt parameters = new MatOfInt(Imgcodecs.IMWRITE_PNG_BILEVEL, 1);
				Imgcodecs.imencode(".png", dewarpFHT3D.getSrc(), byteMat);
				byte[] result = byteMat.toArray();

				sws.writeBinaryMessage(Buffer.buffer(result));
			});
		});

		httpServer.listen(8080);
	}

	private long trace(String message, long ref) {
		long last = System.currentTimeMillis();
		System.out.println(message + " : " + (last - ref));
		return last;
	}

}