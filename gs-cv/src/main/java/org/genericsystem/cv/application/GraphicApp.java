package org.genericsystem.cv.application;

import java.io.ByteArrayInputStream;
import java.util.List;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;

import org.genericsystem.cv.utils.NativeLibraryLoader;
import org.opencv.core.CvType;
import org.opencv.core.Mat;
import org.opencv.core.MatOfByte;
import org.opencv.core.Rect;
import org.opencv.core.Size;
import org.opencv.imgcodecs.Imgcodecs;
import org.opencv.imgproc.Imgproc;
import org.opencv.videoio.VideoCapture;

import javafx.application.Application;
import javafx.application.Platform;
import javafx.event.EventHandler;
import javafx.scene.Group;
import javafx.scene.Scene;
import javafx.scene.control.ScrollPane;
import javafx.scene.image.Image;
import javafx.scene.image.ImageView;
import javafx.scene.input.KeyCode;
import javafx.scene.layout.GridPane;
import javafx.scene.layout.VBox;
import javafx.stage.Stage;
import javafx.stage.WindowEvent;

public class GraphicApp extends Application{

	private final double f = 6.053 / 0.009;
	private final VideoCapture capture = new VideoCapture(0);
	private SuperFrameImg superFrame = SuperFrameImg.create(capture, f);	
	private ReferenceManager referenceManager = new ReferenceManager(superFrame.size());
	private boolean stabilizedMode = false;
	private boolean textsEnabledMode = false;
	private Deperspectiver deperspectiver = new Deperspectiver();
	private DisplayManager displayManager = new DisplayManager();
	private ScheduledExecutorService timer = Executors.newSingleThreadScheduledExecutor();

	public static void main(String[] args) {
		launch(args);
	}

	static {
		NativeLibraryLoader.load();
	}

	public final static double displayWidth = 400d;

	@Override
	public void start(Stage stage) throws Exception {
		GridPane gridPane = new GridPane();
		//	fillGrid(gridPane);
		double displaySizeReduction = 2;

		ImageView view00 = new ImageView();
		ImageView view01 = new ImageView();
		ImageView view10 = new ImageView();
		ImageView view11 = new ImageView();

		gridPane.add(view00, 0, 0);
		gridPane.add(view01, 0, 1);
		gridPane.add(view10, 1, 0);
		gridPane.add(view11, 1, 1);

		view00.setFitWidth(superFrame.width() / displaySizeReduction);
		view00.setFitHeight(superFrame.height() / displaySizeReduction);
		view01.setFitWidth(superFrame.width() / displaySizeReduction);
		view01.setFitHeight(superFrame.height() / displaySizeReduction);
		view10.setFitWidth(superFrame.width() / displaySizeReduction);
		view10.setFitHeight(superFrame.height() / displaySizeReduction);
		view11.setFitWidth(superFrame.width() / displaySizeReduction);
		view11.setFitHeight(superFrame.height() / displaySizeReduction);

		double[] pp = superFrame.getPrincipalPoint();
		timer.scheduleAtFixedRate(() -> {
			try {
				if (!stabilizedMode)
					superFrame = SuperFrameImg.create(capture, f);
				Lines lines = superFrame.detectLines();				
				Mat deperspectiveHomography = deperspectiver.doWork(superFrame, f, pp, textsEnabledMode, lines);				
				if (deperspectiveHomography != null){
					SuperFrameImg superDeperspectived = superFrame.deperspective(deperspectiveHomography);
					List<Rect> detectedRects = superDeperspectived.detectRects();
					ImgDescriptor newImgDescriptor = new ImgDescriptor(superDeperspectived);
					referenceManager.submit(newImgDescriptor, detectedRects);

					Image image1 = displayManager.displayFrame(superFrame, lines, deperspectiver.getCalibratedVps());
					Image image2 = displayManager.displaySuperDeperspectived(superDeperspectived, detectedRects);
					Image image3 = displayManager.displayDiffFrame(superDeperspectived);
					Image image4 = displayManager.displayReferenceFrame(referenceManager, pp ,f);

					Platform.runLater(() -> {
						view00.setImage(image1);
						view01.setImage(image2);
						view10.setImage(image3);
						view11.setImage(image4);
					});
				}
			} catch (Throwable e) {
				e.printStackTrace();
			}
		}, 30, 30, TimeUnit.MILLISECONDS);

		Scene scene = new Scene(new Group());
		stage.setTitle("Generic System Information Retriever");
		ScrollPane scrollPane = new ScrollPane(gridPane);
		scrollPane.setFitToHeight(true);
		VBox root = new VBox(scrollPane);
		scene.setRoot(root);
		stage.setOnCloseRequest(new EventHandler<WindowEvent>() {
			@Override
			public void handle(WindowEvent event) {
				try {
					stop();
				} catch (Exception e) {
					throw new RuntimeException(e);
				}
			}
		});

		scene.setOnKeyPressed(event -> {
			if (event.getCode() == KeyCode.SPACE)
				onSpace();
			if (event.getCode() == KeyCode.R)
				onR();
			if (event.getCode() == KeyCode.T)
				onT();
			if (event.getCode() == KeyCode.S)
				onS();
		});
		stage.setScene(scene);
		stage.show();
	}

	protected void onS() {
		System.out.println("s pressed");
	}

	// hook
	protected void onSpace() {
		System.out.println("space pressed");
		stabilizedMode = !stabilizedMode; 
	}

	protected void onR() {
		System.out.println("r pressed");
	}

	protected void onT() {
		System.out.println("t pressed");
		textsEnabledMode = !textsEnabledMode;
	}

	protected ImageView buildImageViewFromMat(Mat src) {
		Mat conv = new Mat();
		src.convertTo(conv, CvType.CV_8UC1);
		Mat target = new Mat();
		Imgproc.resize(conv, target, new Size(displayWidth, Math.floor((displayWidth / conv.width()) * conv.height())));
		MatOfByte buffer = new MatOfByte();
		Imgcodecs.imencode(".png", target, buffer);
		ImageView imageView = new ImageView(new Image(new ByteArrayInputStream(buffer.toArray())));
		imageView.setPreserveRatio(true);
		imageView.setFitWidth(displayWidth);
		conv.release();
		target.release();
		buffer.release();
		return imageView;
	}

	@Override
	public void stop() throws Exception {
		super.stop();
		timer.shutdown();
		timer.awaitTermination(5000, TimeUnit.MILLISECONDS);
		capture.release();
	}
}
