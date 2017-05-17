package org.genericsystem.cv;


import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;

import org.opencv.core.Core;
import org.opencv.core.Mat;
import org.opencv.imgproc.Imgproc;
import org.opencv.videoio.VideoCapture;

import javafx.scene.image.Image;
import javafx.scene.image.ImageView;
import javafx.scene.layout.GridPane;

public class VideoDisplay extends AbstractApp {

    static {
        System.loadLibrary(Core.NATIVE_LIBRARY_NAME);
    }

    public static void main(String[] args) {
        launch(args);
    }

    private final static String classImgRepertory = "aligned-image-3.png";

    private VideoCapture capture = new VideoCapture();
    private ScheduledExecutorService timer;
    private ImageView currentFrame;

    @Override
    protected void fillGrid(GridPane mainGrid) {

        Runnable frameGrabber = new Runnable() {

            @Override
            public void run() {
                // effectively grab and process a single frame
                Mat frame = grabFrame();
                // convert and show the frame
                Image imageToShow = Utils.mat2Image(frame);
                updateImageView(currentFrame, imageToShow);
                mainGrid.add(currentFrame, 0, 0);
            }

        };

        this.timer = Executors.newSingleThreadScheduledExecutor();
        this.timer.scheduleAtFixedRate(frameGrabber, 0, 33, TimeUnit.MILLISECONDS);

        // Img img = new Img(Tools.getClassMats(classImgRepertory).iterator().next());
        // mainGrid.add(img.getImageView(), 0, 0);

    }

    private Mat grabFrame() {
        // init everything
        Mat frame = new Mat();

        // check if the capture is open
        // if (this.capture.isOpened()) {
        try {
            // read the current frame
            this.capture.read(frame);

            // if the frame is not empty, process it
            if (!frame.empty()) {
                Imgproc.cvtColor(frame, frame, Imgproc.COLOR_BGR2GRAY);
            }

        } catch (Exception e) {
            // log the error
            System.err.println("Exception during the image elaboration: " + e);
        }
        // }

        return frame;
    }

    private void updateImageView(ImageView view, Image image) {
        Utils.onFXThread(view.imageProperty(), image);
    }
}