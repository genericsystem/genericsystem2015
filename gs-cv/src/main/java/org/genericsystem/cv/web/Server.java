package org.genericsystem.cv.web;

import org.genericsystem.cv.utils.NativeLibraryLoader;
import org.opencv.core.Core;
import org.opencv.core.CvType;
import org.opencv.core.Mat;
import org.opencv.core.MatOfByte;
import org.opencv.core.MatOfInt;
import org.opencv.core.Point;
import org.opencv.core.Scalar;
import org.opencv.imgcodecs.Imgcodecs;
import org.opencv.imgproc.Imgproc;

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
				Mat buf = new Mat(1, imageData.length, CvType.CV_8UC1);
				buf.put(0, 0, imageData);
				Mat mat = Imgcodecs.imdecode(buf, Imgcodecs.IMREAD_GRAYSCALE);
				Imgproc.putText(mat, "Coucou", new Point(mat.width() / 2, mat.height() / 2), Core.FONT_HERSHEY_COMPLEX, 1, new Scalar(255, 0, 0));
				MatOfByte byteMat = new MatOfByte();
				MatOfInt parameters = new MatOfInt(Imgcodecs.IMWRITE_PNG_BILEVEL, 1);
				Imgcodecs.imencode(".png", mat, byteMat, parameters);
				byte[] result = byteMat.toArray();

				sws.writeBinaryMessage(Buffer.buffer(result));
			});
		});

		httpServer.listen(8080);
	}

}