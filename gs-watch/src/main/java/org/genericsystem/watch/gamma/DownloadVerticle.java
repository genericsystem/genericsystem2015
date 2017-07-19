package org.genericsystem.watch.gamma;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.util.List;
import java.util.concurrent.ArrayBlockingQueue;
import java.util.concurrent.BlockingQueue;

import io.vertx.core.AsyncResult;
import io.vertx.core.Future;
import io.vertx.core.http.HttpClient;
import io.vertx.core.json.JsonObject;

public class DownloadVerticle extends ActionVerticle {

	public static final String ACTION = "download";

	public DownloadVerticle(String privateAddress, String privatePath, String ip, List<JsonObject> messages, List<JsonObject> tasks) {
		super(privateAddress, privatePath, ip, messages, tasks);
	}

	@Override
	public String getAction() {
		return ACTION;
	}

	@Override
	protected void handle(Future<Object> future, String fileName, JsonObject task) {
		File file = new File(getPrivatePath() + fileName);
		if (!file.exists())
			download(future, fileName, task.getString(DistributedVerticle.IP));
		else {
			System.out.println("File : " + fileName + " is already dowloaded");
			future.complete();	
		}
	}

	@Override
	protected void handleResult(AsyncResult<Object> result, String fileName) {
		if (result.failed()) {
			System.out.println(result.cause());
			throw new IllegalStateException(result.cause());
		} else {
			System.out.println("Download successful " + fileName);
			long id = System.currentTimeMillis();
			getMessages().add(new JsonObject().put(DistributedVerticle.ID, id).put("task", new JsonObject().put(DistributedVerticle.ID, id)
					.put(DistributedVerticle.FILENAME, fileName).put(DistributedVerticle.IP, getIp()).put(DistributedVerticle.TYPE, PdfConverterVerticle.ACTION)));
		}
	}

	private <T> void download(Future<T> future, String fileName, String ip) {
		// System.out.println("Execute blocking task : " +
		// Thread.currentThread());
		BlockingQueue<byte[]> blockingQueue = new ArrayBlockingQueue<>(1);
		HttpClient httpClient = vertx.createHttpClient().getNow(8084, ip, fileName, resp -> resp.bodyHandler(body -> {
			// System.out.println("get Bytes : " + Thread.currentThread());
			try {
				blockingQueue.put(body.getBytes());
			} catch (InterruptedException e) {
				future.fail(e);
				return;
			}
		}));
		httpClient.close();
		byte[] bytes;
		try {
			bytes = blockingQueue.take();
		} catch (InterruptedException e) {
			e.printStackTrace();
			future.fail(e);
			return;
		}
		FileOutputStream fos;

		try {
			File file = new File(getPrivatePath() + fileName);
			new File(getPrivatePath() + fileName.substring(0, fileName.lastIndexOf("/"))).mkdirs();
			fos = new FileOutputStream(file);
			fos.write(bytes);
			fos.close();
		} catch (IOException e) {
			e.printStackTrace();
			future.fail(e);
			return;
		}
		// System.out.println("Blocking task complete on thread : " +
		// Thread.currentThread());
		future.complete();
	}
}
