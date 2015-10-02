package org.genericsystem.distributed;

import io.vertx.core.Handler;
import io.vertx.core.buffer.Buffer;
import io.vertx.core.http.HttpClient;
import io.vertx.core.http.HttpClientOptions;

public class HttpGSHeavyClient extends AbstractGSHeavyClient {

	private final HttpClient httpClient;
	private final String path;

	HttpGSHeavyClient(String host, int port, String path) {
		httpClient = GSVertx.vertx().getVertx().createHttpClient(new HttpClientOptions().setDefaultPort(port).setDefaultHost(host != null ? host : HttpClientOptions.DEFAULT_DEFAULT_HOST));
		this.path = path;
	}

	@Override
	<T> void send(Buffer buffer, Handler<Buffer> responseHandler) {
		httpClient.post(path, reponse -> reponse.bodyHandler(responseHandler)).exceptionHandler(e -> {
			System.out.println("Discard http request because of : ");
			e.printStackTrace();
		}).end(buffer);
	}

	@Override
	public void close() {
		try {
			httpClient.close();
			System.out.println("Close socket");
		} catch (Exception ignore) {
		}
	}
}
