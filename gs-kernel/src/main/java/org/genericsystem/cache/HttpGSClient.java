package org.genericsystem.cache;

import io.vertx.core.Vertx;
import io.vertx.core.buffer.Buffer;
import io.vertx.core.http.HttpClient;
import io.vertx.core.http.HttpClientOptions;
import io.vertx.core.http.HttpClientRequest;

public class HttpGSClient extends AbstractGSClient {

	private final HttpClient httpClient;
	private final String path;

	HttpGSClient(Vertx vertx, ClientEngine engine, String host, int port, String path) {
		super(engine);
		httpClient = vertx.createHttpClient(new HttpClientOptions().setDefaultPort(port).setDefaultHost(host != null ? host : HttpClientOptions.DEFAULT_DEFAULT_HOST));
		this.path = path;
	}

	@Override
	void send(Buffer buffer) {
		HttpClientRequest resquest = httpClient.post(path, reponse -> {
			reponse.bodyHandler(getHandler());
		});
		resquest.exceptionHandler(e -> {
			System.out.println("Discard http request because of : " + e);
		});
		resquest.end(buffer);
	}

	@Override
	public void close() {
		httpClient.close();
	}
}
