package org.genericsystem.cache;

import io.vertx.core.buffer.Buffer;
import io.vertx.core.http.HttpClient;
import io.vertx.core.http.HttpClientOptions;
import io.vertx.core.http.HttpClientRequest;

import org.genericsystem.api.core.exceptions.ConcurrencyControlException;
import org.genericsystem.api.core.exceptions.OptimisticLockConstraintViolationException;

public class HttpGSClient extends AbstractGSClient {

	private final HttpClient httpClient;
	private final String path;

	HttpGSClient(ClientEngine engine, String host, int port, String path) {
		super(engine);
		httpClient = GSVertx
				.vertx()
				.getVertx()
				.createHttpClient(
						new HttpClientOptions()
								.setDefaultPort(port)
								.setDefaultHost(
										host != null ? host
												: HttpClientOptions.DEFAULT_DEFAULT_HOST));
		this.path = path;
	}

	// private int status;

	@Override
	void send(Buffer buffer) {
		HttpClientRequest resquest = httpClient
				.post(path,
						reponse -> {
							if (reponse.statusCode() != 200) {
								System.out.println("An exception as occured: "
										+ reponse.statusCode());
								try {
									blockingQueue
											.put(reponse.statusCode() == 400 ? new ConcurrencyControlException(
													"")
													: new OptimisticLockConstraintViolationException(
															""));
								} catch (Exception e1) {
									// TODO Auto-generated catch block
									e1.printStackTrace();
								}
							} else
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
