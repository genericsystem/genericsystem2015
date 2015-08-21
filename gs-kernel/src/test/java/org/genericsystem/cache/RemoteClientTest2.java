package org.genericsystem.cache;

import io.vertx.core.Vertx;
import io.vertx.core.buffer.Buffer;
import io.vertx.core.http.WebSocket;

import java.util.concurrent.CountDownLatch;

import org.testng.annotations.BeforeClass;
import org.testng.annotations.Test;

public class RemoteClientTest2 extends AbstractTest {

	public static void main(String[] args) {
		Vertx.vertx().createHttpServer()
				.websocketHandler(ws -> ws.handler(buffer -> {
					System.out.println("Server : " + i);
					ws.writeBinaryMessage(buffer);
				})).requestHandler(req -> {
				}).listen(8081);
	}

	WebSocket[] webSocketArray = new WebSocket[1];

	CountDownLatch cdlt;

	@BeforeClass
	public void beforeClass() {
		CountDownLatch cdl = new CountDownLatch(1);
		Vertx.vertx().createHttpClient()
				.websocket(8081, "localhost", "/some-uri", websocket -> {
					websocket.handler(data -> {
						System.out.println(i++);
						cdlt.countDown();
					});
					webSocketArray[0] = websocket;
					cdl.countDown();
				});
		try {
			cdl.await();
		} catch (InterruptedException e) {
			e.printStackTrace();
		}
	}

	static int i = 0;

	@Test(invocationCount = 100)
	public void test_001() {
		cdlt = new CountDownLatch(1);
		webSocketArray[0].writeBinaryMessage(Buffer.buffer("Hello world"));
		try {
			cdlt.await();
		} catch (InterruptedException e) {
			e.printStackTrace();
		}
	}

}
