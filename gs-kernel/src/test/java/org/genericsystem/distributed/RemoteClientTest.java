package org.genericsystem.distributed;
//package org.genericsystem.cache;
//
//import io.vertx.core.Vertx;
//import io.vertx.core.buffer.Buffer;
//import io.vertx.core.http.WebSocket;
//
//import java.util.concurrent.CountDownLatch;
//
//import org.testng.annotations.BeforeClass;
//import org.testng.annotations.Test;
//
//public class RemoteClientTest extends AbstractTest {
//
//	// @BeforeTest
//	// public void beforeClass() {
//	// HttpLocalGSServer.create();
//	//
//	// }
//
//	// private interface HttpLocalGSServer {
//	//
//	// public static void close() {
//	// Vertx.vertx().undeploy(WebSocketGSServer.class.getName());
//	// };
//	//
//	// public static void create(int port, String persistanceRepositoryPath) {
//	// Vertx.vertx().deployVerticle(WebSocketGSServer.class.getName(), new DeploymentOptions().setConfig(new JsonObject().put("port", port).put("persistanceRepositoryPath", persistanceRepositoryPath)));
//	// }
//	//
//	// public static void create() {
//	// create(Statics.DEFAULT_PORT, null);
//	// }
//	//
//	// }
//
//	// @AfterTest
//	// public void afterClass() {
//	// HttpLocalGSServer.close();
//	// }
//
//	public static void main(String[] args) {
//		Vertx.vertx().createHttpServer().websocketHandler(ws -> ws.handler(buffer -> {
//			System.out.println("Server : " + i);
//			ws.writeBinaryMessage(buffer);
//		})).requestHandler(req -> {
//		}).listen(8081);
//	}
//
//	WebSocket[] webSocketArray = new WebSocket[1];
//
//	CountDownLatch cdlt;
//
//	@BeforeClass
//	public void beforeClass() {
//		CountDownLatch cdl = new CountDownLatch(1);
//		Vertx.vertx().createHttpClient().websocket(8081, "localhost", "/some-uri", websocket -> {
//			websocket.handler(data -> {
//				System.out.println(i++);
//				cdlt.countDown();
//			});
//			webSocketArray[0] = websocket;
//			cdl.countDown();
//		});
//		try {
//			cdl.await();
//		} catch (InterruptedException e) {
//			e.printStackTrace();
//		}
//	}
//
//	static int i = 0;
//
//	@Test(invocationCount = 100)
//	public void test_001() {
//		cdlt = new CountDownLatch(1);
//		webSocketArray[0].writeBinaryMessage(Buffer.buffer("Hello world"));
//		try {
//			cdlt.await();
//		} catch (InterruptedException e) {
//			e.printStackTrace();
//		}
//	}
//
//	// System.out.println("Thread : " + System.identityHashCode(Thread.currentThread()));
//	// ClientEngine clientEngine = new ClientEngine("firstEngine");
//	// Thread thread = new Thread() {
//	// @Override
//	// public void run() {
//	// System.out.println("Thread : " + System.identityHashCode(Thread.currentThread()));
//	// ClientEngine clientEngine = new ClientEngine("firstEngine");
//	// }
//	// };
//	// thread.start();
//	// try {
//	// thread.join();
//	// } catch (InterruptedException e) {
//	// e.printStackTrace();
//	// }
//
//	// System.out.println("Thread : " + System.identityHashCode(Thread.currentThread()));
//	// ClientEngine clientEngine = new ClientEngine("firstEngine");
//	// ClientGeneric myVehicle = clientEngine.addInstance("Vehicle");
//	// clientEngine.getCurrentCache().flush();
//	// ClientEngine clientEngine2 = new ClientEngine("secondEngine");
//	// ClientGeneric mySecondVehicle = clientEngine2.getInstance("Vehicle");
//
// }
