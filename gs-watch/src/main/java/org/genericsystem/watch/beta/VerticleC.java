package org.genericsystem.watch.beta;

import org.genericsystem.kernel.Engine;

import io.vertx.core.AbstractVerticle;
import io.vertx.core.eventbus.MessageConsumer;

public class VerticleC extends AbstractVerticle{

	
	public static void main(String[] args) {
		App_Flex.deployVerticle(new VerticleC());
	}
	
	@Override
	public void start() throws Exception {
		
		Engine engineB = new Engine("/home/middleware/ymoumna/VertC/",Task.class,State.class);
		System.out.println("Verticle C");
		MessageConsumer<String> consumer = vertx.eventBus().consumer(App_Flex.STEP2);
		consumer.handler(message-> vertx.executeBlocking(future ->{
			
			System.out.println("Verticle C : handling event");
			try {
				Thread.sleep(Long.parseLong(message.body()));
			} catch (NumberFormatException | InterruptedException e) {
				e.printStackTrace();
			}
			
			System.out.println("Verticle C : handled event");
			System.out.println("###############################################################################################");
			
			System.gc();
			System.runFinalization();
			future.complete();
		}					
				
				, res -> {
			if (res.failed())
				throw new IllegalStateException(res.cause());
		}));
	}
}
