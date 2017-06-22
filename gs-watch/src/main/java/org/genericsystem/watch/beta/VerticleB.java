package org.genericsystem.watch.beta;


import org.genericsystem.kernel.Engine;

import io.vertx.core.AbstractVerticle;
import io.vertx.core.eventbus.MessageConsumer;

public class VerticleB extends AbstractVerticle{

	public static void main(String[] args) {
		App_Flex.deployVerticle(new VerticleB());
	}
	
	@Override
	public void start() throws Exception {
		
		Engine engineB = new Engine("/home/middleware/ymoumna/VertB/",Task.class,State.class);
		System.out.println("Verticle B");
		MessageConsumer<String> consumer = vertx.eventBus().consumer(App_Flex.STEP1);
		consumer.handler(message-> vertx.executeBlocking(future ->{
		
			System.out.println("Verticle B : handling event");
			try {
				Long s = Long.parseLong(message.body(),10);
				Thread.sleep(s);
			} catch (NumberFormatException | InterruptedException e) {
				e.printStackTrace();
			}
			
			System.out.println("Verticle B : handled event");
			System.out.println("###########################################################################");
			vertx.eventBus().publish(App_Flex.STEP2, "1000");
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
