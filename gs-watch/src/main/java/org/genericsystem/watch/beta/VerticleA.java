package org.genericsystem.watch.beta;

import org.genericsystem.kernel.Engine;

import io.vertx.core.AbstractVerticle;
import io.vertx.core.Handler;


public class VerticleA extends AbstractVerticle{

	public static void main(String[] args) {
		App_Flex.deployVerticle(new VerticleA());
	}
	
	@Override
	public void start() throws Exception {
		
		System.out.println("Verticle A");
		
		vertx.setPeriodic(10000, new Handler<Long>() {
			
			@Override
			public void handle(Long event) {
								
				System.out.println("Verticle A publishing");
				vertx.eventBus().publish(App_Flex.STEP1, "Message : "+System.currentTimeMillis());
			}
		});
			
		
	}
}
