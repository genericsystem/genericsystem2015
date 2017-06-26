package org.genericsystem.watch.beta;


import java.util.Iterator;

import org.genericsystem.api.core.Snapshot;
import org.genericsystem.common.Generic;
import org.genericsystem.kernel.Cache;
import org.genericsystem.kernel.Engine;

import io.vertx.core.AbstractVerticle;
import io.vertx.core.eventbus.MessageConsumer;

public class VerticleB extends AbstractVerticle{

	public static void main(String[] args) {
		App_Flex.deployVerticle(new VerticleB());
	}


	private final Engine engineB = new Engine("/home/middleware/ymoumna/VertB/",Task.class,State.class,Message.class);
	private Cache cache = engineB.newCache();

	@Override
	public void start() throws Exception {

		System.out.println("Verticle B");

		MessageConsumer<String> consumer = vertx.eventBus().consumer(App_Flex.STEP1);
		consumer.handler(message-> vertx.executeBlocking(future ->{
			cache.safeConsum(nothing -> {
				
				System.out.println("Verticle B : handling event");

				Generic task = engineB.find(Task.class);			
				Generic mytask = task.addInstance("task : "+System.currentTimeMillis());
				mytask.addHolder(engineB.find(Message.class), message.body());
				mytask.addHolder(engineB.find(State.class), 0);
				
				engineB.getCurrentCache().flush();


				System.out.println("Verticle B : handled event");
				System.out.println("###########################################################################");
				
				mytask.setHolder(engineB.find(State.class), 1);
				
				vertx.eventBus().publish(App_Flex.STEP2, "1000");
				System.gc();
				System.runFinalization();

				Snapshot<Generic> tasks = engineB.find(Task.class).getSubInstances();
				Iterator<Generic> it = tasks.iterator();

				System.out.println("persisted tasks");
				while(it.hasNext()){
					System.out.println(it.next());
				}
				System.out.println("--------------------------------------------------------------------------");			
				future.complete();
			});

		}					

		, res -> {
			if (res.failed())
				throw new IllegalStateException(res.cause());
		}));

	}
}
