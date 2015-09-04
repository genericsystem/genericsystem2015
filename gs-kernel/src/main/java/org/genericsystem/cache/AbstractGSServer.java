package org.genericsystem.cache;

import io.vertx.core.Handler;
import io.vertx.core.buffer.Buffer;
import java.util.Arrays;
import java.util.HashSet;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.function.Consumer;
import java.util.stream.Collectors;
import org.genericsystem.kernel.Root;
import org.genericsystem.kernel.Statics;

public abstract class AbstractGSServer {

	protected Map<String, Root> roots;

	public AbstractGSServer(Root... roots) {
		this.roots = Arrays.stream(roots).collect(Collectors.toMap(root -> "/" + root.getValue(), root -> root));
		assert roots.length == this.roots.size();
	}

	public AbstractGSServer(GSDeploymentOptions options) {
		this(getRoots(options));
	}

	private static Root[] getRoots(GSDeploymentOptions options) {
		Set<Root> roots = new HashSet<>();
		if (options.getEngines().isEmpty()) {
			Root defaultRoot = new Root(Statics.ENGINE_VALUE, null, new Class<?>[] {});
			roots.add(defaultRoot);
			System.out.println("Starts engine : " + "/" + Statics.ENGINE_VALUE);
		} else
			for (Entry<String, String> entry : options.getEngines().entrySet()) {
				roots.add(new Root(entry.getKey(), entry.getValue(), options.getClasses()));
				System.out.println("Starts engine : " + "/" + entry.getKey());
			}
		return roots.toArray(new Root[roots.size()]);
	}

	public void stop() {
		roots.values().forEach(root -> root.close());
	}

	protected Handler<Buffer> getHandler(Root root, Consumer<Buffer> sender, Consumer<Exception> exceptionSender) {
		return buffer -> {
			// System.out.println(Thread.currentThread());
			GSBuffer gsBuffer = new GSBuffer(buffer);
			int methodId = gsBuffer.getInt();
			GSBuffer replyBuffer = new GSBuffer(Buffer.buffer());
			replyBuffer.appendInt(methodId);
			switch (methodId) {
			case AbstractGSClient.PICK_NEW_TS: {
				replyBuffer.appendLong(root.pickNewTs());
				break;
			}
			case AbstractGSClient.GET_DEPENDENCIES: {
				replyBuffer.appendGSLongArray(root.getDependencies(gsBuffer.getLong(), gsBuffer.getLong()));
				break;
			}
			case AbstractGSClient.GET_VERTEX: {
				replyBuffer.appendGSVertex(root.getVertex(gsBuffer.getLong()));
				break;
			}
			case AbstractGSClient.APPLY: {
				try {
					root.apply(gsBuffer.getLong(), gsBuffer.getGSLongArray(), gsBuffer.getGSVertexArray());
					replyBuffer.appendLong(0);
				} catch (Exception e) {
					e.printStackTrace();
					exceptionSender.accept(e);
					return;
				}
				break;
			}
			default:
				throw new IllegalStateException("unable to find method:" + methodId + " ");
			}
			sender.accept(replyBuffer);
		};
	}

}
