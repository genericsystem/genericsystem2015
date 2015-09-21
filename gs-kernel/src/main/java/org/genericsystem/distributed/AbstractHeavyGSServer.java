package org.genericsystem.distributed;

import io.vertx.core.buffer.Buffer;

import org.genericsystem.common.Vertex;
import org.genericsystem.kernel.HeavyServerEngine;

public abstract class AbstractHeavyGSServer extends AbstractGSServer {
	public AbstractHeavyGSServer(GSDeploymentOptions options) {
		super(options);
	}

	Buffer getReplyBuffer(int methodId, HeavyServerEngine root, GSBuffer gsBuffer) {
		GSBuffer replyBuffer = new GSBuffer(Buffer.buffer());
		switch (methodId) {
		case AbstractGSClient.PICK_NEW_TS:
			return replyBuffer.appendLong(root.pickNewTs());
		case AbstractGSClient.GET_DEPENDENCIES:
			return replyBuffer.appendGSVertexArray(root.getDependencies(gsBuffer.getLong(), gsBuffer.getLong()));
		case AbstractGSClient.SHIFT_TS:
			return replyBuffer.appendLong(root.shiftTs(gsBuffer.getLong()));
		case AbstractGSClient.ADD_INSTANCE: {
			long cacheId = gsBuffer.getLong();
			Vertex vertex = gsBuffer.getGSVertex();
			return replyBuffer.appendGSVertex(root.addInstance(cacheId, vertex.getMeta(), vertex.getSupers(), vertex.getValue(), vertex.getComponents()));
		}
		case AbstractGSClient.SET_INSTANCE: {
			long cacheId = gsBuffer.getLong();
			Vertex vertex = gsBuffer.getGSVertex();
			return replyBuffer.appendLong(root.setInstance(cacheId, vertex.getMeta(), vertex.getSupers(), vertex.getValue(), vertex.getComponents()));
		}
		case AbstractGSClient.MERGE: {
			long cacheId = gsBuffer.getLong();
			Vertex vertex = gsBuffer.getGSVertex();
			return replyBuffer.appendLong(root.merge(cacheId, vertex.getMeta(), vertex.getSupers(), vertex.getValue(), vertex.getComponents()));
		}
		case AbstractGSClient.UPDATE: {
			long cacheId = gsBuffer.getLong();
			Vertex vertex = gsBuffer.getGSVertex();
			return replyBuffer.appendGSVertex(root.update(cacheId, vertex.getMeta(), vertex.getSupers(), vertex.getValue(), vertex.getComponents()));
		}
		case AbstractGSClient.REMOVE:
			return replyBuffer.appendLong(root.remove(gsBuffer.getLong(), gsBuffer.getLong()));
		case AbstractGSClient.FORCE_REMOVE:
			return replyBuffer.appendLong(root.forceRemove(gsBuffer.getLong(), gsBuffer.getLong()));
		case AbstractGSClient.CONSERVE_REMOVE:
			return replyBuffer.appendLong(root.conserveRemove(gsBuffer.getLong(), gsBuffer.getLong()));
		case AbstractGSClient.TRY_FLUSH:
			return replyBuffer.appendLong(root.tryFlush(gsBuffer.getLong()));
		case AbstractGSClient.FLUSH:
			return replyBuffer.appendLong(root.flush(gsBuffer.getLong()));
		case AbstractGSClient.MOUNT:
			return replyBuffer.appendLong(root.mount(gsBuffer.getLong()));
		case AbstractGSClient.UNMOUNT:
			return replyBuffer.appendLong(root.unmount(gsBuffer.getLong()));
		case AbstractGSClient.GET_CACHE_LEVEL:
			return replyBuffer.appendInt(root.getCacheLevel(gsBuffer.getLong()));

			// case AbstractGSClient.APPLY:
			// try {
			// root.apply(gsBuffer.getLong(), gsBuffer.getGSLongArray(), gsBuffer.getGSVertexArray());
			// return replyBuffer.appendLong(0);
			// } catch (Exception e) {
			// e.printStackTrace();
			// return replyBuffer.appendLong(e instanceof ConcurrencyControlException ? Statics.CONCURRENCY_CONTROL_EXCEPTION : Statics.OTHER_EXCEPTION);
			// }
		default:
			throw new IllegalStateException("unable to find method:" + methodId + " ");
		}
	}
}
