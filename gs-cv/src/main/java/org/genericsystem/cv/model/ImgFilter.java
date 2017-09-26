package org.genericsystem.cv.model;

import org.genericsystem.api.core.Snapshot;
import org.genericsystem.api.core.annotations.InstanceClass;
import org.genericsystem.api.core.annotations.SystemGeneric;
import org.genericsystem.common.Generic;
import org.genericsystem.cv.model.ImgFilter.ImgFilterInstance;

/**
 * This class stores the names of the filters that were applied to an image.
 * 
 * @author Jean Mathorel
 * @author Pierrik Lassalas
 */
@SystemGeneric
@InstanceClass(ImgFilterInstance.class)
public class ImgFilter implements Generic {

	public static class ImgFilterInstance implements Generic {

	}

	@SuppressWarnings({ "unchecked", "rawtypes" })
	public Snapshot<ImgFilterInstance> getImgFilters() {
		return (Snapshot) getInstances();
	}

	public ImgFilterInstance setImgFilter(String name) {
		return (ImgFilterInstance) setInstance(name);
	}

	public ImgFilterInstance getImgFilter(String name) {
		return (ImgFilterInstance) getInstance(name);
	}

}
