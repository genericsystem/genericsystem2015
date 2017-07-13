package org.genericsystem.cv.model;

import org.genericsystem.api.core.annotations.InstanceClass;
import org.genericsystem.api.core.annotations.SystemGeneric;
import org.genericsystem.common.Generic;
import org.genericsystem.cv.model.Doc.DocInstance;
import org.genericsystem.cv.model.DocClass.DocClassInstance;
import org.genericsystem.cv.model.ZoneGeneric.ZoneH;
import org.genericsystem.cv.model.ZoneGeneric.ZoneInstance;
import org.genericsystem.cv.model.ZoneGeneric.ZoneW;
import org.genericsystem.cv.model.ZoneGeneric.ZoneX;
import org.genericsystem.cv.model.ZoneGeneric.ZoneY;
import org.opencv.core.Point;

/**
 * This class stores the document class.
 * 
 * @author Jean Mathorel
 * @author Pierrik Lassalas
 *
 */
@SystemGeneric
@InstanceClass(DocClassInstance.class)
public class DocClass implements Generic {

	public static class DocClassInstance implements Generic {
		
		public ZoneInstance setZone(int num, int x, int y, int width, int height) {
			ZoneInstance zoneInstance = (ZoneInstance) setHolder(getRoot().find(ZoneGeneric.class), num);
			zoneInstance.setHolder(getRoot().find(ZoneX.class), x);
			zoneInstance.setHolder(getRoot().find(ZoneY.class), y);
			zoneInstance.setHolder(getRoot().find(ZoneW.class), width);
			zoneInstance.setHolder(getRoot().find(ZoneH.class), height);
			return zoneInstance;
		}
		
		public ZoneInstance setZone(int num, Point tl, Point br) {
			ZoneInstance zoneInstance = (ZoneInstance) setHolder(getRoot().find(ZoneGeneric.class), num);
			int x = ((Double) tl.x).intValue();
			int y = ((Double) tl.y).intValue();
			int width = ((Double) br.x).intValue() - x;
			int height = ((Double) br.y).intValue() - y;
			zoneInstance.setHolder(getRoot().find(ZoneX.class), x);
			zoneInstance.setHolder(getRoot().find(ZoneY.class), y);
			zoneInstance.setHolder(getRoot().find(ZoneW.class), width);
			zoneInstance.setHolder(getRoot().find(ZoneH.class), height);
			return zoneInstance;
		}

		public DocInstance setDoc(Generic docInstance, String filename){
			return (DocInstance) setHolder(docInstance, filename);
		}
		
		public DocInstance getDoc(Generic docInstance, String filename){
			return (DocInstance) getHolder(docInstance, filename);
		}

		public ZoneInstance getZone(int num) {
			return (ZoneInstance) getHolder(getRoot().find(ZoneGeneric.class), num);
		}

	}

	public DocClassInstance setDocClass(String name) {
		return (DocClassInstance) setInstance(name);
	}

	public DocClassInstance getDocClass(String name) {
		return (DocClassInstance) getInstance(name);
	}

}
