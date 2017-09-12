package org.genericsystem.cv.docPattern;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.opencv.core.Rect;

public class ZoneStreamOrganizer {

	public static void main(String[] args) {

		List<ZoneStream> zoneStreams = new ArrayList<>();

		String label1[] = { "First Name :", "First Name :", "First Name :", "First Name :", "First Name :" };
		List<String> labelList1 = Arrays.asList(label1);
		ZoneStream zs1 = new ZoneStream(new Rect(100, 100, 500, 100), labelList1);
		zoneStreams.add(zs1);

		String label2[] = { "John", "Jane", "Pat", "Fred", "Daisy" };
		List<String> labelList2 = Arrays.asList(label2);
		ZoneStream zs2 = new ZoneStream(new Rect(700, 100, 500, 100), labelList2);
		zoneStreams.add(zs2);

		String label3[] = { "Sex :", "Sex :", "Sex :", "Sex :", "Sex :" };
		List<String> labelList3 = Arrays.asList(label3);
		ZoneStream zs3 = new ZoneStream(new Rect(100, 300, 500, 100), labelList3);
		zoneStreams.add(zs3);

		String label4[] = { "M", "W", "M", "M", "W" };
		List<String> labelList4 = Arrays.asList(label4);
		ZoneStream zs4 = new ZoneStream(new Rect(700, 300, 500, 100), labelList4);
		zoneStreams.add(zs4);

		String label5[] = { "Age :", "Age :", "Age :", "Age :", "Age :" };
		List<String> labelList5 = Arrays.asList(label5);
		ZoneStream zs5 = new ZoneStream(new Rect(100, 500, 500, 100), labelList5);
		zoneStreams.add(zs5);

		String label6[] = { "12", "11", "16", "18", "15" };
		List<String> labelList6 = Arrays.asList(label6);
		ZoneStream zs6 = new ZoneStream(new Rect(700, 500, 500, 100), labelList6);
		zoneStreams.add(zs6);

		DocPattern pattern = new DocPattern("testDoc", zoneStreams);

		System.out.println(zs1.getType());
		System.out.println(zs1.getAuthorizedValues());
		System.out.println(zs2.getType());
		System.out.println(zs2.getAuthorizedValues());
		System.out.println(zs3.getType());
		System.out.println(zs3.getAuthorizedValues());
		System.out.println(zs4.getType());
		System.out.println(zs4.getAuthorizedValues());
		System.out.println(zs5.getType());
		System.out.println(zs5.getAuthorizedValues());
		System.out.println(zs6.getType());
		System.out.println(zs6.getAuthorizedValues());

	}

}
