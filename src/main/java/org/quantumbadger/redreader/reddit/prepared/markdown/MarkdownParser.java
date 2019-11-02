/*******************************************************************************
 * This file is part of RedReader.
 *
 * RedReader is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * RedReader is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with RedReader.  If not, see <http://www.gnu.org/licenses/>.
 ******************************************************************************/

package org.quantumbadger.redreader.reddit.prepared.markdown;

import java.util.ArrayList;

import static java.lang.Math.abs;

public final class MarkdownParser {

	public enum MarkdownParagraphType {
		TEXT, CODE, BULLET, NUMBERED, QUOTE, HEADER, HLINE, EMPTY
	}

	public static MarkdownParagraphGroup parse(final char[] raw) {

		final CharArrSubstring[] rawLines = CharArrSubstring.generateFromLines(raw);

		final MarkdownLine[] lines = new MarkdownLine[rawLines.length];
		CharArrSubstring[] currTable = new CharArrSubstring[0];
		int tableStart = -1;//we let -1 as no tables being detected.
		int lastTableindex = -1; // this is in case that we find '|' but it is not a table
		for(int i = 0; i < rawLines.length; i++) {
			//Trying to get all of the elements of the array
			if( (i > lastTableindex) && (rawLines[i].toString().contains("|"))){
				CharArrSubstring[] newTable = new CharArrSubstring[currTable.length+1];
				for (int j = 0; j < currTable.length;j++){
					newTable[j] = currTable[j];
				}
				newTable[currTable.length] = rawLines[i];
				currTable = newTable;
				if (tableStart == -1) tableStart = i;
				continue;
			}
			//this means that we have scanned all of the lines that def the table.
			// and now we need to build the table.
			if((i > lastTableindex) && (currTable.length > 0)){
				lastTableindex = i;
				boolean isTable = false;
				//Do some parsing
				//AS Per https://www.reddit.com/wiki/markdown#wiki_tables
				//We are expecting the 2nd line to be the alignment if it does not exist then it
				// is an invalid table so we ignore it.
				// we do this by a REGEX ofc.
				if((currTable.length > 2) && (currTable[1].toString().matches("^([-|:]|\\|:)-*:?\\|(?!$)(:?-+:?\\|?(?!$))*:?-+:?\\|?$"))){
					//this means that we got the right Data alignment row
					//first we need to make sure that we have the same number of cols in each line
					//we do this by making sure that all lines have the | in first and last char
					String[] tableRaw = new String[currTable.length];
					for (int j = 0; j < tableRaw.length;j++){
						String entry  = currTable[j].toString().trim();
						if(entry.charAt(0) != '|'){
							entry = '|'+entry;
						}
						if(entry.charAt(entry.length()-1) != '|'){
							entry = entry+'|';
						}
						tableRaw[j] = entry;
					}
					//now we make sure that all lines contain the same number of cols.
					int expectedCols = tableRaw[0].split("[|]").length;
					boolean isSameNumberOfCols = true;
					for(int j = 0; j < tableRaw.length;j++){
						if(expectedCols != tableRaw[j].split("[|]").length){
							isSameNumberOfCols = false;
						}
					}
					if(isSameNumberOfCols){
						//this means that the table is consistent
						//So now we find the alignment for each cols.
						//0 is for centered -1 for left aligned and 1 for right aligned.
						int[] alignments = new int[expectedCols-1];
						String[] row2 = tableRaw[1].split("[|]");
						for(int j =1; j < expectedCols; j++){
							if(row2[j].matches(":-+")){alignments[j-1] = -1;}
							else if(row2[j].matches("-+:")){alignments[j-1] = 1;}
							else alignments[j-1] = 0;
						}
						//now that we have the alignment for each col we need to find the max size
						// of each col and pad accordingly
						int[] maxStrings = new int[expectedCols-1];//assuming that the array is all 0's
						for(int j = 0; j < tableRaw.length;j++){
							String[] cols = tableRaw[j].split("[|]");
							for(int k = 0; k < maxStrings.length;k++){
								if(maxStrings[k] < cols[k+1].length())//todo: try and extract the true string len i.e. not with the markdown chars
									maxStrings[k] = cols[k+1].length();
							}
						}
						//Finally we pad the entries with given alignments
						String[] finalTable = new String[tableRaw.length-1];
						for(int j = 0; j < tableRaw.length;j++){
							String[] cols = tableRaw[j].split("[|]");
							String result = "|";
							if(j == 1) continue;
							for(int k = 0; k < maxStrings.length;k++){
								int padSize = abs(maxStrings[k]-cols[k+1].length())+2;
								 switch (alignments[k]){
									 case -1: //left aligned
										 result = result+new String(new char[padSize]).replace("\0", " ")+cols[k+1]+"|";
										 continue;
									 case 1:
										 result = result+cols[k+1]+new String(new char[padSize]).replace("\0", " ")+"|";
										 continue;
									 case 0:
										 result = result+new String(new char[padSize/2]).replace("\0", " ")+cols[k+1]+new String(new char[padSize/2]).replace("\0", " ")+"|";//I know that in case of odd padSizes the alignment will be off by one this for simplicty sake
										 continue;
								 }
								 result = result+'|';
							}
							if(j == 1)continue;
							if(j > 1) finalTable[j-1] = result;
							else finalTable[j] = result;

						}
						//Now we override the values in the original rawLines and thats it
						//todo find a proper way to update start accordingly
						//Please note that the alignment row is expected to be not rendered in the end result
						//we handle the "Header" and alignment rows outside of the loop to not make a web of ifs in the for loop
						lines[tableStart] = MarkdownLine.generate(CharArrSubstring.generate(finalTable[0].toCharArray()));
						lines[tableStart+1] = MarkdownLine.generate(CharArrSubstring.generate("".toCharArray()));
						for (int j = 1; j<finalTable.length;j++){
							lines[tableStart+1+j] = MarkdownLine.generate(CharArrSubstring.generate((finalTable[j]+"   ").toCharArray()));
						}
						isTable = true;
					}
				}
				//Finaly empty currTable
				currTable = new CharArrSubstring[0];
				if(!isTable)i = tableStart;
				tableStart = -1;
			}
			lines[i] = MarkdownLine.generate(rawLines[i]);
		}
		//case that the table is the last element in the paragraph.
		if((currTable.length > 0)) {
			//Do some parsing
			//AS Per https://www.reddit.com/wiki/markdown#wiki_tables
			//We are expecting the 2nd line to be the alignment if it does not exist then it
			// is an invalid table so we ignore it.
			// we do this by a REGEX ofc.
			if (currTable[1].toString().matches("^([-|:]|\\|:)-*:?\\|(?!$)(:?-+:?\\|?(?!$))*:?-+:?\\|?$")) {
				//this means that we got the right Data alignment row
				//first we need to make sure that we have the same number of cols in each line
				//we do this by making sure that all lines have the | in first and last char
				String[] tableRaw = new String[currTable.length];
				for (int j = 0; j < tableRaw.length; j++) {
					String entry = currTable[j].toString().trim();
					if (entry.charAt(0) != '|') {
						entry = '|' + entry;
					}
					if (entry.charAt(entry.length() - 1) != '|') {
						entry = entry + '|';
					}
					tableRaw[j] = entry;
				}
				//now we make sure that all lines contain the same number of cols.
				int expectedCols = tableRaw[0].split("[|]").length;
				boolean isSameNumberOfCols = true;
				for (int j = 0; j < tableRaw.length; j++) {
					if (expectedCols != tableRaw[j].split("[|]").length) {
						isSameNumberOfCols = false;
					}
				}
				if (isSameNumberOfCols) {
					//this means that the table is consistent
					//So now we find the alignment for each cols.
					//0 is for centered -1 for left aligned and 1 for right aligned.
					int[] alignments = new int[expectedCols - 1];
					String[] row2 = tableRaw[1].split("[|]");
					for (int j = 1; j < expectedCols; j++) {
						if (row2[j].matches(":-+")) {
							alignments[j - 1] = -1;
						} else if (row2[j].matches("-+:")) {
							alignments[j - 1] = 1;
						} else alignments[j - 1] = 0;
					}
					//now that we have the alignment for each col we need to find the max size
					// of each col and pad accordingly
					int[] maxStrings = new int[expectedCols - 1];//assuming that the array is all 0's
					for (int j = 0; j < tableRaw.length; j++) {
						String[] cols = tableRaw[j].split("[|]");
						for (int k = 0; k < maxStrings.length; k++) {
							if (maxStrings[k] < cols[k + 1].length())//todo: try and extract the true string len i.e. not with the markdown chars
								maxStrings[k] = cols[k + 1].length();
						}
					}
					//Finally we pad the entries with given alignments
					String[] finalTable = new String[tableRaw.length - 1];
					for (int j = 0; j < tableRaw.length; j++) {
						String[] cols = tableRaw[j].split("[|]");
						String result = "|";
						if (j == 1) continue;
						for (int k = 0; k < maxStrings.length; k++) {
							int padSize = abs(maxStrings[k] - cols[k + 1].length()) + 2;
							switch (alignments[k]) {
								case -1: //left aligned
									result = result + new String(new char[padSize]).replace("\0", " ") + cols[k + 1] + "|";
									continue;
								case 1:
									result = result + cols[k + 1] + new String(new char[padSize]).replace("\0", " ") + "|";
									continue;
								case 0:
									result = result + new String(new char[padSize / 2]).replace("\0", " ") + cols[k + 1] + new String(new char[padSize / 2]).replace("\0", " ") + "|";//I know that in case of odd padSizes the alignment will be off by one this for simplicty sake
									continue;
							}
							result = result + '|';
						}
						if (j == 1) continue;
						if (j > 1) finalTable[j - 1] = result;
						else finalTable[j] = result;

					}
					//Now we override the values in the original rawLines and thats it
					//todo find a proper way to update start accordingly
					//Please note that the alignment row is expected to be not rendered in the end result
					//we handle the "Header" and alignment rows outside of the loop to not make a web of ifs in the for loop
					lines[tableStart] = MarkdownLine.generate(CharArrSubstring.generate(finalTable[0].toCharArray()));
					lines[tableStart + 1] = MarkdownLine.generate(CharArrSubstring.generate("".toCharArray()));
					for (int j = 1; j < finalTable.length; j++) {
						lines[tableStart + 1 + j] = MarkdownLine.generate(CharArrSubstring.generate((finalTable[j] + "   ").toCharArray()));
					}
				}
			}
		}
		final ArrayList<MarkdownLine> mergedLines = new ArrayList<>(rawLines.length);
		MarkdownLine currentLine = null;

		for(int i = 0; i < lines.length; i++) {

			if(currentLine != null) {

				switch(lines[i].type) {
					case BULLET:
					case NUMBERED:
					case HEADER:
					case CODE:
					case HLINE:
					case QUOTE:

						mergedLines.add(currentLine);
						currentLine = lines[i];
						break;

					case EMPTY:
						mergedLines.add(currentLine);
						currentLine = null;
						break;

					case TEXT:

						if(i < 1) {
							throw new RuntimeException("Internal error: invalid paragrapher state");
						}

						switch(lines[i - 1].type) {
							case QUOTE:
							case BULLET:
							case NUMBERED:
							case TEXT:

								if(lines[i - 1].spacesAtEnd >= 2) {
									mergedLines.add(currentLine);
									currentLine = lines[i];

								} else {
									currentLine = currentLine.rejoin(lines[i]);
								}
								break;

							case CODE:
							case HEADER:
							case HLINE:
								mergedLines.add(currentLine);
								currentLine = lines[i];
								break;
						}

						break;
				}
			} else if(lines[i].type != MarkdownParagraphType.EMPTY) {
				currentLine = lines[i];
			}
		}

		if(currentLine != null) {
			mergedLines.add(currentLine);
		}

		final ArrayList<MarkdownParagraph> outputParagraphs = new ArrayList<>(mergedLines.size());

		for(final MarkdownLine line : mergedLines) {
			final MarkdownParagraph lastParagraph = outputParagraphs.isEmpty() ? null : outputParagraphs.get(outputParagraphs.size() - 1);
			final MarkdownParagraph paragraph = line.tokenize(lastParagraph);
			if(!paragraph.isEmpty()) outputParagraphs.add(paragraph);
		}

		return new MarkdownParagraphGroup(outputParagraphs.toArray(new MarkdownParagraph[outputParagraphs.size()]));
	}
}
