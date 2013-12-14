package edu.umass.vde;

import java.awt.Dimension;
import javax.swing.JComponent;

/**
 *
 * @author jfoley
 */
public class Utils {
  public static void setMinSize(JComponent comp, int width, int height) {
    Dimension d = new Dimension(width, height);
    comp.setMinimumSize(d);
    comp.setPreferredSize(d);
  }
}
