package edu.umass.vde;

import java.awt.Dimension;
import javax.swing.JTextField;

/**
 *
 * @author jfoley
 */
public class MaxHeightTextField extends JTextField {

  @Override
  public Dimension getMinimumSize() {
    return getPreferredSize();
  }

  @Override
  public Dimension getMaximumSize() {
    int width = super.getMaximumSize().width;
    int height = this.getPreferredSize().height;
    return new Dimension(width, height);
  }
  
}
