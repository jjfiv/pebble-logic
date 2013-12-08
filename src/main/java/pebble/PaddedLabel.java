package pebble;

import java.awt.Color;
import java.awt.Dimension;
import java.awt.image.BufferedImage;
import javax.swing.BorderFactory;
import javax.swing.ImageIcon;
import javax.swing.JLabel;

/**
 *
 * @author jfoley
 */
public class PaddedLabel extends JLabel {

  public PaddedLabel(String text) {
    super(text);
    this.setBorder(BorderFactory.createEmptyBorder(5, 5, 0, 5));
  }

  public PaddedLabel(String text, Color color) {
    this(text);
    this.setForeground(color);
  }

  public PaddedLabel(BufferedImage image) {
    super(new ImageIcon(image));
    Dimension dim = new Dimension(image.getWidth(), image.getHeight());
    this.setMinimumSize(dim);
    this.setPreferredSize(dim);
  }
  
}
