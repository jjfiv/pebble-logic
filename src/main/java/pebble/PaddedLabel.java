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

  private void init() {
        this.setBorder(BorderFactory.createEmptyBorder(5, 5, 0, 5));

  }
  public PaddedLabel(String text) {
    super(text);
    init();
  }

  public PaddedLabel(String text, Color color) {
    super(text);
    this.setForeground(color);
    init();
  }

  public PaddedLabel(BufferedImage image) {
    super(new ImageIcon(image));
    init();
  }
  
  public PaddedLabel(byte[] data) {
    super(new ImageIcon(data));
    init();
  }
  
}
