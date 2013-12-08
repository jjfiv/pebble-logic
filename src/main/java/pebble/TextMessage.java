package pebble;

import java.awt.Color;
import javax.swing.BorderFactory;
import javax.swing.JLabel;

/**
 *
 * @author jfoley
 */
public class TextMessage extends JLabel {

  public TextMessage(String text) {
    super(text);
    this.setBorder(BorderFactory.createEmptyBorder(5, 5, 0, 5));
  }

  public TextMessage(String text, Color color) {
    this(text);
    this.setForeground(color);
  }

  public static TextMessage error(String msg) {
    return new TextMessage("Error: " + msg, Color.RED);
  }
  
}
