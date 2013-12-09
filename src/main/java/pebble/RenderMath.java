package pebble;

import java.awt.Color;
import java.awt.Graphics2D;
import java.awt.Insets;
import java.awt.image.BufferedImage;
import javax.swing.JLabel;
import org.scilab.forge.jlatexmath.TeXConstants;
import org.scilab.forge.jlatexmath.TeXFormula;
import org.scilab.forge.jlatexmath.TeXIcon;

/**
 *
 * @author jfoley
 */
public class RenderMath {
  public static BufferedImage renderLatex(String input) {
    TeXFormula formula = new TeXFormula(input);
    TeXIcon icon = formula.createTeXIcon(TeXConstants.STYLE_DISPLAY, 20);
    
    icon.setInsets(new Insets(5,5,5,5));
    
    int width = icon.getIconWidth();
    int height = icon.getIconHeight();
    
    BufferedImage image = new BufferedImage(width, height, BufferedImage.TYPE_INT_ARGB);
    Graphics2D g = image.createGraphics();
    g.setColor(Color.white);
    g.fillRect(0, 0, width, height);
    
    JLabel lbl = new JLabel();
    lbl.setForeground(Color.black);
    icon.paintIcon(lbl, g, 0, 0);
    
    g.dispose();
    
    return image;
  }
}
