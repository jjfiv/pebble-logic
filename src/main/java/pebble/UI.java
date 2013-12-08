package pebble;


import java.awt.Color;
import java.awt.Dimension;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import javax.swing.BorderFactory;
import javax.swing.BoxLayout;
import javax.swing.JComponent;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollBar;
import javax.swing.JScrollPane;
import javax.swing.JTextField;
import javax.swing.JViewport;
import javax.swing.SwingUtilities;

/**
 *
 * @author jfoley
 */
public class UI implements ActionListener {
  public CommandEvaluator commandEvaluator;
  public final JFrame frame;
  public final JPanel panel;
  public final CanvasBuffer canvasBuffer;
  
  public final JTextField commandField;
  
  public UI(CommandEvaluator ceval) {
    commandEvaluator = ceval;
    
    frame = new JFrame();
    frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
    
    panel = new JPanel();
    panel.setLayout(new BoxLayout(panel, BoxLayout.Y_AXIS));

    canvasBuffer = new CanvasBuffer(panel);
    canvasBuffer.add();
    
    commandField = new MaxHeightTextField();
    commandField.addActionListener(this);
    panel.add(commandField);

    
    frame.setContentPane(panel);
    frame.pack();
    frame.setVisible(true);

  }
  
  public static void main(String[] args) {
    run();
  }
  public static void run() {
    UI ui = new UI(new CommandEvaluator() {
      @Override
      public void evaluate(UI ui, String cmd) {
        if (cmd.contains(":") && Character.isDigit(cmd.charAt(0))) {
          String before = cmd.substring(0, cmd.indexOf(":"));
          String after = cmd.substring(cmd.indexOf(":") + 1);
          try {
            int n = Integer.parseInt(before);
            for (int i = 0; i < n; i++) {
              ui.canvasBuffer.append(new TextMessage(after), false);
            }
            ui.canvasBuffer.update();
            ui.canvasBuffer.scrollDown();
            ui.commandField.setText("");
          } catch (NumberFormatException nfe) {
            ui.showError(nfe.getMessage());
          }
        } else {

          ui.canvasBuffer.append(new TextMessage(cmd));
          ui.commandField.setText("");
        }
      }
    });
  }

  public void showError(String msg) {
    canvasBuffer.append(TextMessage.error(msg));
  }

  @Override
  public void actionPerformed(ActionEvent ae) {
    if (commandField.equals(ae.getSource())) {
      String cmd = commandField.getText();
      commandEvaluator.evaluate(this, cmd);
    } else {
      showError("unknown actionPerformed source");
    }
  }
  
  public static class CanvasBuffer {
    
    private final JPanel panel;
    private final JScrollPane scroller;
    private final JPanel parent;

    public CanvasBuffer(JPanel parent) {
      this.parent = parent;
      this.panel = new JPanel();
      this.scroller = new JScrollPane(panel);
      panel.setLayout(new BoxLayout(panel, BoxLayout.Y_AXIS));
      
      setMinSize(300,300);
    }
    
    public void add() {
      parent.add(scroller);
    }
    
    private void setMinSize(int x, int y) {
      Dimension size = new Dimension(x, y);
      JViewport viewport = scroller.getViewport();
      viewport.setMinimumSize(size);
      viewport.setPreferredSize(size);
    }
    
    public void append(JComponent component) {
      append(component, true);
    }
    public void append(JComponent component, boolean update) {
      panel.add(component);
      if(update) {
        update();
        scrollDown();
      }
    }
    public void scrollDown() {
      SwingUtilities.invokeLater(new Runnable() {
        @Override
        public void run() {
          JScrollBar vsb = scroller.getVerticalScrollBar();
          vsb.setValue(vsb.getMaximum());
        }
      });
    }
    public void update() {
      panel.revalidate();
    }
  }
  
  public static class TextMessage extends JLabel {
    public TextMessage(String text) {
      super(text);
      this.setBorder(BorderFactory.createEmptyBorder(5,5,0,5));
    }
    public TextMessage(String text, Color color) {
      this(text);
      this.setForeground(color);
    }
    
    public static TextMessage error(String msg) {
      return new TextMessage("Error: "+msg, Color.RED);
    }
  }
  
  public static class MaxHeightTextField extends JTextField {
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

  public static interface CommandEvaluator {
    public void evaluate(UI ui, String cmd);
  }
}
