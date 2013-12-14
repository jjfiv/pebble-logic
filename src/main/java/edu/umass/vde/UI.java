package edu.umass.vde;

import java.awt.Color;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyAdapter;
import java.awt.event.KeyEvent;
import java.awt.image.BufferedImage;
import javax.swing.BoxLayout;
import javax.swing.JFrame;
import javax.swing.JPanel;
import javax.swing.JTextField;

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
  
  public UI(String title, CommandEvaluator ceval) {
    commandEvaluator = ceval;
    
    frame = new JFrame();
    frame.setTitle(title);
    frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);

    panel = new JPanel();
    panel.setLayout(new BoxLayout(panel, BoxLayout.Y_AXIS));

    canvasBuffer = new CanvasBuffer(panel);
    canvasBuffer.add();

    commandField = new MaxHeightTextField();
    commandField.addActionListener(this);
    commandField.addKeyListener(new KeyAdapter() {

      @Override
      public void keyPressed(KeyEvent ke) {              
        String now = null;
        if(ke.getKeyCode() == KeyEvent.VK_DOWN)
          now = canvasBuffer.next();
        else if(ke.getKeyCode() == KeyEvent.VK_UP)
          now = canvasBuffer.previous();
        
        if(now != null) {
          commandField.setText(now);
        }
      }
    });
    panel.add(commandField);


    frame.setContentPane(panel);
    frame.pack();
    frame.setVisible(true);

  }

  public static void main(String[] args) {
    run();
  }

  public static UI run() {
    UI ui = new UI("UI-Test", new CommandEvaluator() {
      @Override
      public void evaluate(UI ui, String cmd) {
        if (cmd.contains(":") && Character.isDigit(cmd.charAt(0))) {
          String before = cmd.substring(0, cmd.indexOf(":"));
          String after = cmd.substring(cmd.indexOf(":") + 1);
          try {
            int n = Math.min(CanvasBuffer.SIZE, Integer.parseInt(before));
            for (int i = 0; i < n; i++) {
              ui.canvasBuffer.append(new PaddedLabel(after), false);
            }
            ui.canvasBuffer.update();
            ui.canvasBuffer.scrollDown();
            ui.commandField.setText("");
          } catch (NumberFormatException nfe) {
            ui.showError(nfe.getMessage());
          }
          return;
        }
        if (cmd.startsWith("$")) {
          try {
            BufferedImage img = RenderMath.renderLatex(cmd.substring(1));
            ui.showImage(img);
            return;
          } catch (Exception e) {
            ui.showError("Could not render "+cmd.substring(1)+" as latex math!"+e.getMessage());
          }
        }

        ui.showText(cmd);
        ui.commandField.setText("");
      }
    });

    return ui;
  }

  public void showError(String msg) {
    canvasBuffer.append(new PaddedLabel(msg, Color.RED));
  }

  public void showText(String msg) {
    canvasBuffer.append(new PaddedLabel(msg));
  }

  public void showImage(BufferedImage img) {
    canvasBuffer.append(new PaddedLabel(img));
    canvasBuffer.scrollDown();
  }

  public void showBytesAsImage(byte[] data) {
    canvasBuffer.append(new PaddedLabel(data));
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
}
