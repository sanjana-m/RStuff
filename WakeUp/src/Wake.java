import java.awt.Color;
import java.awt.Font;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.net.*;
import java.io.*;
import javax.swing.*;  
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.ArrayList;
import java.util.List;

public class Wake implements ActionListener {  
	String url = "http://subfusion.net/cgi-bin/quote.pl?quote=cookie";
	StringBuffer html;
	
	JButton b;
	JFrame f;  
	JTextArea ta;
	JLabel t;
	public Wake(){  
		f=new JFrame();//creating instance of JFrame  
        
		b = new JButton("New Quote");//creating instance of JButton  
		ta = new JTextArea(350,200);
		ta.setBounds(25, 200, 350, 200);
		ta.setEditable(false);
		ta.setLineWrap(true);
		ta.setBackground(new Color(0xD7A1D0));
		
		Font font = new Font("Arial", Font.PLAIN, 30);
		t = new JLabel("You Are Awake!");
		t.setFont(font);
		t.setBounds(100, 10, 300, 40);
		
		b.setBorderPainted(false);
		b.setFocusPainted(false);
		b.setRolloverEnabled(false);
		b.setBounds(150,100,100,50);
        b.addActionListener(this);  
		
        f.add(t);
		f.add(b);//adding button in JFrame  
		f.add(ta);
		
		f.setSize(400,500);//400 width and 500 height
		f.getContentPane().setBackground(new Color(0xD7B1EC));
		f.setLayout(null);//using no layout managers  
		f.setVisible(true);//making the frame visible  
	}  
  
	public void actionPerformed(ActionEvent e) {  
		InputStream is = null;
	    BufferedReader br;
	    html = new StringBuffer();
	    String line;
		try{
			URL url1 = new URL(url);
			is = url1.openStream();  // throws an IOException
	        br = new BufferedReader(new InputStreamReader(is));

	        while ((line = br.readLine()) != null) {
	            html.append(line);
	        }
	        String str = html.toString();
	        Pattern pattern = Pattern.compile("<br>(.*?)(?<=<)");
	        Matcher matcher = pattern.matcher(str);

	        List<String> listMatches = new ArrayList<String>();

	        while(matcher.find()) {
	            listMatches.add(matcher.group(1));
	        }
	        String x = (String) listMatches.get(1);
	        System.out.println(x);
	        x = x.replace("<", "");
	        x = x.replace("\\s", " ");
	        x = x.replace("-","\n\t");
	        ta.setText(x);

	        PrintWriter writer = new PrintWriter("quotes.txt", "UTF-8");
	        writer.println(x.replace("\n","*"));
	        writer.close();
	    } 
		catch (java.net.UnknownHostException err){
			FileReader fr = null;
			try {
				fr = new FileReader("quotes.txt");
			} catch (FileNotFoundException e1) {
				e1.printStackTrace();
			}
	        BufferedReader reader = new BufferedReader(fr);
	        try {
				String line1 = reader.readLine();
				ta.setText(line1.replace("*","\n"));
			} catch (IOException e1) {
				e1.printStackTrace();
			}
		}
		catch (MalformedURLException mue) {
	         mue.printStackTrace();
	    } 
	    catch (IOException ioe) {
	         ioe.printStackTrace();
	    }
	    finally {
	        try {
	            if (is != null) is.close();
	        } 
	        catch (IOException ioe) {
	        	ioe.printStackTrace();
	        }
	    }
	}
	
	public static void main(String[] args) {  
		new Wake();  
	}  
}